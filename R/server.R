#' Find a free TCP port on the loopback interface
#'
#' Base R has no direct way to ask the OS for an ephemeral port and read
#' back what it picked, so instead this samples candidate ports in the
#' dynamic/private range and confirms one is bindable. There is an
#' unavoidable, small TOCTOU race between this check and the child process
#' actually binding the port; acceptable for a local, single-user dev tool.
#' @noRd
.find_free_port <- function(tries = 30L) {
  for (i in seq_len(tries)) {
    port <- sample(49152:65535, 1L)
    ok <- tryCatch(
      {
        con <- serverSocket(port)
        close(con)
        TRUE
      },
      error = function(e) FALSE
    )
    if (ok) {
      return(port)
    }
  }
  stop("gitneighbr: could not find a free port after ", tries, " attempts.", call. = FALSE)
}

#' Acquire the single-mutation lock for one session (spec Sec 12.5)
#'
#' At most one repository-mutating operation runs at a time per session.
#' Returns the fresh operation ID -- also set as the `X-Operation-Id`
#' response header, so it's available both in logs and in the response, as
#' spec Sec 12.5 requires -- when the lock was free, or `NULL` when a
#' competing mutation is already in progress; the caller must then answer
#' `423 OPERATION_IN_PROGRESS` without running anything. Always pair a
#' non-`NULL` acquisition with `on.exit(.release_mutation_lock(session_state),
#' add = TRUE)` so the lock is freed even if the operation errors.
#'
#' @param session_state The per-session mutable environment `.build_api()`
#'   holds (`version`, `mutation_lock`, etc.).
#' @param response The plumber2/reqres response object for this request.
#' @noRd
.acquire_mutation_lock <- function(session_state, response) {
  if (isTRUE(session_state$mutation_lock)) {
    return(NULL)
  }
  session_state$mutation_lock <- TRUE
  operation_id <- .new_operation_id()
  response$set_header("X-Operation-Id", operation_id)
  operation_id
}

#' Release the single-mutation lock acquired by `.acquire_mutation_lock()`
#' @noRd
.release_mutation_lock <- function(session_state) {
  session_state$mutation_lock <- FALSE
}

#' Build the plumber2 app for a single repository session
#'
#' @param repo_root Canonical path to the Git working tree root.
#' @param git_bin Path to the `git` executable.
#' @param token Bearer token required on every `/api/v1/*` request.
#' @param port The port this session is bound to, for `Host`/`Origin`
#'   validation (spec Sec 12.1/12.2) -- the actual bind happens later, in
#'   `plumber2::api_run()`, so this must match what's passed there.
#' @param www_dir Directory containing the precompiled frontend.
#' @param session_state Optional pre-built session-state environment (same
#'   shape this function otherwise constructs itself). Exists so tests can
#'   seed state -- e.g. `mutation_lock <- TRUE` to exercise the
#'   `OPERATION_IN_PROGRESS` branch of every mutating endpoint without a real
#'   race -- before the app is built; production callers always omit it.
#' @param base_path The URL path a reverse proxy serves the app under, such as
#'   `"/apps/gitneighbr/"`. The app always answers at `/` too, so it works
#'   behind proxies that strip the prefix and ones that forward the full path.
#' @param managed `NULL` for a standalone session, or a launcher configuration
#'   from `.managed_config()`: the token then arrives in the
#'   `X-Gitneighbr-Token` header, only the configured features are served,
#'   and a shutdown route exists when a shutdown token was given.
#' @noRd
.build_api <- function(repo_root, git_bin, token, port, www_dir = system.file("www", package = "gitneighbr"),
                        session_state = NULL, base_path = "/", managed = NULL) {
  # plumber2's default JSON serializer does not auto-unbox length-1
  # vectors (so `list(ok = TRUE)` would serialize as `{"ok":[true]}`),
  # which is surprising for API clients. `register_serializer()` expects
  # a *generator* function (one that takes config and returns the actual
  # `function(x) ...` serializer) - re-registering `reqres::format_json`
  # with `auto_unbox` pre-bound to TRUE restores the behaviour
  # plumber1/most JSON APIs use, without reimplementing the serializer.
  # `null = "null"` is likewise pre-bound: the default ("list") serializes
  # an R `NULL` as `{}`, but the spec's envelope shape (`"error": null`,
  # optional fields like `old_path` absent) requires JSON `null`.
  plumber2::register_serializer(
    "json",
    function(...) reqres::format_json(..., auto_unbox = TRUE, null = "null"),
    mime_type = "application/json",
    default = TRUE
  )

  # Mutable state private to this one running server process, for the
  # optimistic-concurrency contract in spec Sec 7.3: a monotonically
  # increasing `status_version` (bumped only when computed status actually
  # changes; see `.status_payload()`), the sticky `AUTH_REQUIRED` flag
  # (spec Sec 7.1) set by a failed fetch/push and cleared by the next
  # successful one, and the tag bookkeeping `.status_notices()` uses for
  # the "local-only tag" / "pushed tag" notices.
  if (is.null(session_state)) {
    session_state <- new.env(parent = emptyenv())
    session_state$version <- 0L
    session_state$last_snapshot <- NULL
    session_state$auth_required <- FALSE
    session_state$pending_tags <- character()
    session_state$pushed_tags <- character()
    session_state$mutation_lock <- FALSE
    session_state$github_token <- NULL
  }

  # DNS-rebinding mitigation (spec Sec 12.1): a request whose `Host` header
  # doesn't name this exact loopback authority is rejected outright, before
  # authentication -- an attacker's rebound hostname can carry a stolen
  # token but can never legitimately claim to *be* "127.0.0.1:<port>".
  validate_host <- function(request) {
    if (!.valid_host_header(request$get_header("Host"), port)) {
      plumber2::abort_bad_request("Invalid Host header.")
    }
  }

  # A standalone page sends `Authorization: Bearer` (token from the URL
  # fragment). A managed session's token is injected by the launcher's
  # proxy in its own header instead, so the page never holds it and the
  # proxy never has to touch a user's `Authorization`.
  require_auth <- function(request) {
    if (is.null(managed)) {
      supplied <- request$get_header("Authorization")
      supplied <- sub("^Bearer\\s+", "", supplied %||% "")
    } else {
      supplied <- request$get_header(.MANAGED_TOKEN_HEADER) %||% ""
    }
    if (!.tokens_match(supplied, token)) {
      plumber2::abort_unauthorized("Missing or invalid session token.")
    }
  }

  # A browser marks every request with `Sec-Fetch-Site`; anything a page on
  # another site made it send (a form post, an image, a fetch) is refused
  # before authentication. Non-browser callers send no such header.
  validate_fetch_site <- function(request) {
    site <- request$get_header("Sec-Fetch-Site")
    if (!is.null(site) && !site %in% c("same-origin", "none")) {
      plumber2::abort_forbidden("Cross-site request refused.")
    }
  }

  features <- if (is.null(managed)) .STANDALONE_FEATURES else managed$features
  session_state$features <- features
  session_state$managed <- !is.null(managed)
  # A launcher's policy (groups, protected paths, review labels, ignore
  # template). It comes from outside the repository; NULL without one.
  launcher_policy <- managed$policy
  session_state$launcher_policy <- launcher_policy
  max_diff_bytes <- launcher_policy$max_diff_bytes %||% getOption("gitneighbr.diff_max_bytes", 200000L)

  # Working-tree-changing actions never touch a protected path.
  refuse_protected <- function(paths, response) {
    hit <- .protected_path(paths, launcher_policy)
    if (is.null(hit)) {
      return(NULL)
    }
    response$status <- 403L
    .error_envelope("PATH_PROTECTED", paste0("'", hit, "' is protected here and can't be changed by version history."), recoverable = FALSE)
  }

  # Every route goes through this: Host, cross-site and token checks run
  # before the handler (which repeats the Host and token checks itself; that
  # is harmless), and a route whose feature is off is refused untouched.
  guard <- function(handler, public = FALSE, feature = NULL) {
    wanted <- intersect(c("request", "response"), names(formals(handler)))
    function(request, response) {
      validate_host(request)
      validate_fetch_site(request)
      if (!public) {
        require_auth(request)
      }
      if (!is.null(feature) && !all(feature %in% features)) {
        response$status <- 403L
        return(.error_envelope(
          "FEATURE_DISABLED", "That action isn't available here.",
          recoverable = FALSE
        ))
      }
      do.call(handler, list(request = request, response = response)[wanted])
    }
  }

  # spec Sec 12.2: only checked on mutating requests, and only when a
  # browser actually sent one -- a non-browser API caller (curl, an R
  # script) has no `Origin` to send and is not rejected for its absence.
  validate_origin <- function(request) {
    if (!.valid_origin_header(request$get_header("Origin"), port)) {
      plumber2::abort_forbidden("Invalid Origin header.")
    }
  }

  acquire_mutation_lock <- function(response) .acquire_mutation_lock(session_state, response)
  release_mutation_lock <- function() .release_mutation_lock(session_state)

  # `repo_root` may not be a Git repository yet (onboarding: init/clone
  # haven't run). Every endpoint except health, status, init, and clone
  # needs a real repository to operate on; this returns an error envelope
  # for those, or `NULL` when the request may proceed.
  require_existing_repo <- function() {
    if (!is.null(managed)) {
      check <- .managed_repo_check(repo_root, git_bin)
      if (identical(check$state, "worktree")) {
        return(NULL)
      }
      return(.error_envelope(check$code, check$message, recoverable = FALSE, data = list(detail = check$detail)))
    }
    if (!identical(.git_repo_kind(repo_root, git_bin), "worktree")) {
      return(.error_envelope(
        "NOT_REPOSITORY", "This folder isn't a Git project yet.",
        recoverable = TRUE
      ))
    }
    NULL
  }

  # `request$parse()` requires an explicit named parser list on every call
  # (it has no implicit default), so a malformed/absent JSON body is caught
  # here and turned into `NULL` for callers to map onto the same envelope
  # shape every other endpoint uses, instead of plumber2's plain-text 400.
  parse_body <- function(request) {
    tryCatch(
      {
        do.call(request$parse, plumber2::get_parsers())
        request$body %||% list()
      },
      error = function(e) NULL
    )
  }

  # Gate for every mutating endpoint (spec Sec 7.3): the client must name
  # the `status_version` it last observed. A missing or non-current value
  # is refused as a `409 STATE_CHANGED` carrying fresh status data, so the
  # frontend can update its display without a second round trip. Returns
  # `NULL` when the request may proceed.
  require_fresh <- function(body, response) {
    current <- .status_payload(repo_root, git_bin, session_state)
    client_version <- suppressWarnings(as.integer(body$status_version %||% NA))
    if (is.na(client_version) || client_version != current$version) {
      response$status <- 409L
      return(.error_envelope(
        "STATE_CHANGED",
        "The repository changed since this was shown. Refresh and try again.",
        recoverable = TRUE,
        status_version = current$version,
        data = current$data
      ))
    }
    NULL
  }

  # Records whether a fetch/push result implies the sticky AUTH_REQUIRED
  # flag should change; a failure for an unrelated reason (e.g. the
  # network being unreachable) leaves the previous flag untouched, since
  # it says nothing new about authentication either way.
  note_auth_result <- function(result) {
    if (isTRUE(result$ok)) {
      session_state$auth_required <- FALSE
    } else if (identical(result$code, "AUTH_REQUIRED")) {
      session_state$auth_required <- TRUE
    }
  }

  base_path <- .normalize_base_path(base_path)
  prefixes <- unique(c("/", base_path))
  www_dir <- .prepare_www(www_dir, base_path)

  # Every route is registered under each prefix: "/" for direct use and
  # prefix-stripping proxies, and `base_path` for proxies that forward the
  # full path. Paths here are relative ("api/v1/...").
  route_get <- function(api, path, handler, public = FALSE) {
    handler <- guard(handler, public = public, feature = if (!public) .route_features(path))
    for (prefix in prefixes) {
      api <- plumber2::api_get(api, paste0(prefix, path), handler)
    }
    api
  }
  route_post <- function(api, path, handler, feature = .route_features(path)) {
    handler <- guard(handler, feature = feature)
    for (prefix in prefixes) {
      api <- plumber2::api_post(api, paste0(prefix, path), handler)
    }
    api
  }
  # Send and Update with a full, preview-bound history check (see
  # R/managed-remote.R). Each mutation is gated like every other one: an
  # existing supported repository, the client's current status_version, and
  # the single mutation lock.
  remote_mutation <- function(request, response, action, needs_fresh = TRUE) {
    if (!.git_available(git_bin)) {
      return(.error_envelope("GIT_UNAVAILABLE", "Git isn't available on this computer.", recoverable = FALSE))
    }
    not_a_repo <- require_existing_repo()
    if (!is.null(not_a_repo)) {
      return(not_a_repo)
    }
    body <- parse_body(request)
    if (is.null(body)) {
      return(.error_envelope("COMMAND_FAILED", "The request could not be understood.", recoverable = FALSE))
    }
    if (needs_fresh) {
      stale <- require_fresh(body, response)
      if (!is.null(stale)) {
        return(stale)
      }
    }
    operation_id <- acquire_mutation_lock(response)
    if (is.null(operation_id)) {
      response$status <- 423L
      return(.error_envelope("OPERATION_IN_PROGRESS", "Another repository action is already running. Wait for it to finish and try again."))
    }
    on.exit(release_mutation_lock(), add = TRUE)
    result <- action(body)
    note_auth_result(result)
    payload <- .status_payload(repo_root, git_bin, session_state)
    if (!isTRUE(result$ok)) {
      return(.error_envelope(
        result$code, result$message,
        recoverable = result$recoverable %||% TRUE, status_version = payload$version,
        advanced = result$advanced
      ))
    }
    .ok_envelope(result[setdiff(names(result), "ok")], status_version = payload$version)
  }
  remote_read <- function(action) {
    if (!.git_available(git_bin)) {
      return(.error_envelope("GIT_UNAVAILABLE", "Git isn't available on this computer.", recoverable = FALSE))
    }
    not_a_repo <- require_existing_repo()
    if (!is.null(not_a_repo)) {
      return(not_a_repo)
    }
    result <- action()
    if (!isTRUE(result$ok)) {
      return(.error_envelope(result$code, result$message, recoverable = result$recoverable %||% TRUE))
    }
    .ok_envelope(result[setdiff(names(result), "ok")])
  }
  remote_routes <- function(api) {
    api |>
      route_post("api/v1/remote/check", function(request, response) {
        remote_mutation(request, response, function(body) .managed_remote_check(repo_root, git_bin, session_state))
      }) |>
      route_post("api/v1/remote/connect", function(request, response) {
        remote_mutation(request, response, function(body) .managed_remote_connect(repo_root, git_bin, body$url))
      }) |>
      route_post("api/v1/remote/token", function(request, response) {
        # Held in memory for this session only; never echoed back. An empty
        # token forgets it.
        body <- parse_body(request)
        token_value <- as.character(body$token %||% "")[[1]]
        if (!nzchar(token_value)) {
          session_state$remote_auth <- NULL
          return(.ok_envelope(list(has_token = FALSE)))
        }
        dest <- .managed_destination(repo_root, git_bin)
        if (!isTRUE(dest$ok)) {
          return(.error_envelope(dest$code, dest$message, recoverable = TRUE))
        }
        username <- as.character(body$username %||% "x-access-token")[[1]]
        if (grepl("[:\\s]", username, perl = TRUE) || grepl("[\\s]", token_value, perl = TRUE) || nchar(token_value) > 4096L) {
          return(.error_envelope("INVALID_TOKEN", "That doesn't look like an access token.", recoverable = TRUE))
        }
        session_state$remote_auth <- list(host = dest$host, username = if (nzchar(username)) username else "x-access-token", token = token_value)
        session_state$auth_required <- FALSE
        .ok_envelope(list(has_token = TRUE, host = dest$host))
      }) |>
      route_get("api/v1/send/preview", function(request) {
        remote_read(function() .managed_send_preview(repo_root, git_bin, session_state))
      }) |>
      route_post("api/v1/send", function(request, response) {
        remote_mutation(request, response, function(body) .managed_send(repo_root, git_bin, session_state, body))
      }) |>
      route_get("api/v1/receive/preview", function(request) {
        remote_read(function() .managed_receive_preview(repo_root, git_bin, session_state))
      }) |>
      route_post("api/v1/receive", function(request, response) {
        remote_mutation(request, response, function(body) .managed_receive(repo_root, git_bin, session_state, body))
      })
  }

  # The shutdown route exists only for a managed session started with a
  # shutdown token. It needs that second token as well as the session one:
  # the proxy injects the session token into every request a page makes,
  # so the session token alone must not be enough to stop the server.
  shutdown_route <- function(api) {
    if (is.null(managed) || is.null(managed$shutdown_token)) {
      return(api)
    }
    route_post(api, "api/v1/shutdown", feature = NULL, handler = function(request, response) {
      supplied <- request$get_header(.MANAGED_SHUTDOWN_HEADER) %||% ""
      if (!.tokens_match(supplied, managed$shutdown_token)) {
        plumber2::abort_forbidden("Missing or invalid shutdown token.")
      }
      managed$control$stop_requested <- TRUE
      list(ok = TRUE, data = list(stopping = TRUE), error = NULL)
    })
  }
  # `httpuv`'s static-file layer intercepts every request under `at`
  # (prefix-matched, so "/" means *every* path) before the router ever
  # sees it, and only knows how to serve GET/HEAD - any other method
  # anywhere on the site, including a POST to "/api/v1/...", gets a flat
  # 400 with `fallthrough` having no effect (fallthrough only covers a
  # missing *file* on GET). `except` tells httpuv to treat that prefix as
  # not-static at all, handing it to the router (and its own methods)
  # unmodified. The most specific `at` wins, so each prefix gets its own
  # mount with its own `except`.
  statics <- function(api) {
    for (prefix in prefixes) {
      api <- plumber2::api_statics(api, at = prefix, path = www_dir, fallthrough = TRUE, except = "api")
    }
    api
  }

  plumber2::api(host = "127.0.0.1") |>
    statics() |>
    shutdown_route() |>
    remote_routes() |>
    route_get("api/v1/health", function(request) {
      validate_host(request)
      list(ok = TRUE, data = list(
        status = "ok",
        version = as.character(utils::packageVersion("gitneighbr")),
        busy = isTRUE(session_state$mutation_lock),
        managed = !is.null(managed)
      ), error = NULL)
    }, public = TRUE) |>
    route_get("api/v1/status", function(request) {
      validate_host(request)
      require_auth(request)

      show_ignored <- identical(request$query$show_ignored, "true")
      payload <- .status_payload(repo_root, git_bin, session_state, show_ignored = show_ignored)
      .ok_envelope(payload$data, status_version = payload$version)
    }) |>
    route_get("api/v1/changes", function(request) {
      validate_host(request)
      require_auth(request)

      if (!.git_available(git_bin)) {
        return(.error_envelope("GIT_UNAVAILABLE", "Git isn't available on this computer.", recoverable = FALSE))
      }
      not_a_repo <- require_existing_repo()
      if (!is.null(not_a_repo)) {
        return(not_a_repo)
      }
      changes <- .git_changes(repo_root, git_bin, max_bytes = max_diff_bytes)
      .ok_envelope(list(changes = .annotate_changes(changes, launcher_policy)))
    }) |>
    route_get("api/v1/diff", function(request) {
      validate_host(request)
      require_auth(request)

      if (!.git_available(git_bin)) {
        return(.error_envelope("GIT_UNAVAILABLE", "Git isn't available on this computer.", recoverable = FALSE))
      }
      not_a_repo <- require_existing_repo()
      if (!is.null(not_a_repo)) {
        return(not_a_repo)
      }

      path <- request$query$path %||% ""
      if (!.validate_repo_relative_path(repo_root, path)) {
        return(.error_envelope(
          "PATH_OUTSIDE_REPOSITORY",
          "That path is not inside this repository.",
          recoverable = FALSE
        ))
      }

      offset_lines <- suppressWarnings(as.integer(request$query$offset_lines %||% "0"))
      if (is.na(offset_lines) || offset_lines < 0L) {
        offset_lines <- 0L
      }

      result <- .git_diff(repo_root, git_bin, path, offset_lines = offset_lines, max_bytes = max_diff_bytes)
      if (!result$found) {
        return(.error_envelope("COMMAND_FAILED", "That path has no pending changes to show."))
      }
      .ok_envelope(result[names(result) != "found"])
    }) |>
    route_get("api/v1/identity", function(request) {
      validate_host(request)
      require_auth(request)

      if (!.git_available(git_bin)) {
        return(.error_envelope("GIT_UNAVAILABLE", "Git isn't available on this computer.", recoverable = FALSE))
      }
      not_a_repo <- require_existing_repo()
      if (!is.null(not_a_repo)) {
        return(not_a_repo)
      }
      .ok_envelope(.git_identity(repo_root, git_bin))
    }) |>
    route_get("api/v1/credential-diagnosis", function(request) {
      validate_host(request)
      require_auth(request)

      if (!.git_available(git_bin)) {
        return(.error_envelope("GIT_UNAVAILABLE", "Git isn't available on this computer.", recoverable = FALSE))
      }
      not_a_repo <- require_existing_repo()
      if (!is.null(not_a_repo)) {
        return(not_a_repo)
      }
      .ok_envelope(.git_credential_diagnosis(repo_root, git_bin))
    }) |>
    route_get("api/v1/diagnostic-report", function(request) {
      validate_host(request)
      require_auth(request)

      if (!.git_available(git_bin)) {
        return(.error_envelope("GIT_UNAVAILABLE", "Git isn't available on this computer.", recoverable = FALSE))
      }
      not_a_repo <- require_existing_repo()
      if (!is.null(not_a_repo)) {
        return(not_a_repo)
      }
      .ok_envelope(list(report = .diagnostic_report(repo_root, git_bin, session_state)))
    }) |>
    route_post("api/v1/identity", function(request, response) {
      validate_host(request)
      require_auth(request)
      validate_origin(request)

      if (!.git_available(git_bin)) {
        return(.error_envelope("GIT_UNAVAILABLE", "Git isn't available on this computer.", recoverable = FALSE))
      }
      not_a_repo <- require_existing_repo()
      if (!is.null(not_a_repo)) {
        return(not_a_repo)
      }
      body <- parse_body(request)
      if (is.null(body)) {
        return(.error_envelope("COMMAND_FAILED", "The request could not be understood.", recoverable = FALSE))
      }
      stale <- require_fresh(body, response)
      if (!is.null(stale)) {
        return(stale)
      }

      operation_id <- acquire_mutation_lock(response)
      if (is.null(operation_id)) {
        response$status <- 423L
        return(.error_envelope(
          "OPERATION_IN_PROGRESS",
          "Another repository action is already running. Wait for it to finish and try again."
        ))
      }
      on.exit(release_mutation_lock(), add = TRUE)

      result <- .git_set_identity(repo_root, git_bin, name = body$name, email = body$email, scope = body$scope)
      payload <- .status_payload(repo_root, git_bin, session_state)
      if (!isTRUE(result$ok)) {
        return(.error_envelope(
          result$code, result$message,
          recoverable = result$recoverable %||% TRUE, status_version = payload$version,
          advanced = result$advanced
        ))
      }
      .ok_envelope(list(name = result$name, email = result$email, scope = result$scope), status_version = payload$version)
    }) |>
    route_post("api/v1/commit", function(request, response) {
      validate_host(request)
      require_auth(request)
      validate_origin(request)

      if (!.git_available(git_bin)) {
        return(.error_envelope("GIT_UNAVAILABLE", "Git isn't available on this computer.", recoverable = FALSE))
      }
      not_a_repo <- require_existing_repo()
      if (!is.null(not_a_repo)) {
        return(not_a_repo)
      }
      body <- parse_body(request)
      if (is.null(body)) {
        return(.error_envelope("COMMAND_FAILED", "The request could not be understood.", recoverable = FALSE))
      }
      stale <- require_fresh(body, response)
      if (!is.null(stale)) {
        return(stale)
      }

      operation_id <- acquire_mutation_lock(response)
      if (is.null(operation_id)) {
        response$status <- 423L
        return(.error_envelope(
          "OPERATION_IN_PROGRESS",
          "Another repository action is already running. Wait for it to finish and try again."
        ))
      }
      on.exit(release_mutation_lock(), add = TRUE)

      # `require_fresh()` just confirmed the client saw the current status,
      # whose `head` is the commit this snapshot must go on top of.
      result <- .git_commit_selected(
        repo_root, git_bin,
        expected_head = session_state$last_snapshot$head,
        selected_paths = body$paths,
        summary = body$summary,
        details = body$details
      )
      payload <- .status_payload(repo_root, git_bin, session_state)
      if (!isTRUE(result$ok)) {
        return(.error_envelope(
          result$code, result$message,
          recoverable = result$recoverable %||% TRUE, status_version = payload$version,
          advanced = result$advanced
        ))
      }
      .ok_envelope(list(sha = result$sha, summary = result$summary), status_version = payload$version)
    }) |>
    route_post("api/v1/refresh-remote", function(request, response) {
      validate_host(request)
      require_auth(request)
      validate_origin(request)

      if (!.git_available(git_bin)) {
        return(.error_envelope("GIT_UNAVAILABLE", "Git isn't available on this computer.", recoverable = FALSE))
      }
      not_a_repo <- require_existing_repo()
      if (!is.null(not_a_repo)) {
        return(not_a_repo)
      }
      body <- parse_body(request)
      if (is.null(body)) {
        return(.error_envelope("COMMAND_FAILED", "The request could not be understood.", recoverable = FALSE))
      }
      stale <- require_fresh(body, response)
      if (!is.null(stale)) {
        return(stale)
      }

      operation_id <- acquire_mutation_lock(response)
      if (is.null(operation_id)) {
        response$status <- 423L
        return(.error_envelope(
          "OPERATION_IN_PROGRESS",
          "Another repository action is already running. Wait for it to finish and try again."
        ))
      }
      on.exit(release_mutation_lock(), add = TRUE)

      result <- .git_refresh_remote(repo_root, git_bin)
      note_auth_result(result)
      payload <- .status_payload(repo_root, git_bin, session_state)
      if (!isTRUE(result$ok)) {
        return(.error_envelope(
          result$code, result$message,
          recoverable = result$recoverable %||% TRUE, status_version = payload$version,
          advanced = result$advanced, diagnosis = result$diagnosis
        ))
      }
      .ok_envelope(list(
        primary_state = payload$data$primary_state,
        upstream = result$upstream,
        branch = result$branch,
        ahead = result$ahead %||% 0L,
        behind = result$behind %||% 0L,
        staged_count = result$staged_count %||% 0L,
        unstaged_count = result$unstaged_count %||% 0L,
        untracked_count = result$untracked_count %||% 0L
      ), status_version = payload$version)
    }) |>
    route_post("api/v1/push", function(request, response) {
      validate_host(request)
      require_auth(request)
      validate_origin(request)

      if (!.git_available(git_bin)) {
        return(.error_envelope("GIT_UNAVAILABLE", "Git isn't available on this computer.", recoverable = FALSE))
      }
      not_a_repo <- require_existing_repo()
      if (!is.null(not_a_repo)) {
        return(not_a_repo)
      }
      body <- parse_body(request)
      if (is.null(body)) {
        return(.error_envelope("COMMAND_FAILED", "The request could not be understood.", recoverable = FALSE))
      }
      stale <- require_fresh(body, response)
      if (!is.null(stale)) {
        return(stale)
      }

      operation_id <- acquire_mutation_lock(response)
      if (is.null(operation_id)) {
        response$status <- 423L
        return(.error_envelope(
          "OPERATION_IN_PROGRESS",
          "Another repository action is already running. Wait for it to finish and try again."
        ))
      }
      on.exit(release_mutation_lock(), add = TRUE)

      # The repository policy is enforced here, not only by disabled buttons.
      capabilities <- .status_payload(repo_root, git_bin, session_state)$data$capabilities
      if (isTRUE(capabilities$requires_pull_request)) {
        response$status <- 403L
        return(.error_envelope(
          "PULL_REQUEST_REQUIRED",
          "This branch only accepts changes through a pull request.",
          recoverable = FALSE
        ))
      }
      result <- .git_push_current_branch(repo_root, git_bin)
      note_auth_result(result)
      payload <- .status_payload(repo_root, git_bin, session_state)
      if (!isTRUE(result$ok)) {
        return(.error_envelope(
          result$code, result$message,
          recoverable = result$recoverable %||% TRUE, status_version = payload$version,
          advanced = result$advanced, diagnosis = result$diagnosis
        ))
      }
      .ok_envelope(list(
        remote = result$remote,
        remote_branch = result$remote_branch,
        branch = result$branch,
        sha = result$sha,
        pushed_count = result$pushed_count
      ), status_version = payload$version)
    }) |>
    route_post("api/v1/update", function(request, response) {
      validate_host(request)
      require_auth(request)
      validate_origin(request)

      if (!.git_available(git_bin)) {
        return(.error_envelope("GIT_UNAVAILABLE", "Git isn't available on this computer.", recoverable = FALSE))
      }
      not_a_repo <- require_existing_repo()
      if (!is.null(not_a_repo)) {
        return(not_a_repo)
      }
      body <- parse_body(request)
      if (is.null(body)) {
        return(.error_envelope("COMMAND_FAILED", "The request could not be understood.", recoverable = FALSE))
      }
      stale <- require_fresh(body, response)
      if (!is.null(stale)) {
        return(stale)
      }

      operation_id <- acquire_mutation_lock(response)
      if (is.null(operation_id)) {
        response$status <- 423L
        return(.error_envelope(
          "OPERATION_IN_PROGRESS",
          "Another repository action is already running. Wait for it to finish and try again."
        ))
      }
      on.exit(release_mutation_lock(), add = TRUE)

      result <- .git_update_current_branch(repo_root, git_bin)
      note_auth_result(result)
      payload <- .status_payload(repo_root, git_bin, session_state)
      if (!isTRUE(result$ok)) {
        return(.error_envelope(
          result$code, result$message,
          recoverable = result$recoverable %||% TRUE, status_version = payload$version,
          advanced = result$advanced, diagnosis = result$diagnosis
        ))
      }
      .ok_envelope(list(
        remote = result$remote,
        remote_branch = result$remote_branch,
        branch = result$branch,
        sha = result$sha,
        updated_count = result$updated_count
      ), status_version = payload$version)
    }) |>
    route_post("api/v1/tag", function(request, response) {
      validate_host(request)
      require_auth(request)
      validate_origin(request)

      if (!.git_available(git_bin)) {
        return(.error_envelope("GIT_UNAVAILABLE", "Git isn't available on this computer.", recoverable = FALSE))
      }
      not_a_repo <- require_existing_repo()
      if (!is.null(not_a_repo)) {
        return(not_a_repo)
      }
      body <- parse_body(request)
      if (is.null(body)) {
        return(.error_envelope("COMMAND_FAILED", "The request could not be understood.", recoverable = FALSE))
      }
      stale <- require_fresh(body, response)
      if (!is.null(stale)) {
        return(stale)
      }

      operation_id <- acquire_mutation_lock(response)
      if (is.null(operation_id)) {
        response$status <- 423L
        return(.error_envelope(
          "OPERATION_IN_PROGRESS",
          "Another repository action is already running. Wait for it to finish and try again."
        ))
      }
      on.exit(release_mutation_lock(), add = TRUE)

      result <- .git_create_tag(repo_root, git_bin, name = body$name, annotation = body$annotation)
      if (isTRUE(result$ok)) {
        session_state$pending_tags <- union(session_state$pending_tags, result$name)
      }
      payload <- .status_payload(repo_root, git_bin, session_state)
      if (!isTRUE(result$ok)) {
        return(.error_envelope(
          result$code, result$message,
          recoverable = result$recoverable %||% TRUE, status_version = payload$version,
          advanced = result$advanced
        ))
      }
      .ok_envelope(list(name = result$name, sha = result$sha), status_version = payload$version)
    }) |>
    route_post("api/v1/push-tag", function(request, response) {
      validate_host(request)
      require_auth(request)
      validate_origin(request)

      if (!.git_available(git_bin)) {
        return(.error_envelope("GIT_UNAVAILABLE", "Git isn't available on this computer.", recoverable = FALSE))
      }
      not_a_repo <- require_existing_repo()
      if (!is.null(not_a_repo)) {
        return(not_a_repo)
      }
      body <- parse_body(request)
      if (is.null(body)) {
        return(.error_envelope("COMMAND_FAILED", "The request could not be understood.", recoverable = FALSE))
      }
      stale <- require_fresh(body, response)
      if (!is.null(stale)) {
        return(stale)
      }

      operation_id <- acquire_mutation_lock(response)
      if (is.null(operation_id)) {
        response$status <- 423L
        return(.error_envelope(
          "OPERATION_IN_PROGRESS",
          "Another repository action is already running. Wait for it to finish and try again."
        ))
      }
      on.exit(release_mutation_lock(), add = TRUE)

      result <- .git_push_tag(repo_root, git_bin, name = body$name)
      note_auth_result(result)
      if (isTRUE(result$ok)) {
        session_state$pending_tags <- setdiff(session_state$pending_tags, result$name)
        session_state$pushed_tags <- union(session_state$pushed_tags, result$name)
      }
      payload <- .status_payload(repo_root, git_bin, session_state)
      if (!isTRUE(result$ok)) {
        return(.error_envelope(
          result$code, result$message,
          recoverable = result$recoverable %||% TRUE, status_version = payload$version,
          advanced = result$advanced
        ))
      }
      .ok_envelope(list(remote = result$remote, name = result$name), status_version = payload$version)
    }) |>
    route_post("api/v1/restore", function(request, response) {
      validate_host(request)
      require_auth(request)
      validate_origin(request)

      if (!.git_available(git_bin)) {
        return(.error_envelope("GIT_UNAVAILABLE", "Git isn't available on this computer.", recoverable = FALSE))
      }
      not_a_repo <- require_existing_repo()
      if (!is.null(not_a_repo)) {
        return(not_a_repo)
      }
      body <- parse_body(request)
      if (is.null(body)) {
        return(.error_envelope("COMMAND_FAILED", "The request could not be understood.", recoverable = FALSE))
      }
      stale <- require_fresh(body, response)
      if (!is.null(stale)) {
        return(stale)
      }

      operation_id <- acquire_mutation_lock(response)
      if (is.null(operation_id)) {
        response$status <- 423L
        return(.error_envelope(
          "OPERATION_IN_PROGRESS",
          "Another repository action is already running. Wait for it to finish and try again."
        ))
      }
      on.exit(release_mutation_lock(), add = TRUE)

      protected <- refuse_protected(body$path, response)
      if (!is.null(protected)) {
        return(protected)
      }
      result <- .git_restore_tracked_file(repo_root, git_bin, path = body$path)
      payload <- .status_payload(repo_root, git_bin, session_state)
      if (!isTRUE(result$ok)) {
        return(.error_envelope(
          result$code, result$message,
          recoverable = result$recoverable %||% TRUE, status_version = payload$version,
          advanced = result$advanced
        ))
      }
      .ok_envelope(list(path = result$path), status_version = payload$version)
    }) |>
    route_post("api/v1/trash", function(request, response) {
      validate_host(request)
      require_auth(request)
      validate_origin(request)

      if (!.git_available(git_bin)) {
        return(.error_envelope("GIT_UNAVAILABLE", "Git isn't available on this computer.", recoverable = FALSE))
      }
      not_a_repo <- require_existing_repo()
      if (!is.null(not_a_repo)) {
        return(not_a_repo)
      }
      body <- parse_body(request)
      if (is.null(body)) {
        return(.error_envelope("COMMAND_FAILED", "The request could not be understood.", recoverable = FALSE))
      }
      stale <- require_fresh(body, response)
      if (!is.null(stale)) {
        return(stale)
      }

      operation_id <- acquire_mutation_lock(response)
      if (is.null(operation_id)) {
        response$status <- 423L
        return(.error_envelope(
          "OPERATION_IN_PROGRESS",
          "Another repository action is already running. Wait for it to finish and try again."
        ))
      }
      on.exit(release_mutation_lock(), add = TRUE)

      if (isTRUE(.read_repo_policy(repo_root)$disallow_untracked_trash)) {
        response$status <- 403L
        return(.error_envelope(
          "TRASH_DISALLOWED",
          "This repository's policy doesn't allow removing files from here.",
          recoverable = FALSE
        ))
      }
      protected <- refuse_protected(body$path, response)
      if (!is.null(protected)) {
        return(protected)
      }
      result <- .git_trash_untracked_file(repo_root, git_bin, path = body$path)
      payload <- .status_payload(repo_root, git_bin, session_state)
      if (!isTRUE(result$ok)) {
        return(.error_envelope(
          result$code, result$message,
          recoverable = result$recoverable %||% TRUE, status_version = payload$version,
          advanced = result$advanced
        ))
      }
      .ok_envelope(list(path = result$path), status_version = payload$version)
    }) |>
    route_post("api/v1/ignore", function(request, response) {
      validate_host(request)
      require_auth(request)
      validate_origin(request)

      if (!.git_available(git_bin)) {
        return(.error_envelope("GIT_UNAVAILABLE", "Git isn't available on this computer.", recoverable = FALSE))
      }
      not_a_repo <- require_existing_repo()
      if (!is.null(not_a_repo)) {
        return(not_a_repo)
      }
      body <- parse_body(request)
      if (is.null(body)) {
        return(.error_envelope("COMMAND_FAILED", "The request could not be understood.", recoverable = FALSE))
      }
      stale <- require_fresh(body, response)
      if (!is.null(stale)) {
        return(stale)
      }

      operation_id <- acquire_mutation_lock(response)
      if (is.null(operation_id)) {
        response$status <- 423L
        return(.error_envelope(
          "OPERATION_IN_PROGRESS",
          "Another repository action is already running. Wait for it to finish and try again."
        ))
      }
      on.exit(release_mutation_lock(), add = TRUE)

      protected <- refuse_protected(".gitignore", response)
      if (!is.null(protected)) {
        return(protected)
      }
      result <- .git_ignore_path(repo_root, git_bin, path = body$path)
      payload <- .status_payload(repo_root, git_bin, session_state)
      if (!isTRUE(result$ok)) {
        return(.error_envelope(
          result$code, result$message,
          recoverable = result$recoverable %||% TRUE, status_version = payload$version,
          advanced = result$advanced
        ))
      }
      .ok_envelope(list(path = result$path, rule = result$rule, added = result$added), status_version = payload$version)
    }) |>
    route_post("api/v1/init", function(request, response) {
      validate_host(request)
      require_auth(request)
      validate_origin(request)

      if (!.git_available(git_bin)) {
        return(.error_envelope("GIT_UNAVAILABLE", "Git isn't available on this computer.", recoverable = FALSE))
      }
      body <- parse_body(request)
      if (is.null(body)) {
        return(.error_envelope("COMMAND_FAILED", "The request could not be understood.", recoverable = FALSE))
      }
      stale <- require_fresh(body, response)
      if (!is.null(stale)) {
        return(stale)
      }

      operation_id <- acquire_mutation_lock(response)
      if (is.null(operation_id)) {
        response$status <- 423L
        return(.error_envelope(
          "OPERATION_IN_PROGRESS",
          "Another repository action is already running. Wait for it to finish and try again."
        ))
      }
      on.exit(release_mutation_lock(), add = TRUE)

      # A managed session initializes only its own folder, and only when
      # that folder is in no repository at all (not one above it).
      if (!is.null(managed)) {
        check <- .managed_repo_check(repo_root, git_bin)
        if (!identical(check$state, "not_a_repository")) {
          return(.error_envelope(
            check$code %||% "ALREADY_REPOSITORY",
            check$message %||% "This folder is already a Git project.",
            recoverable = FALSE
          ))
        }
      }
      # Suggested ignore rules the user kept, written only as a new
      # .gitignore (never over an existing one), and checked before Git is
      # set up so a refusal leaves the folder untouched.
      ignore_rules <- unlist(body$ignore_rules %||% list())
      if (length(ignore_rules) > 0L) {
        if (is.null(launcher_policy)) {
          return(.error_envelope("COMMAND_FAILED", "There are no suggested ignore rules here.", recoverable = FALSE))
        }
        offered <- vapply(launcher_policy$ignore_template, `[[`, character(1), "rule")
        if (!all(ignore_rules %in% offered)) {
          return(.error_envelope("COMMAND_FAILED", "Only the suggested ignore rules can be added here.", recoverable = FALSE))
        }
        if (file.exists(file.path(repo_root, ".gitignore"))) {
          return(.error_envelope("GITIGNORE_EXISTS", "This folder already has a .gitignore; it was left unchanged.", recoverable = TRUE))
        }
      }
      result <- .git_init_workspace(repo_root, git_bin)
      if (isTRUE(result$ok) && length(ignore_rules) > 0L) {
        result <- .write_ignore_template(repo_root, launcher_policy, ignore_rules)
      }
      payload <- .status_payload(repo_root, git_bin, session_state)
      if (!isTRUE(result$ok)) {
        return(.error_envelope(
          result$code, result$message,
          recoverable = result$recoverable %||% TRUE, status_version = payload$version,
          advanced = result$advanced
        ))
      }
      .ok_envelope(list(gitignore_written = isTRUE(result$written)), status_version = payload$version)
    }) |>
    route_post("api/v1/clone", function(request, response) {
      validate_host(request)
      require_auth(request)
      validate_origin(request)

      if (!.git_available(git_bin)) {
        return(.error_envelope("GIT_UNAVAILABLE", "Git isn't available on this computer.", recoverable = FALSE))
      }
      body <- parse_body(request)
      if (is.null(body)) {
        return(.error_envelope("COMMAND_FAILED", "The request could not be understood.", recoverable = FALSE))
      }
      stale <- require_fresh(body, response)
      if (!is.null(stale)) {
        return(stale)
      }

      operation_id <- acquire_mutation_lock(response)
      if (is.null(operation_id)) {
        response$status <- 423L
        return(.error_envelope(
          "OPERATION_IN_PROGRESS",
          "Another repository action is already running. Wait for it to finish and try again."
        ))
      }
      on.exit(release_mutation_lock(), add = TRUE)

      result <- .git_clone_repo(repo_root, git_bin, url = body$url)
      payload <- .status_payload(repo_root, git_bin, session_state)
      if (!isTRUE(result$ok)) {
        return(.error_envelope(
          result$code, result$message,
          recoverable = result$recoverable %||% TRUE, status_version = payload$version,
          advanced = result$advanced
        ))
      }
      .ok_envelope(list(), status_version = payload$version)
    }) |>
    route_post("api/v1/publish", function(request, response) {
      validate_host(request)
      require_auth(request)
      validate_origin(request)

      if (!.git_available(git_bin)) {
        return(.error_envelope("GIT_UNAVAILABLE", "Git isn't available on this computer.", recoverable = FALSE))
      }
      not_a_repo <- require_existing_repo()
      if (!is.null(not_a_repo)) {
        return(not_a_repo)
      }
      body <- parse_body(request)
      if (is.null(body)) {
        return(.error_envelope("COMMAND_FAILED", "The request could not be understood.", recoverable = FALSE))
      }
      stale <- require_fresh(body, response)
      if (!is.null(stale)) {
        return(stale)
      }

      operation_id <- acquire_mutation_lock(response)
      if (is.null(operation_id)) {
        response$status <- 423L
        return(.error_envelope(
          "OPERATION_IN_PROGRESS",
          "Another repository action is already running. Wait for it to finish and try again."
        ))
      }
      on.exit(release_mutation_lock(), add = TRUE)

      result <- .git_publish_repo(repo_root, git_bin, url = body$url, force = isTRUE(body$force))
      note_auth_result(result)
      payload <- .status_payload(repo_root, git_bin, session_state)
      if (!isTRUE(result$ok)) {
        return(.error_envelope(
          result$code, result$message,
          recoverable = result$recoverable %||% TRUE, status_version = payload$version,
          data = result$data, advanced = result$advanced, diagnosis = result$diagnosis
        ))
      }
      .ok_envelope(list(
        remote = result$remote,
        remote_branch = result$remote_branch,
        branch = result$branch,
        sha = result$sha,
        pushed_count = result$pushed_count
      ), status_version = payload$version)
    }) |>
    route_get("api/v1/policy", function(request) {
      validate_host(request)
      require_auth(request)
      policy <- .read_repo_policy(repo_root)
      list(ok = TRUE, data = policy, error = NULL)
    }) |>
    route_get("api/v1/github/status", function(request) {
      validate_host(request)
      require_auth(request)
      policy <- .read_repo_policy(repo_root)
      gh_status <- .github_api_status(repo_root, git_bin, session_state = session_state, policy = policy)
      list(ok = TRUE, data = gh_status, error = NULL)
    }) |>
    route_post("api/v1/github/connect", function(request, response) {
      validate_host(request)
      require_auth(request)
      validate_origin(request)
      body <- parse_body(request)
      if (is.null(body) || is.null(body$token) || !nzchar(trimws(as.character(body$token)[[1]]))) {
        return(.error_envelope("INVALID_SUMMARY", "A GitHub token is required.", recoverable = TRUE))
      }
      token_val <- trimws(as.character(body$token)[[1]])
      policy <- .read_repo_policy(repo_root)
      api_base <- .github_api_base(policy = policy)
      user <- .github_get_user(token_val, api_base = api_base)
      if (is.null(user)) {
        return(.error_envelope("AUTH_REQUIRED", "GitHub could not verify the provided token.", recoverable = TRUE))
      }
      session_state$github_token <- token_val
      payload <- .status_payload(repo_root, git_bin, session_state)
      .ok_envelope(list(connected = TRUE, user = user), status_version = payload$version)
    }) |>
    route_post("api/v1/github/disconnect", function(request, response) {
      validate_host(request)
      require_auth(request)
      validate_origin(request)
      session_state$github_token <- NULL
      payload <- .status_payload(repo_root, git_bin, session_state)
      .ok_envelope(list(connected = FALSE), status_version = payload$version)
    }) |>
    route_post("api/v1/pull-request", function(request, response) {
      validate_host(request)
      require_auth(request)
      validate_origin(request)

      if (!.git_available(git_bin)) {
        return(.error_envelope("GIT_UNAVAILABLE", "Git isn't available on this computer.", recoverable = FALSE))
      }
      not_a_repo <- require_existing_repo()
      if (!is.null(not_a_repo)) {
        return(not_a_repo)
      }
      body <- parse_body(request)
      if (is.null(body)) {
        return(.error_envelope("COMMAND_FAILED", "The request could not be understood.", recoverable = FALSE))
      }
      stale <- require_fresh(body, response)
      if (!is.null(stale)) {
        return(stale)
      }

      operation_id <- acquire_mutation_lock(response)
      if (is.null(operation_id)) {
        response$status <- 423L
        return(.error_envelope(
          "OPERATION_IN_PROGRESS",
          "Another repository action is already running. Wait for it to finish and try again."
        ))
      }
      on.exit(release_mutation_lock(), add = TRUE)

      policy <- .read_repo_policy(repo_root)
      result <- .git_create_pull_request(
        repo_root = repo_root,
        git_bin = git_bin,
        target_branch = body$target_branch,
        pr_branch = body$pr_branch,
        title = body$title,
        body = body$body,
        session_state = session_state,
        policy = policy
      )
      note_auth_result(result)
      payload <- .status_payload(repo_root, git_bin, session_state)
      if (!isTRUE(result$ok)) {
        return(.error_envelope(
          result$code, result$message,
          recoverable = result$recoverable %||% TRUE, status_version = payload$version,
          advanced = result$advanced, diagnosis = result$diagnosis
        ))
      }
      .ok_envelope(list(
        pr_number = result$pr_number,
        pr_url = result$pr_url,
        pr_branch = result$pr_branch,
        base_branch = result$base_branch,
        title = result$title,
        state = result$state
      ), status_version = payload$version)
    }) |>
    route_post("api/v1/github/release", function(request, response) {
      validate_host(request)
      require_auth(request)
      validate_origin(request)

      if (!.git_available(git_bin)) {
        return(.error_envelope("GIT_UNAVAILABLE", "Git isn't available on this computer.", recoverable = FALSE))
      }
      not_a_repo <- require_existing_repo()
      if (!is.null(not_a_repo)) {
        return(not_a_repo)
      }
      body <- parse_body(request)
      if (is.null(body)) {
        return(.error_envelope("COMMAND_FAILED", "The request could not be understood.", recoverable = FALSE))
      }
      stale <- require_fresh(body, response)
      if (!is.null(stale)) {
        return(stale)
      }

      operation_id <- acquire_mutation_lock(response)
      if (is.null(operation_id)) {
        response$status <- 423L
        return(.error_envelope(
          "OPERATION_IN_PROGRESS",
          "Another repository action is already running. Wait for it to finish and try again."
        ))
      }
      on.exit(release_mutation_lock(), add = TRUE)

      policy <- .read_repo_policy(repo_root)
      result <- .git_create_release(
        repo_root = repo_root,
        git_bin = git_bin,
        tag_name = body$tag_name,
        name = body$name,
        body = body$body,
        draft = isTRUE(body$draft),
        prerelease = isTRUE(body$prerelease),
        session_state = session_state,
        policy = policy
      )
      note_auth_result(result)
      payload <- .status_payload(repo_root, git_bin, session_state)
      if (!isTRUE(result$ok)) {
        return(.error_envelope(
          result$code, result$message,
          recoverable = result$recoverable %||% TRUE, status_version = payload$version,
          advanced = result$advanced, diagnosis = result$diagnosis
        ))
      }
      .ok_envelope(list(
        release_id = result$data$release_id,
        tag_name = result$data$tag_name,
        name = result$data$name,
        html_url = result$data$html_url,
        draft = result$data$draft,
        prerelease = result$data$prerelease
      ), status_version = payload$version)
    }) |>
    route_get("api/v1/github/releases", function(request) {
      validate_host(request)
      require_auth(request)
      policy <- .read_repo_policy(repo_root)
      res <- .git_list_releases(repo_root, git_bin, session_state = session_state, policy = policy)
      if (!isTRUE(res$ok)) {
        return(.error_envelope(res$code %||% "GITHUB_API_ERROR", res$message %||% "Failed to list releases."))
      }
      list(ok = TRUE, data = list(releases = res$releases), error = NULL)
    })
}

`%||%` <- function(x, y) if (is.null(x)) y else x

#' Entry point run inside the background server process
#'
#' Reads its configuration from environment variables set by the parent
#' process (`gitneighbr_serve()`) rather than command-line arguments, so the
#' session token never appears in `ps`/`Rscript` argv listings.
#' @noRd
.run_server <- function() {
  repo_root <- Sys.getenv("GITNEIGHBR_REPO")
  host <- Sys.getenv("GITNEIGHBR_HOST", "127.0.0.1")
  port <- as.integer(Sys.getenv("GITNEIGHBR_PORT"))
  token <- Sys.getenv("GITNEIGHBR_TOKEN")
  git_bin <- Sys.getenv("GITNEIGHBR_GIT")
  www_dir <- Sys.getenv("GITNEIGHBR_WWW_DIR", system.file("www", package = "gitneighbr"))

  api <- .build_api(repo_root, git_bin, token, port, www_dir)
  plumber2::api_run(api, host = host, port = port, block = TRUE, showcase = FALSE)
}
