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
#' @noRd
.build_api <- function(repo_root, git_bin, token, port, www_dir = system.file("www", package = "gitneighbr")) {
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
  session_state <- new.env(parent = emptyenv())
  session_state$version <- 0L
  session_state$last_snapshot <- NULL
  session_state$auth_required <- FALSE
  session_state$pending_tags <- character()
  session_state$pushed_tags <- character()
  session_state$mutation_lock <- FALSE

  # DNS-rebinding mitigation (spec Sec 12.1): a request whose `Host` header
  # doesn't name this exact loopback authority is rejected outright, before
  # authentication -- an attacker's rebound hostname can carry a stolen
  # token but can never legitimately claim to *be* "127.0.0.1:<port>".
  validate_host <- function(request) {
    if (!.valid_host_header(request$get_header("Host"), port)) {
      plumber2::abort_bad_request("Invalid Host header.")
    }
  }

  require_auth <- function(request) {
    supplied <- request$get_header("Authorization")
    supplied <- sub("^Bearer\\s+", "", supplied %||% "")
    if (!.tokens_match(supplied, token)) {
      plumber2::abort_unauthorized("Missing or invalid session token.")
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

  plumber2::api(host = "127.0.0.1") |>
    # `httpuv`'s static-file layer intercepts every request under `at`
    # (prefix-matched, so "/" means *every* path) before the router ever
    # sees it, and only knows how to serve GET/HEAD - any other method
    # anywhere on the site, including a POST to "/api/v1/...", gets a flat
    # 400 with `fallthrough` having no effect (fallthrough only covers a
    # missing *file* on GET). `except` tells httpuv to treat that prefix as
    # not-static at all, handing it to the router (and its own methods)
    # unmodified.
    plumber2::api_statics(at = "/", path = www_dir, fallthrough = TRUE, except = "api") |>
    plumber2::api_get("/api/v1/health", function(request) {
      validate_host(request)
      list(ok = TRUE, data = list(status = "ok"), error = NULL)
    }) |>
    plumber2::api_get("/api/v1/status", function(request) {
      validate_host(request)
      require_auth(request)

      show_ignored <- identical(request$query$show_ignored, "true")
      payload <- .status_payload(repo_root, git_bin, session_state, show_ignored = show_ignored)
      .ok_envelope(payload$data, status_version = payload$version)
    }) |>
    plumber2::api_get("/api/v1/changes", function(request) {
      validate_host(request)
      require_auth(request)

      if (!.git_available(git_bin)) {
        return(.error_envelope("GIT_UNAVAILABLE", "Git isn't available on this computer.", recoverable = FALSE))
      }
      not_a_repo <- require_existing_repo()
      if (!is.null(not_a_repo)) {
        return(not_a_repo)
      }
      .ok_envelope(list(changes = .git_changes(repo_root, git_bin)))
    }) |>
    plumber2::api_get("/api/v1/diff", function(request) {
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

      result <- .git_diff(repo_root, git_bin, path, offset_lines = offset_lines)
      if (!result$found) {
        return(.error_envelope("COMMAND_FAILED", "That path has no pending changes to show."))
      }
      .ok_envelope(result[names(result) != "found"])
    }) |>
    plumber2::api_get("/api/v1/identity", function(request) {
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
    plumber2::api_get("/api/v1/credential-diagnosis", function(request) {
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
    plumber2::api_get("/api/v1/diagnostic-report", function(request) {
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
    plumber2::api_post("/api/v1/identity", function(request, response) {
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
    plumber2::api_post("/api/v1/commit", function(request, response) {
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

      result <- .git_commit_selected(
        repo_root, git_bin,
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
    plumber2::api_post("/api/v1/refresh-remote", function(request, response) {
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
    plumber2::api_post("/api/v1/push", function(request, response) {
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
    plumber2::api_post("/api/v1/update", function(request, response) {
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
    plumber2::api_post("/api/v1/tag", function(request, response) {
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
    plumber2::api_post("/api/v1/push-tag", function(request, response) {
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
    plumber2::api_post("/api/v1/restore", function(request, response) {
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
    plumber2::api_post("/api/v1/trash", function(request, response) {
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
    plumber2::api_post("/api/v1/ignore", function(request, response) {
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
    plumber2::api_post("/api/v1/init", function(request, response) {
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

      result <- .git_init_workspace(repo_root, git_bin)
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
    plumber2::api_post("/api/v1/clone", function(request, response) {
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
    plumber2::api_post("/api/v1/publish", function(request, response) {
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
