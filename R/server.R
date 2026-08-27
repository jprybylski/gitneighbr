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

#' Build the plumber2 app for a single repository session
#'
#' @param repo_root Canonical path to the Git working tree root.
#' @param git_bin Path to the `git` executable.
#' @param token Bearer token required on every `/api/v1/*` request.
#' @param www_dir Directory containing the precompiled frontend.
#' @noRd
.build_api <- function(repo_root, git_bin, token, www_dir = system.file("www", package = "gitneighbr")) {
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

  require_auth <- function(request) {
    supplied <- request$get_header("Authorization")
    supplied <- sub("^Bearer\\s+", "", supplied %||% "")
    if (!.tokens_match(supplied, token)) {
      plumber2::abort_unauthorized("Missing or invalid session token.")
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
    plumber2::api_get("/api/v1/health", function() {
      list(ok = TRUE, data = list(status = "ok"), error = NULL)
    }) |>
    plumber2::api_get("/api/v1/status", function(request) {
      require_auth(request)

      git_ok <- .git_available(git_bin)
      status <- if (git_ok) .git_status(repo_root, git_bin) else NULL
      state <- .primary_state(status, git_ok)

      .ok_envelope(list(
        repository = list(
          root_display = fs::path_file(repo_root)
        ),
        primary_state = state,
        upstream = status$upstream,
        branch = status$branch,
        ahead = status$ahead %||% 0L,
        behind = status$behind %||% 0L,
        staged_count = status$staged_count %||% 0L,
        unstaged_count = status$unstaged_count %||% 0L,
        untracked_count = status$untracked_count %||% 0L
      ))
    }) |>
    plumber2::api_get("/api/v1/changes", function(request) {
      require_auth(request)

      if (!.git_available(git_bin)) {
        return(.error_envelope("GIT_UNAVAILABLE", "Git isn't available on this computer.", recoverable = FALSE))
      }
      .ok_envelope(list(changes = .git_changes(repo_root, git_bin)))
    }) |>
    plumber2::api_get("/api/v1/diff", function(request) {
      require_auth(request)

      if (!.git_available(git_bin)) {
        return(.error_envelope("GIT_UNAVAILABLE", "Git isn't available on this computer.", recoverable = FALSE))
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
    plumber2::api_post("/api/v1/commit", function(request) {
      require_auth(request)

      if (!.git_available(git_bin)) {
        return(.error_envelope("GIT_UNAVAILABLE", "Git isn't available on this computer.", recoverable = FALSE))
      }

      # `request$parse()` requires an explicit named parser list on every
      # call (it has no implicit default), so a malformed/absent JSON body
      # is caught here and turned into the same envelope shape every other
      # endpoint uses, instead of plumber2's plain-text 400 response.
      body <- tryCatch(
        {
          do.call(request$parse, plumber2::get_parsers())
          request$body %||% list()
        },
        error = function(e) NULL
      )
      if (is.null(body)) {
        return(.error_envelope("COMMAND_FAILED", "The request could not be understood.", recoverable = FALSE))
      }

      result <- .git_commit_selected(
        repo_root, git_bin,
        selected_paths = body$paths,
        summary = body$summary,
        details = body$details
      )
      if (!isTRUE(result$ok)) {
        return(.error_envelope(result$code, result$message, recoverable = result$recoverable %||% TRUE))
      }
      .ok_envelope(list(sha = result$sha, summary = result$summary))
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

  api <- .build_api(repo_root, git_bin, token, www_dir)
  plumber2::api_run(api, host = host, port = port, block = TRUE, showcase = FALSE)
}
