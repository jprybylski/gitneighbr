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
  plumber2::register_serializer(
    "json",
    function(...) reqres::format_json(..., auto_unbox = TRUE),
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
    plumber2::api_statics(at = "/", path = www_dir, fallthrough = TRUE) |>
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
