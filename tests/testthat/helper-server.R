# Test helpers for exercising R/server.R's plumber2 routes.
#
# `open_repo()` always runs the real server in an independent `Rscript`
# child process (see `R/serve.R`) so the R console is never blocked -- a
# hard product requirement, not a testing convenience. But that means a
# real `open_repo()` session (as used in test-session-lifecycle.R) executes
# every route handler in a *different* R process, invisible to covr's
# line-coverage instrumentation of the parent process, however thoroughly
# it's exercised over real HTTP.
#
# plumber2/fiery's `$test_request()` sidesteps this: it runs a request
# through the exact same header/body/route logic `api_run()` would, but
# in-process and without a real socket round-trip, so route handler code
# runs (and is tracked by covr) in the current test session. It takes a
# Rook-shaped environment (not a `reqres::Request`) -- `reqres:::mock_rook()`
# builds one from a URL/method/headers/body, matching what `test_request()`
# actually expects (confirmed empirically; `mock_request()` wraps too early).

#' A freshly initialized, identity-configured Git repository for tests
#' @noRd
.new_test_repo <- function(env = parent.frame()) {
  git <- unname(Sys.which("git"))
  testthat::skip_if(!nzchar(git), "git not available")
  dir <- withr::local_tempdir(.local_envir = env)
  run <- function(...) processx::run(git, c("-C", dir, ...), error_on_status = TRUE)
  run("init", "-q", "-b", "main")
  run("config", "user.email", "test@example.com")
  run("config", "user.name", "Test")
  list(dir = dir, git = git, run = run)
}

#' Build an in-process `.build_api()` instance for `repo_root`
#'
#' Binds a real loopback port (plumber2/httpuv require one even though
#' `test_request()` never opens a socket against it) and registers a
#' `withr::defer()` to stop it when `env` exits. Pass `session_state` (e.g.
#' with `mutation_lock <- TRUE` pre-set) to exercise state-dependent branches
#' without a real race.
#' @noRd
.test_api <- function(repo_root, git_bin = unname(Sys.which("git")), token = "test-session-token",
                       www_dir = NULL, session_state = NULL, env = parent.frame()) {
  if (is.null(www_dir)) {
    www_dir <- withr::local_tempdir(.local_envir = env)
  }
  # `.find_free_port()` has an inherent (documented) TOCTOU race between
  # probing a port and httpuv actually binding it a moment later; retry a
  # handful of times rather than let that rare collision flake the suite.
  server <- NULL
  port <- NULL
  for (attempt in 1:5) {
    port <- gitneighbr:::.find_free_port()
    app <- gitneighbr:::.build_api(repo_root, git_bin, token, port, www_dir = www_dir, session_state = session_state)
    server <- tryCatch(
      plumber2::api_run(app, host = "127.0.0.1", port = port, block = FALSE, silent = TRUE),
      error = function(e) NULL
    )
    if (!is.null(server)) break
  }
  if (is.null(server)) {
    stop("gitneighbr test helper: could not bind a test server after 5 attempts.", call. = FALSE)
  }
  withr::defer(server$stop(), envir = env)
  list(server = server, port = port, token = token, host = sprintf("127.0.0.1:%d", port))
}

#' A fresh session-state environment, same shape `.build_api()` builds itself
#' @noRd
.new_session_state <- function() {
  session_state <- new.env(parent = emptyenv())
  session_state$version <- 0L
  session_state$last_snapshot <- NULL
  session_state$auth_required <- FALSE
  session_state$pending_tags <- character()
  session_state$pushed_tags <- character()
  session_state$mutation_lock <- FALSE
  session_state$github_token <- NULL
  session_state
}

#' Simulate one HTTP request against a `.test_api()` instance
#'
#' `resp$json` is the parsed JSON body (or `NULL` if the body wasn't JSON).
#' @noRd
.api_req <- function(api, path, method = "get", headers = list(), body = NULL, host = NULL) {
  host_hdr <- host %||% api$host
  h <- headers
  if (!"Host" %in% names(h)) {
    h$Host <- host_hdr
  }
  content <- ""
  if (!is.null(body)) {
    content <- as.character(jsonlite::toJSON(body, auto_unbox = TRUE, null = "null"))
    if (!"Content-Type" %in% names(h)) {
      h[["Content-Type"]] <- "application/json"
    }
  }
  rook <- reqres:::mock_rook(
    url = paste0("http://", host_hdr, path),
    method = method,
    headers = h,
    content = content
  )
  resp <- api$server$test_request(rook)
  resp$json <- tryCatch(jsonlite::fromJSON(resp$body, simplifyVector = TRUE), error = function(e) NULL)
  resp
}

#' The `Authorization` header for a `.test_api()` instance's bearer token
#' @noRd
.api_auth <- function(api) list(Authorization = paste("Bearer", api$token))

#' Fetch the current `status_version` from a `.test_api()` instance
#' @noRd
.api_version <- function(api) {
  .api_req(api, "/api/v1/status", headers = .api_auth(api))$json$status_version
}
