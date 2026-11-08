#' Features a managed session can offer
#'
#' `local`: status, changes, diffs, selected-file commits, `.gitignore`
#' help, explicit init and identity. `remote`: check destination, send and
#' update. The rest are the standalone app's extra actions.
#' @noRd
.MANAGED_FEATURES <- c("local", "remote", "tags", "restore", "trash", "clone", "github")

#' The header a launcher's proxy uses to pass the session token
#' @noRd
.MANAGED_TOKEN_HEADER <- "X-Gitneighbr-Token"

#' The header that carries the shutdown token on `POST api/v1/shutdown`
#' @noRd
.MANAGED_SHUTDOWN_HEADER <- "X-Gitneighbr-Shutdown-Token"

#' Parse a `--features=` value
#' @noRd
.parse_features <- function(features) {
  if (is.null(features)) {
    return("local")
  }
  values <- unique(trimws(unlist(strsplit(as.character(features), ",", fixed = TRUE))))
  values <- values[nzchar(values)]
  unknown <- setdiff(values, .MANAGED_FEATURES)
  if (length(unknown) > 0L) {
    stop("Unknown feature(s): ", paste(unknown, collapse = ", "),
      ". Known: ", paste(.MANAGED_FEATURES, collapse = ", "), ".",
      call. = FALSE
    )
  }
  if (!"local" %in% values) {
    stop("`features` must include \"local\".", call. = FALSE)
  }
  values
}

#' Build the configuration a managed session runs with
#'
#' `NULL` everywhere a managed value is absent means standalone behaviour.
#' @noRd
.managed_config <- function(shutdown_token = NULL, features = "local") {
  config <- list(
    shutdown_token = shutdown_token,
    features = .parse_features(features),
    control = new.env(parent = emptyenv())
  )
  config$control$stop_requested <- FALSE
  config
}

#' Run gitneighbr in the foreground for a launcher
#'
#' Serves one folder until a launcher stops it with `POST api/v1/shutdown`
#' (or the process is killed). Unlike [open_repo()] it blocks, never opens a
#' browser, needs an explicit port, and prints one line to standard output
#' once the socket is bound:
#'
#' `GITNEIGHBR_READY {"url":"http://127.0.0.1:<port><base_path>",...}`
#'
#' The session token is never placed in a URL. A launcher's reverse proxy
#' passes it on every request in the `X-Gitneighbr-Token` header; the page
#' itself never sees it.
#'
#' `inst/scripts/serve.R` is the command-line form of this function; see
#' `vignette("launcher")`.
#'
#' @param repo The folder to serve.
#' @param port Loopback port to listen on.
#' @param git Absolute path to the `git` executable to run.
#' @param api_token Token required on every request except health.
#' @param host Loopback host: `"127.0.0.1"` (default) or `"localhost"`.
#' @param base_path The URL path a reverse proxy serves the app under.
#' @param shutdown_token Token that `POST api/v1/shutdown` must carry in the
#'   `X-Gitneighbr-Shutdown-Token` header. Without it there is no shutdown
#'   route.
#' @param features Comma-separated features to offer (see Details). Always
#'   includes `"local"`.
#' @return Invisibly, `0L` once the server has stopped.
#' @export
gitneighbr_serve <- function(repo, port, git, api_token, host = "127.0.0.1", base_path = "/",
                             shutdown_token = NULL, features = "local") {
  .assert_loopback_host(host)
  port <- suppressWarnings(as.integer(port))
  if (length(port) != 1L || is.na(port) || port < 1L || port > 65535L) {
    stop("`port` must be a TCP port number.", call. = FALSE)
  }
  if (!is.character(repo) || length(repo) != 1L || !dir.exists(repo)) {
    stop("`repo` must be an existing folder.", call. = FALSE)
  }
  if (!is.character(git) || length(git) != 1L || !.is_absolute_path(git) || !file.exists(git)) {
    stop("`git` must be the absolute path of an existing Git executable.", call. = FALSE)
  }
  for (secret in list(api_token, shutdown_token)) {
    if (!is.null(secret) && (!is.character(secret) || length(secret) != 1L || nchar(secret) < 16L)) {
      stop("Tokens must be strings of at least 16 characters.", call. = FALSE)
    }
  }
  if (is.null(api_token)) {
    stop("`api_token` is required.", call. = FALSE)
  }

  repo_root <- as.character(fs::path_real(repo))
  base_path <- .normalize_base_path(base_path)
  managed <- .managed_config(shutdown_token = shutdown_token, features = features)
  api <- .build_api(repo_root, git, api_token, port, base_path = base_path, managed = managed)
  .serve_run(api, host, port, base_path, managed$control)
}

#' Is `path` absolute on this platform?
#' @noRd
.is_absolute_path <- function(path) {
  grepl("^(/|[A-Za-z]:[/\\\\]|\\\\\\\\)", path)
}

#' The line a launcher waits for, written once the socket is bound
#' @noRd
.serve_ready_line <- function(host, port, base_path) {
  info <- list(
    url = paste0("http://", host, ":", port, base_path),
    host = host,
    port = port,
    pid = Sys.getpid(),
    version = as.character(utils::packageVersion("gitneighbr"))
  )
  paste("GITNEIGHBR_READY", jsonlite::toJSON(info, auto_unbox = TRUE))
}

#' Run an API until the shutdown route asks it to stop
#'
#' fiery checks for a stop at the end of every service cycle, so a running
#' request (a Git command included) always finishes and its reply is sent
#' before the socket closes.
#' @noRd
.serve_run <- function(api, host, port, base_path, control) {
  ready <- FALSE
  api <- plumber2::api_on(api, "cycle-start", function(...) {
    if (!ready) {
      ready <<- TRUE
      cat(.serve_ready_line(host, port, base_path), "\n", sep = "")
      flush(stdout())
    }
  })
  api <- plumber2::api_on(api, "cycle-end", function(...) {
    if (isTRUE(control$stop_requested)) {
      plumber2::api_stop(api)
    }
  })
  tryCatch(
    plumber2::api_run(api, host = host, port = port, block = TRUE, showcase = FALSE, silent = TRUE),
    interrupt = function(e) invisible(NULL)
  )
  invisible(0L)
}
