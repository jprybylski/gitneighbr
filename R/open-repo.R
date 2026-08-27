#' Open the gitneighbr app for a repository
#'
#' Launches a local, loopback-only web server for the Git repository at
#' `path` and (by default) opens it in the browser. Returns immediately: the
#' server runs in an independent background process, so the R console is
#' never blocked. Stop it later with `session$stop()` or [stop_session()].
#'
#' @param path Path inside the Git working tree to serve. Defaults to the
#'   current directory.
#' @param browse Whether to open the app in the default browser. Defaults to
#'   `TRUE` in interactive sessions.
#' @param host Loopback host to bind to. Only `"127.0.0.1"`/`"localhost"`
#'   are supported.
#' @param port Port to bind to, or `0L` (the default) to pick a free port.
#' @param git Path to the `git` executable.
#' @return A `gitneighbr_session` object, invisibly-printable and safe to
#'   discard (the server keeps running; recover it later with
#'   [stop_session()] using the port).
#' @export
open_repo <- function(path = ".",
                       browse = interactive(),
                       host = getOption("gitneighbr.host", "127.0.0.1"),
                       port = getOption("gitneighbr.port", 0L),
                       git = getOption("gitneighbr.git", unname(Sys.which("git")))) {
  git <- unname(git)
  if (!nzchar(git)) {
    stop("gitneighbr: no `git` executable found. Install Git and ensure it is on PATH.", call. = FALSE)
  }
  if (!.git_available(git)) {
    stop("gitneighbr: the configured `git` executable at '", git, "' could not be run.", call. = FALSE)
  }

  repo_root <- .git_root(path, git)
  if (is.null(repo_root)) {
    stop("gitneighbr: '", path, "' is not inside a Git working tree.", call. = FALSE)
  }

  session <- .gitneighbr_serve(repo_root = repo_root, host = host, port = port, git_bin = git)

  if (isTRUE(browse)) {
    session$browse()
  }

  session
}

#' Stop a gitneighbr session
#'
#' @param session A `gitneighbr_session` object, or `NULL`.
#' @param port If `session` is `NULL`, the port of a session to look up and
#'   stop by force (e.g. after the R session that started it was restarted).
#' @param host Host to look on when stopping by port.
#' @export
stop_session <- function(session = NULL, port = NULL, host = "127.0.0.1") {
  if (!is.null(session)) {
    return(session$stop())
  }
  if (is.null(port)) {
    stop("gitneighbr: provide either `session` or `port`.", call. = FALSE)
  }
  .stop_server_on_port(host, port)
}
