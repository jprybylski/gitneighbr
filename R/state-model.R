#' Derive the primary repository state
#'
#' A minimal slice of the state model described in the project spec: enough
#' states to describe an ordinary repo truthfully, with the richer notice
#' system (hooks, LFS, submodules, signing, ...) left for a later phase.
#'
#' @param status A list as returned by `.git_status()`, or `NULL` if the
#'   path is not inside a Git working tree.
#' @param git_ok Whether a usable Git binary was found at all.
#' @return A single string, one of `GIT_UNAVAILABLE`, `NOT_REPOSITORY`,
#'   `DETACHED_HEAD`, `NO_UPSTREAM`, `READY`, `CHANGES_ONLY`, `LOCAL_ONLY`,
#'   `CHANGES_AND_LOCAL`, `REMOTE_ONLY_CLEAN`, `REMOTE_ONLY_DIRTY`, or
#'   `DIVERGED`.
#' @noRd
.primary_state <- function(status, git_ok) {
  if (!git_ok) {
    return("GIT_UNAVAILABLE")
  }
  if (is.null(status)) {
    return("NOT_REPOSITORY")
  }
  if (status$detached) {
    return("DETACHED_HEAD")
  }
  if (is.null(status$upstream)) {
    return("NO_UPSTREAM")
  }

  ahead <- status$ahead
  behind <- status$behind
  has_changes <- status$has_changes

  if (ahead > 0L && behind > 0L) {
    return("DIVERGED")
  }
  if (ahead > 0L) {
    return(if (has_changes) "CHANGES_AND_LOCAL" else "LOCAL_ONLY")
  }
  if (behind > 0L) {
    return(if (has_changes) "REMOTE_ONLY_DIRTY" else "REMOTE_ONLY_CLEAN")
  }
  if (has_changes) "CHANGES_ONLY" else "READY"
}
