#' Map `git clone`'s stderr to a stable application error code
#'
#' Order matters: GitHub deliberately returns the same "not found" response
#' for a nonexistent repository and for a private one the caller isn't
#' authenticated for (so as not to leak which is which to an unauthenticated
#' caller) -- a genuine credential-helper failure is checked first since
#' that is unambiguous, and "not found" is classified separately with a
#' message that names both possibilities.
#' @noRd
.classify_clone_failure <- function(stderr_text) {
  text <- tolower(stderr_text %||% "")
  if (grepl("already exists and is not an empty directory", text)) {
    return("CLONE_DESTINATION_NOT_EMPTY")
  }
  if (grepl("authentication failed|permission denied|could not read username|could not read password|access denied|invalid credentials", text)) {
    return("AUTH_REQUIRED")
  }
  if (grepl("repository not found|not found", text)) {
    return("REMOTE_NOT_FOUND")
  }
  if (grepl("could not resolve host|network is unreachable|connection timed out|temporary failure|could not read from remote|no route to host", text)) {
    return("REMOTE_UNREACHABLE")
  }
  "COMMAND_FAILED"
}

#' Clone a remote repository into a workspace path
#'
#' Implements the "clone an existing GitHub repository" half of spec Sec 22
#' (0.2.0). Refuses if `workspace_path` is already a Git working tree, the
#' same guard `.git_init_workspace()` uses; `git clone` itself refuses (as
#' `CLONE_DESTINATION_NOT_EMPTY`) if the destination exists and already has
#' other files in it.
#'
#' Does not attach a `.git_credential_diagnosis()` block to an
#' `AUTH_REQUIRED` failure the way push/fetch/publish do: that helper reads
#' guidance from an already-configured repository's remote config, which
#' does not exist yet when cloning has failed.
#'
#' @param workspace_path The clone destination. Need not exist yet, but its
#'   parent directory will be created if missing.
#' @param git_bin Path to the `git` executable.
#' @param url The remote URL to clone.
#' @return A list. On success: `ok = TRUE`. On failure: `ok = FALSE`,
#'   `code`, `message`, `recoverable`, and (for a failed `git clone`)
#'   `advanced`.
#' @noRd
.git_clone_repo <- function(workspace_path, git_bin, url) {
  if (identical(.git_repo_kind(workspace_path, git_bin), "worktree")) {
    return(list(
      ok = FALSE, code = "ALREADY_A_REPOSITORY",
      message = "This folder is already a Git project.", recoverable = FALSE
    ))
  }
  if (!nzchar(trimws(url %||% ""))) {
    return(list(
      ok = FALSE, code = "INVALID_REMOTE_URL",
      message = "Enter the address of the GitHub repository to clone.", recoverable = TRUE
    ))
  }

  parent_created <- tryCatch(
    {
      fs::dir_create(fs::path_dir(workspace_path), recurse = TRUE)
      TRUE
    },
    error = function(e) FALSE
  )
  if (!parent_created) {
    return(list(
      ok = FALSE, code = "COMMAND_FAILED",
      message = "Could not create that folder.", recoverable = TRUE
    ))
  }

  clone_args <- c("clone", url, workspace_path)
  result <- processx::run(git_bin, clone_args, error_on_status = FALSE, timeout = 300)
  if (!identical(result$status, 0L)) {
    return(list(
      ok = FALSE, code = .classify_clone_failure(result$stderr),
      message = "Could not clone that repository.", recoverable = TRUE,
      advanced = .advanced_block(clone_args, result)
    ))
  }

  list(ok = TRUE)
}
