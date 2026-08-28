#' Initialize a new Git repository at a workspace path
#'
#' Implements the "start from scratch" half of spec Sec 22 (0.2.0): creates
#' `workspace_path` if it doesn't exist yet (it's fine for it to already
#' contain ordinary files -- that's the normal "add version control to an
#' existing project" case) and runs `git init` in it. Refuses if
#' `workspace_path` is already a Git working tree, so a stray retry can
#' never re-initialize (and thereby reset branch/config state on) a
#' repository that already exists.
#'
#' @param workspace_path The path to initialize. Need not exist yet.
#' @param git_bin Path to the `git` executable.
#' @return A list. On success: `ok = TRUE`. On failure: `ok = FALSE`,
#'   `code`, `message`, `recoverable`, and (for a failed `git init`)
#'   `advanced`.
#' @noRd
.git_init_workspace <- function(workspace_path, git_bin) {
  if (identical(.git_repo_kind(workspace_path, git_bin), "worktree")) {
    return(list(
      ok = FALSE, code = "ALREADY_A_REPOSITORY",
      message = "This folder is already a Git project.", recoverable = FALSE
    ))
  }

  created <- tryCatch(
    {
      fs::dir_create(workspace_path, recurse = TRUE)
      TRUE
    },
    error = function(e) FALSE
  )
  if (!created) {
    return(list(
      ok = FALSE, code = "COMMAND_FAILED",
      message = "Could not create that folder.", recoverable = TRUE
    ))
  }

  # `-b main` needs Git >=2.28; `open_repo()` already enforces
  # `.min_git_version` (2.34.0) before the server ever starts.
  init_args <- c("-C", workspace_path, "init", "-q", "-b", "main")
  result <- processx::run(git_bin, init_args, error_on_status = FALSE, timeout = 15)
  if (!identical(result$status, 0L)) {
    return(list(
      ok = FALSE, code = "COMMAND_FAILED",
      message = "Could not initialize a Git repository here.", recoverable = TRUE,
      advanced = .advanced_block(init_args, result)
    ))
  }

  list(ok = TRUE)
}
