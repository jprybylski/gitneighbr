#' Restore one tracked file's working-tree content from the index
#'
#' Implements spec Sec 8.7 / Sec 13.2: `git restore --worktree -- <path>`
#' only ever touches the named path's *working tree* content, never the
#' index -- restore is deliberately kept out of the index-manipulation code
#' path that `.git_commit_selected()` owns (spec Sec 13.4's staging
#' invariants). In the overwhelmingly common case (nothing pre-staged
#' outside a `gitneighbr`-driven commit, so the index already matches
#' `HEAD`) this restores the file to its last saved snapshot exactly as the
#' interface promises; if a path happens to be staged with edited content
#' *and* separately edited again unstaged, only the unstaged layer is
#' undone, mirroring `git restore`'s own default `--source` (the index).
#'
#' "State freshness" (spec Sec 8.7 point 4) is verified against a fresh
#' `.git_status_entries()` read: a path no longer present as an ordinary or
#' renamed change is refused as `STATE_CHANGED` (the list the user acted on
#' is stale -- this also covers an untracked path, which has no committed
#' "last saved version" to restore to), and a path with an unresolved merge
#' conflict is refused as `CONFLICTS_PRESENT`.
#'
#' @return A list. On success: `ok = TRUE`, `path`. On failure: `ok =
#'   FALSE`, `code`, `message`, `recoverable`.
#' @noRd
.git_restore_tracked_file <- function(repo_root, git_bin, path) {
  path <- if (is.null(path)) "" else as.character(path)[[1]]

  if (!.validate_repo_relative_path(repo_root, path)) {
    return(list(
      ok = FALSE, code = "PATH_OUTSIDE_REPOSITORY",
      message = "That path is not inside this repository.", recoverable = FALSE
    ))
  }

  status_entries <- .git_status_entries(repo_root, git_bin)
  entry <- Find(function(e) identical(e$path, path), status_entries)

  if (!is.null(entry) && entry$kind == "conflicted") {
    return(list(
      ok = FALSE, code = "CONFLICTS_PRESENT",
      message = "Resolve this file's conflict before restoring it.", recoverable = FALSE
    ))
  }
  if (is.null(entry) || !(entry$kind %in% c("ordinary", "rename"))) {
    return(list(
      ok = FALSE, code = "STATE_CHANGED",
      message = "The repository changed since this was shown. Refresh and try again.",
      recoverable = TRUE
    ))
  }

  restore_args <- c("-C", repo_root, "restore", "--worktree", "--", path)
  restore_result <- processx::run(
    git_bin, restore_args,
    error_on_status = FALSE, timeout = 15
  )
  if (!identical(restore_result$status, 0L)) {
    return(list(
      ok = FALSE, code = "COMMAND_FAILED",
      message = "Git could not restore that file.", recoverable = TRUE,
      advanced = .advanced_block(restore_args, restore_result)
    ))
  }

  list(ok = TRUE, path = path)
}
