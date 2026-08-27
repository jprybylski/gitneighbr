#' Validate a commit summary against the spec's 3-72 Unicode character rule
#' @noRd
.validate_commit_summary <- function(summary) {
  if (is.null(summary) || length(summary) != 1L || !is.character(summary) || is.na(summary)) {
    return(FALSE)
  }
  n <- nchar(summary, type = "chars")
  n >= 3L && n <= 72L
}

#' Map `git commit`'s stderr to a stable application error code
#'
#' A best-effort classification (hooks and signing are the only failure
#' modes the spec calls out by name); anything else falls back to the
#' generic `COMMAND_FAILED` code.
#' @noRd
.classify_commit_failure <- function(stderr_text) {
  text <- tolower(stderr_text %||% "")
  if (grepl("hook", text, fixed = TRUE)) {
    return("HOOK_FAILED")
  }
  if (grepl("gpg|signing|sign the commit", text)) {
    return("SIGNING_FAILED")
  }
  "COMMAND_FAILED"
}

#' Repository-relative paths touched by one status entry
#'
#' For a rename, this is both sides of the pair: the old and new path must
#' always move together, or the index ends up in a half-renamed state (see
#' `.git_commit_selected()`).
#' @noRd
.commit_entry_paths <- function(entry) {
  unique(c(entry$path, entry$old_path))
}

#' Stage exactly the user's selected paths and create one commit
#'
#' Implements the staging invariants from spec Sec 13.4: the pre-operation
#' index is snapshotted with `git write-tree` (a pure index-read, no
#' filesystem side effects), the desired staged set is built explicitly
#' from `selected_paths` rather than trusting whatever happened to already
#' be staged, and the snapshot is restored with `git read-tree` (index-only,
#' never touches the working tree) if anything goes wrong before Git itself
#' commits.
#'
#' Any path already staged (from before `gitneighbr` was even opened) that
#' is not part of the current selection is explicitly unstaged first with
#' `git reset -- <path>`, so a previously staged-but-deselected path can
#' never leak into the new commit. `git add -- <path>` both stages ordinary
#' additions/modifications and stages a deletion for a tracked path missing
#' from the working tree, so one code path covers new, changed, deleted,
#' and renamed entries.
#'
#' @param selected_paths Character vector of repository-relative paths the
#'   user selected, matched against a fresh `.git_status_entries()` read of
#'   the repository (never trusted at face value: a path that no longer
#'   matches a real pending change is treated as `STATE_CHANGED`, and a
#'   selection touching a conflicted path -- or a repository with *any*
#'   unresolved conflicts, which blocks committing regardless of selection
#'   -- is rejected as `CONFLICTS_PRESENT`).
#' @param summary Required 3-72 character commit subject.
#' @param details Optional longer commit body.
#' @return A list. On success: `ok = TRUE`, `sha`, `summary`. On failure:
#'   `ok = FALSE`, `code`, `message`, `recoverable`.
#' @noRd
.git_commit_selected <- function(repo_root, git_bin, selected_paths, summary, details = NULL) {
  selected_paths <- unique(as.character(selected_paths %||% character()))
  summary <- if (is.null(summary)) "" else trimws(as.character(summary)[[1]])
  details <- if (is.null(details) || length(details) != 1L || !nzchar(trimws(details))) NULL else details

  if (!.validate_commit_summary(summary)) {
    return(list(
      ok = FALSE, code = "INVALID_SUMMARY",
      message = "Summary must be between 3 and 72 characters.", recoverable = TRUE
    ))
  }
  if (length(selected_paths) == 0L) {
    return(list(
      ok = FALSE, code = "EMPTY_SELECTION",
      message = "Select at least one file to save.", recoverable = TRUE
    ))
  }

  status_entries <- .git_status_entries(repo_root, git_bin)

  has_conflicts <- any(vapply(status_entries, function(e) identical(e$kind, "conflicted"), logical(1)))
  if (has_conflicts) {
    return(list(
      ok = FALSE, code = "CONFLICTS_PRESENT",
      message = "Resolve conflicted files before saving a snapshot.", recoverable = FALSE
    ))
  }

  by_path <- status_entries
  names(by_path) <- vapply(status_entries, `[[`, character(1), "path")
  unknown <- setdiff(selected_paths, names(by_path))
  if (length(unknown) > 0L) {
    return(list(
      ok = FALSE, code = "STATE_CHANGED",
      message = "The repository changed since this list was shown. Refresh and try again.",
      recoverable = TRUE
    ))
  }

  # Only the current path is ever `git add`-able: once a rename is index-
  # recorded at all, its old path has no index entry left to reconcile, and
  # `git add -- <old_path>` errors ("did not match any files"). The old
  # path only matters for unstaging (below), where both sides must move
  # together to fully revert to HEAD.
  add_paths <- selected_paths
  deselected_staged <- Filter(
    function(e) isTRUE(e$staged) && !(e$path %in% selected_paths),
    status_entries
  )
  reset_paths <- unique(unlist(lapply(deselected_staged, .commit_entry_paths)))

  tree_result <- processx::run(
    git_bin, c("-C", repo_root, "write-tree"),
    error_on_status = FALSE, timeout = 15
  )
  if (!identical(tree_result$status, 0L)) {
    return(list(
      ok = FALSE, code = "COMMAND_FAILED",
      message = "Could not read the current index.", recoverable = TRUE
    ))
  }
  original_tree <- trimws(tree_result$stdout)

  restore_index <- function() {
    processx::run(git_bin, c("-C", repo_root, "read-tree", original_tree), error_on_status = FALSE, timeout = 15)
  }

  if (length(reset_paths) > 0L) {
    result <- processx::run(git_bin, c("-C", repo_root, "reset", "--", reset_paths), error_on_status = FALSE, timeout = 15)
    if (!identical(result$status, 0L)) {
      restore_index()
      return(list(
        ok = FALSE, code = "COMMAND_FAILED",
        message = "Could not update the saved selection.", recoverable = TRUE
      ))
    }
  }
  if (length(add_paths) > 0L) {
    result <- processx::run(git_bin, c("-C", repo_root, "add", "--", add_paths), error_on_status = FALSE, timeout = 15)
    if (!identical(result$status, 0L)) {
      restore_index()
      return(list(
        ok = FALSE, code = "COMMAND_FAILED",
        message = "Could not stage the selected files.", recoverable = TRUE
      ))
    }
  }

  empty_check <- processx::run(git_bin, c("-C", repo_root, "diff", "--cached", "--quiet"), error_on_status = FALSE, timeout = 15)
  if (identical(empty_check$status, 0L)) {
    restore_index()
    return(list(
      ok = FALSE, code = "EMPTY_COMMIT",
      message = "The selected files have no changes to save.", recoverable = TRUE
    ))
  }

  msg_file <- tempfile("gitneighbr-commit-")
  on.exit(unlink(msg_file), add = TRUE)
  message_text <- if (is.null(details)) summary else paste0(summary, "\n\n", details)
  writeLines(message_text, msg_file, useBytes = TRUE)
  Sys.chmod(msg_file, "0600")

  commit_result <- processx::run(git_bin, c("-C", repo_root, "commit", "-F", msg_file), error_on_status = FALSE, timeout = 30)
  if (!identical(commit_result$status, 0L)) {
    restore_index()
    return(list(
      ok = FALSE, code = .classify_commit_failure(commit_result$stderr),
      message = "Git rejected this snapshot.", recoverable = TRUE
    ))
  }

  sha <- trimws(processx::run(git_bin, c("-C", repo_root, "rev-parse", "--short", "HEAD"), error_on_status = TRUE, timeout = 15)$stdout)
  list(ok = TRUE, sha = sha, summary = summary)
}
