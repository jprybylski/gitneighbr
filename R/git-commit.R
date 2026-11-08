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
  if (grepl("please tell me who you are|unable to auto-detect email|empty ident name", text)) {
    return("IDENTITY_MISSING")
  }
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

#' Create one commit from exactly the user's selected paths
#'
#' The commit is built on a temporary index (`GIT_INDEX_FILE`) that starts
#' from `HEAD` and gets only the selected paths (and a selected rename's old
#' path), so whatever else the user had staged -- other files, or partial
#' staging -- is never included and never disturbed. Afterwards only the
#' selected paths are refreshed in the real index to match the new commit.
#'
#' A standalone session commits with `git commit`, so the user's own hooks
#' and signing still apply. A managed session uses `write-tree`,
#' `commit-tree` and `update-ref HEAD <new> <old>`: if `HEAD` moved after the
#' list was shown, the update is refused and nothing changes.
#'
#' Every selected path is re-checked at commit time: it must still be a
#' pending change in a fresh status read, stay inside the repository after
#' resolving any linked folders, and not be inside `.git/`.
#'
#' @param selected_paths Character vector of repository-relative paths the
#'   user selected, matched against a fresh `.git_status_entries()` read of
#'   the repository (never trusted at face value: a path that no longer
#'   matches a real pending change is treated as `STATE_CHANGED`, and a
#'   repository with *any* unresolved conflicts is rejected as
#'   `CONFLICTS_PRESENT`).
#' @param summary Required 3-72 character commit subject.
#' @param details Optional longer commit body.
#' @param expected_head If not `NULL`, the commit `HEAD` must still be at
#'   (`""` for an unborn branch); otherwise `STATE_CHANGED`.
#' @return A list. On success: `ok = TRUE`, `sha`, `summary`. On failure:
#'   `ok = FALSE`, `code`, `message`, `recoverable`.
#' @noRd
.git_commit_selected <- function(repo_root, git_bin, selected_paths, summary, details = NULL, expected_head = NULL) {
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

  # A selected rename brings its old path along, so the commit records the
  # move rather than a copy.
  commit_paths <- unique(unlist(lapply(by_path[selected_paths], .commit_entry_paths)))
  for (path in commit_paths) {
    if (!.commit_path_is_safe(repo_root, path)) {
      return(list(
        ok = FALSE, code = "PATH_REJECTED",
        message = "A selected file is outside this folder or inside Git's own data, so nothing was saved.",
        recoverable = FALSE
      ))
    }
  }

  head <- .git_exec(git_bin, c("-C", repo_root, "rev-parse", "--verify", "-q", "HEAD"), error_on_status = FALSE, timeout = 15)
  old_head <- if (identical(head$status, 0L)) trimws(head$stdout) else ""
  if (!is.null(expected_head) && !identical(old_head, expected_head)) {
    return(list(
      ok = FALSE, code = "STATE_CHANGED",
      message = "The repository changed since this list was shown. Refresh and try again.",
      recoverable = TRUE
    ))
  }

  index_file <- tempfile("gitneighbr-index-")
  on.exit(unlink(index_file), add = TRUE)
  index_env <- c(GIT_INDEX_FILE = index_file)
  step <- function(args, message, timeout = 15) {
    result <- .git_exec(git_bin, c("-C", repo_root, args), error_on_status = FALSE, timeout = timeout, env = index_env)
    if (!identical(result$status, 0L)) {
      stop(structure(
        class = c("gitneighbr_commit_step", "error", "condition"),
        list(message = message, call = NULL, args = c("-C", repo_root, args), result = result)
      ))
    }
    result
  }

  message_file <- tempfile("gitneighbr-commit-")
  on.exit(unlink(message_file), add = TRUE)
  message_text <- if (is.null(details)) summary else paste0(summary, "\n\n", details)
  writeLines(message_text, message_file, useBytes = TRUE)
  Sys.chmod(message_file, "0600")

  outcome <- tryCatch(
    {
      step(if (nzchar(old_head)) c("read-tree", old_head) else c("read-tree", "--empty"), "Could not read the current snapshot.")
      step(c("add", "-A", "--", commit_paths), "Could not stage the selected files.")
      tree <- trimws(step("write-tree", "Could not record the selected files.")$stdout)
      old_tree <- if (nzchar(old_head)) {
        trimws(step(c("rev-parse", paste0(old_head, "^{tree}")), "Could not read the current snapshot.")$stdout)
      } else {
        ""
      }
      if (identical(tree, old_tree)) {
        list(
          ok = FALSE, code = "EMPTY_COMMIT",
          message = "The selected files have no changes to save.", recoverable = TRUE
        )
      } else if (.is_managed_git(git_bin)) {
        parent <- if (nzchar(old_head)) c("-p", old_head)
        sha <- trimws(step(c("commit-tree", tree, parent, "-F", message_file), "Git rejected this snapshot.", timeout = 30)$stdout)
        moved <- .git_exec(
          git_bin, c("-C", repo_root, "update-ref", "-m", paste("commit:", summary), "HEAD", sha, old_head),
          error_on_status = FALSE, timeout = 15
        )
        if (!identical(moved$status, 0L)) {
          list(
            ok = FALSE, code = "STATE_CHANGED",
            message = "The repository changed while saving. Nothing was saved; refresh and try again.",
            recoverable = TRUE
          )
        } else {
          list(ok = TRUE, sha = sha)
        }
      } else {
        step(c("commit", "-q", "-F", message_file), "Git rejected this snapshot.", timeout = 30)
        list(ok = TRUE, sha = trimws(step(c("rev-parse", "HEAD"), "Could not read the new snapshot.")$stdout))
      }
    },
    gitneighbr_commit_step = function(e) {
      code <- if (identical(e$message, "Git rejected this snapshot.")) .classify_commit_failure(e$result$stderr) else "COMMAND_FAILED"
      list(ok = FALSE, code = code, message = e$message, recoverable = TRUE, advanced = .advanced_block(e$args, e$result))
    }
  )
  if (!isTRUE(outcome$ok)) {
    return(outcome)
  }

  # Bring only the committed paths in the real index up to the new commit;
  # anything else the user had staged stays exactly as it was.
  .git_exec(git_bin, c("-C", repo_root, "reset", "-q", "--", commit_paths), error_on_status = FALSE, timeout = 15)
  short <- .git_exec(git_bin, c("-C", repo_root, "rev-parse", "--short", outcome$sha), error_on_status = FALSE, timeout = 15)
  list(ok = TRUE, sha = trimws(short$stdout), summary = summary)
}

#' May this path be committed?
#'
#' Lexically inside the repository, not Git's own data, and (when its folder
#' exists) inside the repository after resolving linked folders.
#' @noRd
.commit_path_is_safe <- function(repo_root, path) {
  if (!.validate_repo_relative_path(repo_root, path)) {
    return(FALSE)
  }
  first <- strsplit(gsub("\\\\", "/", path), "/", fixed = TRUE)[[1]][[1]]
  if (identical(tolower(first), ".git")) {
    return(FALSE)
  }
  parent <- dirname(file.path(repo_root, path))
  if (dir.exists(parent)) {
    root <- paste0(.real_path(repo_root), "/")
    real_parent <- paste0(.real_path(parent), "/")
    if (!startsWith(real_parent, root)) {
      return(FALSE)
    }
  }
  TRUE
}
