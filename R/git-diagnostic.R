#' Run one read-only Git command for a diagnostic report
#'
#' Captures the exact argv, exit status, and sanitized stdout/stderr, in
#' the same shape `.advanced_block()` uses for a failed mutating command,
#' so the frontend can render both with one component.
#' @noRd
.diagnostic_command <- function(repo_root, git_bin, args, timeout = 15) {
  result <- tryCatch(
    processx::run(git_bin, c("-C", repo_root, args), error_on_status = FALSE, timeout = timeout),
    error = function(e) list(status = NA_integer_, stdout = "", stderr = conditionMessage(e))
  )
  list(
    command = .sanitize_git_output(paste(c("git", args), collapse = " ")),
    exit_status = result$status %||% NA_integer_,
    stdout = .sanitize_git_output(result$stdout %||% ""),
    stderr = .sanitize_git_output(result$stderr %||% "")
  )
}

#' Build an exportable diagnostic report for conflict/divergence handoff
#'
#' Implements the "improved conflict handoff" roadmap item (spec Sec 22,
#' issue #21): rather than leaving a `CONFLICTED` or `DIVERGED` user with
#' only the safe-refusal message (spec Sec 5.3), re-runs a small set of
#' already-read-only Git commands and packages what they show so the user
#' can hand it to someone more experienced. Every command's stdout/stderr
#' passes through `.sanitize_git_output()` first, matching the "Advanced
#' details" disclosure's existing secret-redaction guarantee (spec Sec
#' 5.6); no merge, rebase, or reset is ever attempted.
#'
#' @param repo_root Canonical path to the Git working tree root.
#' @param git_bin Path to the `git` executable.
#' @param session_state The per-session environment `.build_api()` holds;
#'   only `auth_required` is read here, to keep `primary_state` consistent
#'   with what `/api/v1/status` last reported.
#' @return A list: `generated_at` (ISO 8601 string), `primary_state`,
#'   `branch`, `upstream`, `ahead`, `behind`, `conflicted_files` (character
#'   vector), and `commands` (a list of `list(command, exit_status, stdout,
#'   stderr)`).
#' @noRd
.diagnostic_report <- function(repo_root, git_bin, session_state) {
  status <- .git_status(repo_root, git_bin)
  state <- .primary_state(status, git_ok = TRUE, auth_required = isTRUE(session_state$auth_required))

  commands <- list(.diagnostic_command(repo_root, git_bin, c("status", "--porcelain=v2", "--branch")))

  conflicted_files <- character()
  if ((status$conflicted_count %||% 0L) > 0L) {
    conflict_cmd <- .diagnostic_command(repo_root, git_bin, c("diff", "--name-only", "--diff-filter=U"))
    commands[[length(commands) + 1L]] <- conflict_cmd
    conflicted_files <- strsplit(trimws(conflict_cmd$stdout), "\n", fixed = TRUE)[[1]]
    conflicted_files <- conflicted_files[nzchar(conflicted_files)]
  }

  if (!is.null(status$upstream)) {
    if ((status$ahead %||% 0L) > 0L) {
      commands[[length(commands) + 1L]] <- .diagnostic_command(
        repo_root, git_bin, c("log", "--oneline", "-n", "10", paste0(status$upstream, "..HEAD"))
      )
    }
    if ((status$behind %||% 0L) > 0L) {
      commands[[length(commands) + 1L]] <- .diagnostic_command(
        repo_root, git_bin, c("log", "--oneline", "-n", "10", paste0("HEAD..", status$upstream))
      )
    }
  }

  list(
    generated_at = strftime(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
    primary_state = state,
    branch = status$branch,
    upstream = status$upstream,
    ahead = status$ahead %||% 0L,
    behind = status$behind %||% 0L,
    conflicted_files = conflicted_files,
    commands = commands
  )
}
