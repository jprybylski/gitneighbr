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

#' Build state-specific manual recovery guidance (spec Sec 13.3, issue #30)
#'
#' Returns copy-only guidance for a person to type into their own terminal
#' - every entry in each block's `commands` is a plain display string,
#' never passed to `processx` or executed by gitneighbr. This is how
#' gitneighbr offers a way forward for `CONFLICTED`/`DIVERGED` without
#' crossing the non-negotiable line: it still never merges, rebases,
#' resets, or force pushes on the user's behalf.
#'
#' @param state The primary state, as returned by `.primary_state()`.
#' @param in_progress_operation One of `"merge"`, `"rebase"`,
#'   `"cherry-pick"`, `"revert"`, or `NULL`, as returned by
#'   `.git_in_progress_operation()`.
#' @param upstream The upstream ref name (e.g. `"origin/main"`), or `NULL`.
#' @return A list of `list(heading, description, commands, risk)` blocks;
#'   empty list if `state` has no recovery guidance to offer.
#' @noRd
.recovery_commands <- function(state, in_progress_operation, upstream) {
  block <- function(heading, description, commands, risk) {
    # `as.list()` keeps a single-command block an array in the JSON
    # response too - the server's serializer auto-unboxes length-1
    # atomic vectors to a bare scalar (spec Sec 11... see server.R).
    list(heading = heading, description = description, commands = as.list(commands), risk = risk)
  }

  if (identical(state, "CONFLICTED")) {
    if (identical(in_progress_operation, "merge")) {
      return(list(
        block(
          "Finish the merge",
          "Open each conflicted file, resolve the conflict markers, then stage and commit.",
          c("git status", "# edit each conflicted file", "git add <file>", "git commit"),
          "Committing finishes the merge. This only affects your local copy until you send it."
        ),
        block(
          "Cancel the merge",
          "Go back to how things were right before the merge started.",
          c("git merge --abort"),
          "Discards any conflict-resolution edits you've made so far, but does not touch commits you already saved."
        )
      ))
    }
    if (identical(in_progress_operation, "rebase")) {
      return(list(
        block(
          "Continue the rebase",
          "Resolve the conflict markers in each file, stage them, then continue.",
          c("git status", "# edit each conflicted file", "git add <file>", "git rebase --continue"),
          "Rebasing rewrites your local commits' hashes. If this branch is already on GitHub and shared with others, check with them before sending it again."
        ),
        block(
          "Cancel the rebase",
          "Go back to how things were right before the rebase started.",
          c("git rebase --abort"),
          "Discards any conflict-resolution edits you've made so far, but does not touch commits you already saved."
        )
      ))
    }
    return(list(block(
      "Resolve the conflict",
      "Open each conflicted file, resolve the conflict markers, then stage and commit.",
      c("git status", "# edit each conflicted file", "git add <file>", "git commit"),
      "This only affects your local copy until you send it."
    )))
  }

  if (identical(state, "DIVERGED") && !is.null(upstream)) {
    return(list(
      block(
        "Combine with a merge",
        "Keeps both histories and adds one merge commit. If any files were changed on both sides, Git will ask you to resolve conflicts.",
        c(paste("git merge", upstream)),
        "Safe for a shared branch - it never rewrites commits that already exist."
      ),
      block(
        "Combine with a rebase",
        "Replays your local commits on top of GitHub's, without a merge commit. If any files were changed on both sides, Git will ask you to resolve conflicts.",
        c(paste("git rebase", upstream)),
        "Rewrites your local commits' hashes. Do not force-push the result if this branch is already shared with others - check with them first."
      )
    ))
  }

  list()
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
#'   vector), `in_progress_operation` (see `.git_in_progress_operation()`),
#'   `recovery` (see `.recovery_commands()` - copy-only guidance, nothing
#'   here is ever executed), and `commands` (a list of
#'   `list(command, exit_status, stdout, stderr)`).
#' @noRd
.diagnostic_report <- function(repo_root, git_bin, session_state) {
  status <- .git_status(repo_root, git_bin)
  state <- .primary_state(status, git_ok = TRUE, auth_required = isTRUE(session_state$auth_required))
  in_progress_operation <- .git_in_progress_operation(repo_root, git_bin)

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
    in_progress_operation = in_progress_operation,
    recovery = .recovery_commands(state, in_progress_operation, status$upstream),
    commands = commands
  )
}
