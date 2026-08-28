#' Read the remote and remote branch name configured for a local branch
#'
#' Deliberately reads `branch.<name>.remote`/`.merge` via `git config`
#' rather than splitting the `origin/main`-shaped shorthand `.git_status()`
#' reports as `upstream`: a remote name is technically permitted to contain
#' `/`, so splitting the shorthand on the first slash could misparse it.
#'
#' @return A list with `remote` and `branch` (both strings), or `NULL` if
#'   `branch` has no upstream configured.
#' @noRd
.git_upstream_info <- function(repo_root, git_bin, branch) {
  remote_result <- processx::run(
    git_bin, c("-C", repo_root, "config", "--get", paste0("branch.", branch, ".remote")),
    error_on_status = FALSE, timeout = 5
  )
  merge_result <- processx::run(
    git_bin, c("-C", repo_root, "config", "--get", paste0("branch.", branch, ".merge")),
    error_on_status = FALSE, timeout = 5
  )
  if (!identical(remote_result$status, 0L) || !identical(merge_result$status, 0L)) {
    return(NULL)
  }
  remote <- trimws(remote_result$stdout)
  merge_ref <- trimws(merge_result$stdout)
  if (!nzchar(remote) || !nzchar(merge_ref)) {
    return(NULL)
  }
  list(remote = remote, branch = sub("^refs/heads/", "", merge_ref))
}

#' Map `git fetch`'s stderr to a stable application error code
#' @noRd
.classify_fetch_failure <- function(stderr_text) {
  text <- tolower(stderr_text %||% "")
  if (grepl("could not resolve host|network is unreachable|connection timed out|temporary failure|could not read from remote|no route to host", text)) {
    return("REMOTE_UNREACHABLE")
  }
  if (grepl("authentication failed|permission denied|could not read username|could not read password|access denied|invalid credentials", text)) {
    return("AUTH_REQUIRED")
  }
  "COMMAND_FAILED"
}

#' Map `git push`'s stderr to a stable application error code
#'
#' Order matters: a protected-branch rejection often also mentions "hook
#' declined" in the same `[remote rejected]` line GitHub sends, so the more
#' specific check runs first.
#' @noRd
.classify_push_failure <- function(stderr_text) {
  text <- tolower(stderr_text %||% "")
  if (grepl("authentication failed|permission denied|could not read username|could not read password|access denied|invalid credentials", text)) {
    return("AUTH_REQUIRED")
  }
  if (grepl("could not resolve host|network is unreachable|connection timed out|temporary failure|could not read from remote|no route to host", text)) {
    return("REMOTE_UNREACHABLE")
  }
  if (grepl("protected branch|gh006", text)) {
    return("PROTECTED_BRANCH")
  }
  if (grepl("large file|exceeds github's file size limit|exceeds file size limit|this exceeds", text)) {
    return("LARGE_FILE_REJECTED")
  }
  if (grepl("hook declined", text)) {
    return("HOOK_FAILED")
  }
  if (grepl("non-fast-forward|fetch first|updates were rejected", text)) {
    return("REMOTE_AHEAD")
  }
  "COMMAND_FAILED"
}

#' Attach `.git_credential_diagnosis()` to a failed fetch/push result, but
#' only when the failure was actually classified as `AUTH_REQUIRED` -- for
#' any other code (e.g. a network error) the diagnosis would be misleading
#' noise about credentials that were never the problem.
#' @noRd
.diagnosis_for_failure <- function(repo_root, git_bin, code, stderr_text) {
  if (!identical(code, "AUTH_REQUIRED")) {
    return(NULL)
  }
  .git_credential_diagnosis(repo_root, git_bin, stderr_text = stderr_text)
}

#' Fetch the configured upstream remote and return refreshed status
#'
#' Implements the `/api/v1/refresh-remote` endpoint: this is a read-only
#' network operation (a fetch updates local remote-tracking refs, never the
#' working tree or the current branch), used by the interface to preview
#' whether a push is safe before offering **Send to GitHub**.
#'
#' @return A list. On success: `ok = TRUE` plus every field `.git_status()`
#'   returns. On failure: `ok = FALSE`, `code`, `message`, `recoverable`.
#' @noRd
.git_refresh_remote <- function(repo_root, git_bin) {
  status <- .git_status(repo_root, git_bin)

  if (status$detached) {
    return(list(
      ok = FALSE, code = "DETACHED_HEAD",
      message = "Check out a branch before checking for updates.", recoverable = FALSE
    ))
  }
  upstream_info <- .git_upstream_info(repo_root, git_bin, status$branch)
  if (is.null(upstream_info)) {
    return(list(
      ok = FALSE, code = "NO_UPSTREAM",
      message = "This branch has no destination configured on GitHub yet.", recoverable = FALSE
    ))
  }

  fetch_args <- c("-C", repo_root, "fetch", upstream_info$remote)
  fetch_result <- processx::run(
    git_bin, fetch_args,
    error_on_status = FALSE, timeout = 60
  )
  if (!identical(fetch_result$status, 0L)) {
    code <- .classify_fetch_failure(fetch_result$stderr)
    return(list(
      ok = FALSE, code = code,
      message = "Could not reach GitHub to check for updates.", recoverable = TRUE,
      advanced = .advanced_block(fetch_args, fetch_result),
      diagnosis = .diagnosis_for_failure(repo_root, git_bin, code, fetch_result$stderr)
    ))
  }

  c(list(ok = TRUE), .git_status(repo_root, git_bin))
}

#' Fetch, verify safety, and push the current branch to its upstream
#'
#' Implements spec Sec 8.3: a fetch always runs first so the safety check
#' reflects the remote's *current* state rather than whatever `gitneighbr`
#' last saw; if the fetch reveals the remote is ahead or the histories have
#' diverged, the push never runs. Only ever pushes the current branch to its
#' own configured upstream ref (`git push <remote> <local>:<remote>`),
#' never `--force`.
#'
#' @return A list. On success: `ok = TRUE`, `remote`, `remote_branch`,
#'   `branch`, `sha`, `pushed_count` (commits that were ahead and are now on
#'   the remote). On failure: `ok = FALSE`, `code`, `message`, `recoverable`.
#' @noRd
.git_push_current_branch <- function(repo_root, git_bin) {
  status <- .git_status(repo_root, git_bin)

  if (status$detached) {
    return(list(
      ok = FALSE, code = "DETACHED_HEAD",
      message = "Check out a branch before sending snapshots.", recoverable = FALSE
    ))
  }
  upstream_info <- .git_upstream_info(repo_root, git_bin, status$branch)
  if (is.null(upstream_info)) {
    return(list(
      ok = FALSE, code = "NO_UPSTREAM",
      message = "This branch has no destination configured on GitHub yet.", recoverable = FALSE
    ))
  }

  fetch_args <- c("-C", repo_root, "fetch", upstream_info$remote)
  fetch_result <- processx::run(
    git_bin, fetch_args,
    error_on_status = FALSE, timeout = 60
  )
  if (!identical(fetch_result$status, 0L)) {
    code <- .classify_fetch_failure(fetch_result$stderr)
    return(list(
      ok = FALSE, code = code,
      message = "Could not check GitHub for newer work before sending.", recoverable = TRUE,
      advanced = .advanced_block(fetch_args, fetch_result),
      diagnosis = .diagnosis_for_failure(repo_root, git_bin, code, fetch_result$stderr)
    ))
  }

  fresh_status <- .git_status(repo_root, git_bin)
  if (fresh_status$ahead > 0L && fresh_status$behind > 0L) {
    return(list(
      ok = FALSE, code = "DIVERGED",
      message = paste(
        "This computer and GitHub both have work the other does not.",
        "Combining them requires a person to choose how the changes fit together."
      ),
      recoverable = FALSE
    ))
  }
  if (fresh_status$behind > 0L) {
    return(list(
      ok = FALSE, code = "REMOTE_AHEAD",
      message = "GitHub has newer work. Get those updates before sending your saved snapshots.",
      recoverable = TRUE
    ))
  }

  refspec <- paste0(status$branch, ":", upstream_info$branch)
  push_args <- c("-C", repo_root, "push", upstream_info$remote, refspec)
  push_result <- processx::run(
    git_bin, push_args,
    error_on_status = FALSE, timeout = 60
  )
  if (!identical(push_result$status, 0L)) {
    code <- .classify_push_failure(push_result$stderr)
    return(list(
      ok = FALSE, code = code,
      message = "GitHub rejected this push.", recoverable = TRUE,
      advanced = .advanced_block(push_args, push_result),
      diagnosis = .diagnosis_for_failure(repo_root, git_bin, code, push_result$stderr)
    ))
  }

  sha <- trimws(processx::run(
    git_bin, c("-C", repo_root, "rev-parse", "--short", "HEAD"),
    error_on_status = TRUE, timeout = 15
  )$stdout)

  list(
    ok = TRUE,
    remote = upstream_info$remote,
    remote_branch = upstream_info$branch,
    branch = status$branch,
    sha = sha,
    pushed_count = fresh_status$ahead
  )
}

#' Map `git merge --ff-only`'s stderr to a stable application error code
#' @noRd
.classify_update_failure <- function(stderr_text) {
  text <- tolower(stderr_text %||% "")
  if (grepl("not possible to fast-forward", text)) {
    return("DIVERGED")
  }
  "COMMAND_FAILED"
}

#' Verify safety and apply a fast-forward-only update to the current branch
#'
#' Implements spec Sec 8.6: the working tree must be clean *before*
#' fetching, since a fast-forward merge writes into the working tree and a
#' dirty tree can never be checked out over safely; dirty trees are refused
#' as `DIRTY_BLOCKS_UPDATE` without ever reaching the network (cleanliness
#' doesn't change from a fetch, so checking against the pre-fetch status is
#' equivalent and saves the round trip). After a successful fetch, diverged
#' histories are refused exactly like `.git_push_current_branch()` refuses
#' them. The update itself always runs as `git merge --ff-only
#' <fully-qualified remote-tracking ref>` -- never `git pull`, which could
#' implicitly rebase or perform a non-fast-forward merge.
#'
#' @return A list. On success: `ok = TRUE`, `remote`, `remote_branch`,
#'   `branch`, `sha`, `updated_count` (commits that were behind and are now
#'   merged in). On failure: `ok = FALSE`, `code`, `message`, `recoverable`.
#' @noRd
.git_update_current_branch <- function(repo_root, git_bin) {
  status <- .git_status(repo_root, git_bin)

  if (status$detached) {
    return(list(
      ok = FALSE, code = "DETACHED_HEAD",
      message = "Check out a branch before getting updates.", recoverable = FALSE
    ))
  }
  upstream_info <- .git_upstream_info(repo_root, git_bin, status$branch)
  if (is.null(upstream_info)) {
    return(list(
      ok = FALSE, code = "NO_UPSTREAM",
      message = "This branch has no destination configured on GitHub yet.", recoverable = FALSE
    ))
  }
  if (status$has_changes) {
    return(list(
      ok = FALSE, code = "DIRTY_BLOCKS_UPDATE",
      message = "Save or clear your unsaved changes before getting updates.", recoverable = TRUE
    ))
  }

  fetch_args <- c("-C", repo_root, "fetch", upstream_info$remote)
  fetch_result <- processx::run(
    git_bin, fetch_args,
    error_on_status = FALSE, timeout = 60
  )
  if (!identical(fetch_result$status, 0L)) {
    code <- .classify_fetch_failure(fetch_result$stderr)
    return(list(
      ok = FALSE, code = code,
      message = "Could not reach GitHub to get updates.", recoverable = TRUE,
      advanced = .advanced_block(fetch_args, fetch_result),
      diagnosis = .diagnosis_for_failure(repo_root, git_bin, code, fetch_result$stderr)
    ))
  }

  fresh_status <- .git_status(repo_root, git_bin)
  if (fresh_status$ahead > 0L && fresh_status$behind > 0L) {
    return(list(
      ok = FALSE, code = "DIVERGED",
      message = paste(
        "This computer and GitHub both have work the other does not.",
        "Combining them requires a person to choose how the changes fit together."
      ),
      recoverable = FALSE
    ))
  }

  remote_ref <- paste0("refs/remotes/", upstream_info$remote, "/", upstream_info$branch)
  merge_args <- c("-C", repo_root, "merge", "--ff-only", remote_ref)
  merge_result <- processx::run(
    git_bin, merge_args,
    error_on_status = FALSE, timeout = 30
  )
  if (!identical(merge_result$status, 0L)) {
    return(list(
      ok = FALSE, code = .classify_update_failure(merge_result$stderr),
      message = "Could not safely apply that update.", recoverable = TRUE,
      advanced = .advanced_block(merge_args, merge_result)
    ))
  }

  sha <- trimws(processx::run(
    git_bin, c("-C", repo_root, "rev-parse", "--short", "HEAD"),
    error_on_status = TRUE, timeout = 15
  )$stdout)

  list(
    ok = TRUE,
    remote = upstream_info$remote,
    remote_branch = upstream_info$branch,
    branch = status$branch,
    sha = sha,
    updated_count = fresh_status$behind
  )
}
