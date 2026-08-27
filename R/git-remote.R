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

  fetch_result <- processx::run(
    git_bin, c("-C", repo_root, "fetch", upstream_info$remote),
    error_on_status = FALSE, timeout = 60
  )
  if (!identical(fetch_result$status, 0L)) {
    return(list(
      ok = FALSE, code = .classify_fetch_failure(fetch_result$stderr),
      message = "Could not reach GitHub to check for updates.", recoverable = TRUE
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

  fetch_result <- processx::run(
    git_bin, c("-C", repo_root, "fetch", upstream_info$remote),
    error_on_status = FALSE, timeout = 60
  )
  if (!identical(fetch_result$status, 0L)) {
    return(list(
      ok = FALSE, code = .classify_fetch_failure(fetch_result$stderr),
      message = "Could not check GitHub for newer work before sending.", recoverable = TRUE
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
  push_result <- processx::run(
    git_bin, c("-C", repo_root, "push", upstream_info$remote, refspec),
    error_on_status = FALSE, timeout = 60
  )
  if (!identical(push_result$status, 0L)) {
    return(list(
      ok = FALSE, code = .classify_push_failure(push_result$stderr),
      message = "GitHub rejected this push.", recoverable = TRUE
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
