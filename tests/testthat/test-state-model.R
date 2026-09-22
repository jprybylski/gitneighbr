local_git_repo <- function(env = parent.frame()) {
  git <- unname(Sys.which("git"))
  skip_if(!nzchar(git), "git not available")

  dir <- withr::local_tempdir(.local_envir = env)
  run <- function(...) processx::run(git, c("-C", dir, ...), error_on_status = TRUE)
  run("init", "-q", "-b", "main")
  run("config", "user.email", "test@example.com")
  run("config", "user.name", "Test")

  list(dir = dir, git = git, run = run)
}

new_session_state <- function() {
  state <- new.env(parent = emptyenv())
  state$version <- 0L
  state$last_snapshot <- NULL
  state$auth_required <- FALSE
  state$pending_tags <- character()
  state$pushed_tags <- character()
  state
}

test_that(".primary_state prioritizes CONFLICTED over upstream states", {
  status <- list(
    detached = FALSE, upstream = "origin/main", ahead = 0L, behind = 0L,
    has_changes = TRUE, conflicted_count = 1L
  )
  expect_equal(.primary_state(status, git_ok = TRUE), "CONFLICTED")
})

test_that(".primary_state surfaces AUTH_REQUIRED only when signaled, below CONFLICTED", {
  status <- list(
    detached = FALSE, upstream = "origin/main", ahead = 1L, behind = 0L,
    has_changes = FALSE, conflicted_count = 0L
  )
  expect_equal(.primary_state(status, git_ok = TRUE, auth_required = TRUE), "AUTH_REQUIRED")
  expect_equal(.primary_state(status, git_ok = TRUE, auth_required = FALSE), "LOCAL_ONLY")

  conflicted <- status
  conflicted$conflicted_count <- 1L
  expect_equal(.primary_state(conflicted, git_ok = TRUE, auth_required = TRUE), "CONFLICTED")
})

test_that(".primary_state covers every remaining branch of the state machine", {
  base <- list(detached = FALSE, upstream = "origin/main", ahead = 0L, behind = 0L, has_changes = FALSE, conflicted_count = 0L)

  expect_equal(.primary_state(NULL, git_ok = TRUE), "NOT_REPOSITORY")
  expect_equal(.primary_state(base, git_ok = FALSE), "GIT_UNAVAILABLE")

  detached <- base
  detached$detached <- TRUE
  expect_equal(.primary_state(detached, git_ok = TRUE), "DETACHED_HEAD")

  no_upstream <- base
  no_upstream$upstream <- NULL
  expect_equal(.primary_state(no_upstream, git_ok = TRUE), "NO_UPSTREAM")

  behind_clean <- base
  behind_clean$behind <- 1L
  expect_equal(.primary_state(behind_clean, git_ok = TRUE), "REMOTE_ONLY_CLEAN")

  behind_dirty <- behind_clean
  behind_dirty$has_changes <- TRUE
  expect_equal(.primary_state(behind_dirty, git_ok = TRUE), "REMOTE_ONLY_DIRTY")

  expect_equal(.primary_state(base, git_ok = TRUE), "READY")

  changes_only <- base
  changes_only$has_changes <- TRUE
  expect_equal(.primary_state(changes_only, git_ok = TRUE), "CHANGES_ONLY")
})

test_that(".git_in_progress_operation detects a cherry-pick or revert in progress via marker files", {
  repo <- local_git_repo()
  git_dir <- file.path(repo$dir, ".git")

  writeLines("deadbeef", file.path(git_dir, "CHERRY_PICK_HEAD"))
  expect_equal(.git_in_progress_operation(repo$dir, repo$git), "cherry-pick")
  file.remove(file.path(git_dir, "CHERRY_PICK_HEAD"))

  writeLines("deadbeef", file.path(git_dir, "REVERT_HEAD"))
  expect_equal(.git_in_progress_operation(repo$dir, repo$git), "revert")
})

test_that(".git_status reports conflicted_count for unmerged paths without double-counting", {
  repo <- local_git_repo()
  writeLines("base", file.path(repo$dir, "file.txt"))
  repo$run("add", "file.txt")
  repo$run("commit", "-q", "-m", "base")

  repo$run("checkout", "-q", "-b", "side")
  writeLines("side change", file.path(repo$dir, "file.txt"))
  repo$run("commit", "-q", "-am", "side change")

  repo$run("checkout", "-q", "main")
  writeLines("main change", file.path(repo$dir, "file.txt"))
  repo$run("commit", "-q", "-am", "main change")

  processx::run(repo$git, c("-C", repo$dir, "merge", "side"), error_on_status = FALSE)

  status <- .git_status(repo$dir, repo$git)
  expect_equal(status$conflicted_count, 1L)
  expect_equal(status$staged_count, 0L)
  expect_equal(status$unstaged_count, 0L)
  expect_true(status$has_changes)
  expect_equal(.primary_state(status, git_ok = TRUE), "CONFLICTED")
})

test_that(".git_in_progress_operation is NULL for a clean repo", {
  repo <- local_git_repo()
  writeLines("hello", file.path(repo$dir, "file.txt"))
  repo$run("add", "file.txt")
  repo$run("commit", "-q", "-m", "initial commit")

  expect_null(.git_in_progress_operation(repo$dir, repo$git))
})

test_that(".git_in_progress_operation detects an in-progress merge", {
  repo <- local_git_repo()
  writeLines("base", file.path(repo$dir, "file.txt"))
  repo$run("add", "file.txt")
  repo$run("commit", "-q", "-m", "base")

  repo$run("checkout", "-q", "-b", "side")
  writeLines("side change", file.path(repo$dir, "file.txt"))
  repo$run("commit", "-q", "-am", "side change")

  repo$run("checkout", "-q", "main")
  writeLines("main change", file.path(repo$dir, "file.txt"))
  repo$run("commit", "-q", "-am", "main change")

  processx::run(repo$git, c("-C", repo$dir, "merge", "side"), error_on_status = FALSE)

  expect_equal(.git_in_progress_operation(repo$dir, repo$git), "merge")
})

test_that(".git_in_progress_operation detects an in-progress rebase", {
  repo <- local_git_repo()
  writeLines("base", file.path(repo$dir, "file.txt"))
  repo$run("add", "file.txt")
  repo$run("commit", "-q", "-m", "base")

  repo$run("checkout", "-q", "-b", "feature")
  writeLines("feature change", file.path(repo$dir, "file.txt"))
  repo$run("commit", "-q", "-am", "feature commit")

  repo$run("checkout", "-q", "main")
  writeLines("main change", file.path(repo$dir, "file.txt"))
  repo$run("commit", "-q", "-am", "main commit")

  repo$run("checkout", "-q", "feature")
  processx::run(repo$git, c("-C", repo$dir, "rebase", "main"), error_on_status = FALSE)

  expect_equal(.git_in_progress_operation(repo$dir, repo$git), "rebase")
})

test_that(".status_notices reports STALE_CHANGES only once unsaved changes are old enough", {
  repo <- local_git_repo()
  old_date <- format(Sys.time() - 20 * 86400, "%Y-%m-%dT%H:%M:%S")
  withr::with_envvar(
    c(GIT_AUTHOR_DATE = old_date, GIT_COMMITTER_DATE = old_date),
    {
      writeLines("hello", file.path(repo$dir, "file.txt"))
      repo$run("add", "file.txt")
      repo$run("commit", "-q", "-m", "old commit")
    }
  )
  writeLines("changed", file.path(repo$dir, "file.txt"))

  status <- .git_status(repo$dir, repo$git)
  codes <- vapply(.status_notices(repo$dir, repo$git, status, new_session_state()), `[[`, character(1), "code")
  expect_true("STALE_CHANGES" %in% codes)
})

test_that(".status_notices omits STALE_CHANGES for recent changes", {
  repo <- local_git_repo()
  writeLines("hello", file.path(repo$dir, "file.txt"))
  repo$run("add", "file.txt")
  repo$run("commit", "-q", "-m", "recent commit")
  writeLines("changed", file.path(repo$dir, "file.txt"))

  status <- .git_status(repo$dir, repo$git)
  codes <- vapply(.status_notices(repo$dir, repo$git, status, new_session_state()), `[[`, character(1), "code")
  expect_false("STALE_CHANGES" %in% codes)
})

test_that(".has_ignored_files, .git_lfs_active, .days_since_last_commit, and .git_tags_at_head report their empty/failure cases", {
  git <- unname(Sys.which("git"))
  skip_if(!nzchar(git), "git not available")
  false_bin <- unname(Sys.which("false"))
  skip_if(!nzchar(false_bin), "no 'false' binary available")

  dir <- withr::local_tempdir()
  processx::run(git, c("-C", dir, "init", "-q", "-b", "main"), error_on_status = TRUE)

  expect_false(.has_ignored_files(dir, false_bin))
  expect_false(.git_lfs_active(dir, false_bin))
  expect_null(.days_since_last_commit(dir, git)) # no commits yet
  expect_equal(.git_tags_at_head(dir, false_bin), character())
})

test_that(".git_lfs_active detects a committed .gitattributes LFS filter pattern", {
  repo <- local_git_repo()
  writeLines("*.bin filter=lfs diff=lfs merge=lfs -text", file.path(repo$dir, ".gitattributes"))
  expect_true(.git_lfs_active(repo$dir, repo$git))
})

test_that(".status_notices reports an active commit hook, signing, LFS, and a submodule marker", {
  repo <- local_git_repo()
  writeLines("hello", file.path(repo$dir, "file.txt"))
  repo$run("add", "file.txt")
  repo$run("commit", "-q", "-m", "initial commit")

  hooks_dir <- file.path(repo$dir, ".git", "hooks")
  writeLines("#!/bin/sh\nexit 0", file.path(hooks_dir, "pre-commit"))
  Sys.chmod(file.path(hooks_dir, "pre-commit"), "0755")
  repo$run("config", "commit.gpgsign", "true")
  writeLines("*.bin filter=lfs diff=lfs merge=lfs -text", file.path(repo$dir, ".gitattributes"))
  writeLines("[submodule \"x\"]", file.path(repo$dir, ".gitmodules"))

  status <- .git_status(repo$dir, repo$git)
  notices <- .status_notices(repo$dir, repo$git, status, new_session_state())
  codes <- vapply(notices, `[[`, character(1), "code")
  expect_true("COMMIT_HOOK_ACTIVE" %in% codes)
  expect_true("SIGNING_ENABLED" %in% codes)
  expect_true("LFS_ACTIVE" %in% codes)
  expect_true("SUBMODULE_PRESENT" %in% codes)
})

test_that(".status_notices reports POLICY_INVALID for a malformed policy file, and POLICY_PR_REQUIRED otherwise", {
  repo <- local_git_repo()
  writeLines("hello", file.path(repo$dir, "file.txt"))
  repo$run("add", "file.txt")
  repo$run("commit", "-q", "-m", "initial commit")

  writeLines("{ not valid json", file.path(repo$dir, ".gitneighbr.json"))
  status <- .git_status(repo$dir, repo$git)
  invalid_codes <- vapply(.status_notices(repo$dir, repo$git, status, new_session_state()), `[[`, character(1), "code")
  expect_true("POLICY_INVALID" %in% invalid_codes)

  writeLines('{"require_pull_request": true}', file.path(repo$dir, ".gitneighbr.json"))
  pr_codes <- vapply(.status_notices(repo$dir, repo$git, status, new_session_state()), `[[`, character(1), "code")
  expect_true("POLICY_PR_REQUIRED" %in% pr_codes)
  expect_false("POLICY_INVALID" %in% pr_codes)
})

test_that(".status_notices reports untracked files and a non-GitHub remote", {
  repo <- local_git_repo()
  writeLines("hello", file.path(repo$dir, "file.txt"))
  repo$run("add", "file.txt")
  repo$run("commit", "-q", "-m", "initial commit")
  writeLines("new", file.path(repo$dir, "new.txt"))
  repo$run("remote", "add", "origin", "https://example.com/not-github/repo.git")

  status <- .git_status(repo$dir, repo$git)
  notices <- .status_notices(repo$dir, repo$git, status, new_session_state())
  codes <- vapply(notices, `[[`, character(1), "code")

  expect_true("UNTRACKED_PRESENT" %in% codes)
  expect_true("REMOTE_NOT_GITHUB" %in% codes)
})

test_that(".status_notices reports pending and pushed tags from session bookkeeping", {
  repo <- local_git_repo()
  writeLines("hello", file.path(repo$dir, "file.txt"))
  repo$run("add", "file.txt")
  repo$run("commit", "-q", "-m", "initial commit")
  repo$run("tag", "-a", "v1.0.0", "-m", "v1.0.0")

  status <- .git_status(repo$dir, repo$git)

  pending_state <- new_session_state()
  pending_state$pending_tags <- "v1.0.0"
  pending_codes <- vapply(
    .status_notices(repo$dir, repo$git, status, pending_state), `[[`, character(1), "code"
  )
  expect_true("LOCAL_ONLY_TAG" %in% pending_codes)
  expect_false("PUSHED_TAG_AT_HEAD" %in% pending_codes)

  pushed_state <- new_session_state()
  pushed_state$pushed_tags <- "v1.0.0"
  pushed_codes <- vapply(
    .status_notices(repo$dir, repo$git, status, pushed_state), `[[`, character(1), "code"
  )
  expect_true("PUSHED_TAG_AT_HEAD" %in% pushed_codes)
  expect_false("LOCAL_ONLY_TAG" %in% pushed_codes)
})

test_that(".status_payload only bumps status_version when the snapshot actually changes", {
  repo <- local_git_repo()
  writeLines("hello", file.path(repo$dir, "file.txt"))
  repo$run("add", "file.txt")
  repo$run("commit", "-q", "-m", "initial commit")

  state <- new_session_state()
  first <- .status_payload(repo$dir, repo$git, state)
  second <- .status_payload(repo$dir, repo$git, state)
  expect_equal(first$version, second$version)

  writeLines("changed", file.path(repo$dir, "file.txt"))
  third <- .status_payload(repo$dir, repo$git, state)
  expect_gt(third$version, second$version)
  expect_equal(third$data$unstaged_count, 1L)
})

test_that(".status_payload reports NOT_REPOSITORY instead of throwing for a non-repo path", {
  git <- unname(Sys.which("git"))
  skip_if(!nzchar(git), "git not available")
  dir <- withr::local_tempdir()

  payload <- .status_payload(dir, git, new_session_state())
  expect_equal(payload$data$primary_state, "NOT_REPOSITORY")
})
