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
