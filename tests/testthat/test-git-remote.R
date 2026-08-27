local_repo_with_remote <- function(env = parent.frame()) {
  git <- unname(Sys.which("git"))
  skip_if(!nzchar(git), "git not available")

  remote_dir <- withr::local_tempdir(.local_envir = env)
  processx::run(git, c("init", "-q", "--bare", "-b", "main", remote_dir), error_on_status = TRUE)

  origin_dir <- withr::local_tempdir(.local_envir = env)
  origin_run <- function(...) processx::run(git, c("-C", origin_dir, ...), error_on_status = TRUE)
  origin_run("init", "-q", "-b", "main")
  origin_run("config", "user.email", "seed@example.com")
  origin_run("config", "user.name", "Seed")
  writeLines("seed", file.path(origin_dir, "seed.txt"))
  origin_run("add", "seed.txt")
  origin_run("commit", "-q", "-m", "seed commit")
  origin_run("remote", "add", "origin", remote_dir)
  origin_run("push", "-q", "-u", "origin", "main")

  dir <- withr::local_tempdir(.local_envir = env)
  processx::run(git, c("clone", "-q", remote_dir, dir), error_on_status = TRUE)
  run <- function(...) processx::run(git, c("-C", dir, ...), error_on_status = TRUE)
  run("config", "user.email", "test@example.com")
  run("config", "user.name", "Test")

  list(dir = dir, git = git, run = run, remote_dir = remote_dir, origin_dir = origin_dir, origin_run = origin_run)
}

test_that(".git_upstream_info reads the configured remote and branch", {
  repo <- local_repo_with_remote()
  info <- .git_upstream_info(repo$dir, repo$git, "main")
  expect_equal(info$remote, "origin")
  expect_equal(info$branch, "main")
})

test_that(".git_upstream_info returns NULL when there is no upstream", {
  git <- unname(Sys.which("git"))
  skip_if(!nzchar(git), "git not available")
  dir <- withr::local_tempdir()
  run <- function(...) processx::run(git, c("-C", dir, ...), error_on_status = TRUE)
  run("init", "-q", "-b", "main")

  expect_null(.git_upstream_info(dir, git, "main"))
})

test_that(".classify_fetch_failure and .classify_push_failure map common Git errors", {
  expect_equal(.classify_fetch_failure("fatal: could not resolve host: github.com"), "REMOTE_UNREACHABLE")
  expect_equal(.classify_fetch_failure("fatal: Authentication failed for 'https://...'"), "AUTH_REQUIRED")
  expect_equal(.classify_fetch_failure("fatal: something else broke"), "COMMAND_FAILED")

  expect_equal(.classify_push_failure("remote: error: GH006: Protected branch update failed"), "PROTECTED_BRANCH")
  expect_equal(.classify_push_failure("! [remote rejected] main -> main (hook declined)"), "HOOK_FAILED")
  expect_equal(.classify_push_failure("! [rejected] main -> main (non-fast-forward)"), "REMOTE_AHEAD")
  expect_equal(.classify_push_failure("remote: this exceeds GitHub's file size limit"), "LARGE_FILE_REJECTED")
  expect_equal(.classify_push_failure("fatal: Authentication failed"), "AUTH_REQUIRED")
})

test_that(".git_refresh_remote fetches and reports an up-to-date branch as clean", {
  repo <- local_repo_with_remote()
  result <- .git_refresh_remote(repo$dir, repo$git)
  expect_true(result$ok)
  expect_equal(result$ahead, 0L)
  expect_equal(result$behind, 0L)
})

test_that(".git_refresh_remote reports the branch as behind after a remote-only commit", {
  repo <- local_repo_with_remote()
  writeLines("more", file.path(repo$origin_dir, "more.txt"))
  repo$origin_run("add", "more.txt")
  repo$origin_run("commit", "-q", "-m", "remote-only commit")
  repo$origin_run("push", "-q", "origin", "main")

  result <- .git_refresh_remote(repo$dir, repo$git)
  expect_true(result$ok)
  expect_equal(result$behind, 1L)
})

test_that(".git_refresh_remote fails with NO_UPSTREAM when the branch has none", {
  git <- unname(Sys.which("git"))
  skip_if(!nzchar(git), "git not available")
  dir <- withr::local_tempdir()
  run <- function(...) processx::run(git, c("-C", dir, ...), error_on_status = TRUE)
  run("init", "-q", "-b", "main")
  writeLines("x", file.path(dir, "x.txt"))
  run("add", "x.txt")
  run("commit", "-q", "-m", "initial")

  result <- .git_refresh_remote(dir, git)
  expect_false(result$ok)
  expect_equal(result$code, "NO_UPSTREAM")
})

test_that(".git_push_current_branch pushes a local-only commit", {
  repo <- local_repo_with_remote()
  writeLines("local change", file.path(repo$dir, "local.txt"))
  repo$run("add", "local.txt")
  repo$run("commit", "-q", "-m", "local commit")

  result <- .git_push_current_branch(repo$dir, repo$git)
  expect_true(result$ok)
  expect_equal(result$remote, "origin")
  expect_equal(result$remote_branch, "main")
  expect_equal(result$pushed_count, 1L)

  remote_log <- processx::run(repo$git, c("-C", repo$remote_dir, "log", "-1", "--format=%s", "main"), error_on_status = TRUE)
  expect_equal(trimws(remote_log$stdout), "local commit")
})

test_that(".git_push_current_branch refuses to push when the remote is ahead", {
  repo <- local_repo_with_remote()
  writeLines("more", file.path(repo$origin_dir, "more.txt"))
  repo$origin_run("add", "more.txt")
  repo$origin_run("commit", "-q", "-m", "remote-only commit")
  repo$origin_run("push", "-q", "origin", "main")

  writeLines("local change", file.path(repo$dir, "local.txt"))
  repo$run("add", "local.txt")
  repo$run("commit", "-q", "-m", "local commit")

  result <- .git_push_current_branch(repo$dir, repo$git)
  expect_false(result$ok)
  expect_equal(result$code, "DIVERGED")
})

test_that(".git_push_current_branch refuses to push when only the remote has new work", {
  repo <- local_repo_with_remote()
  writeLines("more", file.path(repo$origin_dir, "more.txt"))
  repo$origin_run("add", "more.txt")
  repo$origin_run("commit", "-q", "-m", "remote-only commit")
  repo$origin_run("push", "-q", "origin", "main")

  result <- .git_push_current_branch(repo$dir, repo$git)
  expect_false(result$ok)
  expect_equal(result$code, "REMOTE_AHEAD")
})

test_that(".git_push_current_branch fails with NO_UPSTREAM when the branch has none", {
  git <- unname(Sys.which("git"))
  skip_if(!nzchar(git), "git not available")
  dir <- withr::local_tempdir()
  run <- function(...) processx::run(git, c("-C", dir, ...), error_on_status = TRUE)
  run("init", "-q", "-b", "main")
  writeLines("x", file.path(dir, "x.txt"))
  run("add", "x.txt")
  run("commit", "-q", "-m", "initial")

  result <- .git_push_current_branch(dir, git)
  expect_false(result$ok)
  expect_equal(result$code, "NO_UPSTREAM")
})
