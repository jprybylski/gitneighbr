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

test_that(".classify_update_failure maps a non-fast-forwardable merge to DIVERGED", {
  expect_equal(
    .classify_update_failure("fatal: Not possible to fast-forward, aborting."),
    "DIVERGED"
  )
  expect_equal(.classify_update_failure("fatal: something else broke"), "COMMAND_FAILED")
})

test_that(".git_update_current_branch fast-forwards a behind-only branch", {
  repo <- local_repo_with_remote()
  writeLines("more", file.path(repo$origin_dir, "more.txt"))
  repo$origin_run("add", "more.txt")
  repo$origin_run("commit", "-q", "-m", "remote-only commit")
  repo$origin_run("push", "-q", "origin", "main")

  result <- .git_update_current_branch(repo$dir, repo$git)
  expect_true(result$ok)
  expect_equal(result$remote, "origin")
  expect_equal(result$remote_branch, "main")
  expect_equal(result$updated_count, 1L)
  expect_true(file.exists(file.path(repo$dir, "more.txt")))
})

test_that(".git_update_current_branch refuses a dirty working tree without fetching", {
  repo <- local_repo_with_remote()
  writeLines("more", file.path(repo$origin_dir, "more.txt"))
  repo$origin_run("add", "more.txt")
  repo$origin_run("commit", "-q", "-m", "remote-only commit")
  repo$origin_run("push", "-q", "origin", "main")

  writeLines("dirty", file.path(repo$dir, "dirty.txt"))

  result <- .git_update_current_branch(repo$dir, repo$git)
  expect_false(result$ok)
  expect_equal(result$code, "DIRTY_BLOCKS_UPDATE")
  # No fetch happened: the remote-only commit is still not reflected locally.
  status <- .git_status(repo$dir, repo$git)
  expect_equal(status$behind, 0L)
})

test_that(".git_update_current_branch refuses to update when histories have diverged", {
  repo <- local_repo_with_remote()
  writeLines("more", file.path(repo$origin_dir, "more.txt"))
  repo$origin_run("add", "more.txt")
  repo$origin_run("commit", "-q", "-m", "remote-only commit")
  repo$origin_run("push", "-q", "origin", "main")

  writeLines("local change", file.path(repo$dir, "local.txt"))
  repo$run("add", "local.txt")
  repo$run("commit", "-q", "-m", "local commit")

  result <- .git_update_current_branch(repo$dir, repo$git)
  expect_false(result$ok)
  expect_equal(result$code, "DIVERGED")
})

test_that(".git_update_current_branch is a no-op success when already up to date", {
  repo <- local_repo_with_remote()
  result <- .git_update_current_branch(repo$dir, repo$git)
  expect_true(result$ok)
  expect_equal(result$updated_count, 0L)
})

test_that(".git_update_current_branch fails with NO_UPSTREAM when the branch has none", {
  git <- unname(Sys.which("git"))
  skip_if(!nzchar(git), "git not available")
  dir <- withr::local_tempdir()
  run <- function(...) processx::run(git, c("-C", dir, ...), error_on_status = TRUE)
  run("init", "-q", "-b", "main")
  writeLines("x", file.path(dir, "x.txt"))
  run("add", "x.txt")
  run("commit", "-q", "-m", "initial")

  result <- .git_update_current_branch(dir, git)
  expect_false(result$ok)
  expect_equal(result$code, "NO_UPSTREAM")
})

local_repo_without_remote <- function(env = parent.frame()) {
  git <- unname(Sys.which("git"))
  skip_if(!nzchar(git), "git not available")

  dir <- withr::local_tempdir(.local_envir = env)
  run <- function(...) processx::run(git, c("-C", dir, ...), error_on_status = TRUE)
  run("init", "-q", "-b", "main")
  run("config", "user.email", "test@example.com")
  run("config", "user.name", "Test")
  writeLines("hello", file.path(dir, "hello.txt"))
  run("add", "hello.txt")
  run("commit", "-q", "-m", "initial commit")

  list(dir = dir, git = git, run = run)
}

test_that(".git_publish_repo connects a fresh remote and pushes with -u", {
  repo <- local_repo_without_remote()
  remote_dir <- withr::local_tempdir()
  processx::run(repo$git, c("init", "-q", "--bare", "-b", "main", remote_dir), error_on_status = TRUE)

  result <- .git_publish_repo(repo$dir, repo$git, url = remote_dir)
  expect_true(result$ok)
  expect_equal(result$remote, "origin")
  expect_equal(result$remote_branch, "main")
  expect_equal(result$pushed_count, 1L)

  remote_log <- processx::run(repo$git, c("-C", remote_dir, "log", "-1", "--format=%s", "main"), error_on_status = TRUE)
  expect_equal(trimws(remote_log$stdout), "initial commit")
  expect_equal(.git_upstream_info(repo$dir, repo$git, "main")$remote, "origin")
})

test_that(".git_publish_repo fails with NOTHING_TO_PUBLISH when there are no commits yet", {
  git <- unname(Sys.which("git"))
  skip_if(!nzchar(git), "git not available")
  dir <- withr::local_tempdir()
  processx::run(git, c("-C", dir, "init", "-q", "-b", "main"), error_on_status = TRUE)

  result <- .git_publish_repo(dir, git, url = "https://github.com/example/example.git")
  expect_false(result$ok)
  expect_equal(result$code, "NOTHING_TO_PUBLISH")
})

test_that(".git_publish_repo fails with ALREADY_PUBLISHED when an upstream is already configured", {
  repo <- local_repo_with_remote()
  result <- .git_publish_repo(repo$dir, repo$git, url = "https://github.com/example/example.git")
  expect_false(result$ok)
  expect_equal(result$code, "ALREADY_PUBLISHED")
})

test_that(".git_publish_repo refuses to overwrite an existing origin without force, then succeeds with force", {
  repo <- local_repo_without_remote()
  first_remote <- withr::local_tempdir()
  processx::run(repo$git, c("init", "-q", "--bare", "-b", "main", first_remote), error_on_status = TRUE)
  repo$run("remote", "add", "origin", first_remote)

  second_remote <- withr::local_tempdir()
  processx::run(repo$git, c("init", "-q", "--bare", "-b", "main", second_remote), error_on_status = TRUE)

  refused <- .git_publish_repo(repo$dir, repo$git, url = second_remote)
  expect_false(refused$ok)
  expect_equal(refused$code, "REMOTE_ALREADY_SET")
  expect_equal(refused$data$existing_url, first_remote)

  forced <- .git_publish_repo(repo$dir, repo$git, url = second_remote, force = TRUE)
  expect_true(forced$ok)
  expect_equal(.git_remote_url(repo$dir, repo$git, "origin"), second_remote)
})
