test_that(".classify_clone_failure maps common Git clone errors", {
  expect_equal(
    .classify_clone_failure("fatal: destination path 'x' already exists and is not an empty directory."),
    "CLONE_DESTINATION_NOT_EMPTY"
  )
  expect_equal(.classify_clone_failure("fatal: Authentication failed for 'https://...'"), "AUTH_REQUIRED")
  expect_equal(.classify_clone_failure("remote: Repository not found.\nfatal: repository not found"), "REMOTE_NOT_FOUND")
  expect_equal(.classify_clone_failure("fatal: could not resolve host: github.com"), "REMOTE_UNREACHABLE")
  expect_equal(.classify_clone_failure("fatal: something else broke"), "COMMAND_FAILED")
})

local_bare_remote_with_commit <- function(env = parent.frame()) {
  git <- unname(Sys.which("git"))
  skip_if(!nzchar(git), "git not available")

  remote_dir <- withr::local_tempdir(.local_envir = env)
  processx::run(git, c("init", "-q", "--bare", "-b", "main", remote_dir), error_on_status = TRUE)

  seed_dir <- withr::local_tempdir(.local_envir = env)
  seed_run <- function(...) processx::run(git, c("-C", seed_dir, ...), error_on_status = TRUE)
  seed_run("init", "-q", "-b", "main")
  seed_run("config", "user.email", "seed@example.com")
  seed_run("config", "user.name", "Seed")
  writeLines("seed", file.path(seed_dir, "seed.txt"))
  seed_run("add", "seed.txt")
  seed_run("commit", "-q", "-m", "seed commit")
  seed_run("remote", "add", "origin", remote_dir)
  seed_run("push", "-q", "-u", "origin", "main")

  list(git = git, remote_dir = remote_dir)
}

test_that(".git_clone_repo clones into a workspace path that doesn't exist yet", {
  fixture <- local_bare_remote_with_commit()
  dest <- file.path(withr::local_tempdir(), "nested", "project")

  result <- .git_clone_repo(dest, fixture$git, fixture$remote_dir)
  expect_true(result$ok)
  expect_true(file.exists(file.path(dest, "seed.txt")))
  expect_equal(.git_repo_kind(dest, fixture$git), "worktree")
  expect_equal(.git_remote_url(dest, fixture$git, "origin"), fixture$remote_dir)
})

test_that(".git_clone_repo refuses a destination that is already a Git repository", {
  fixture <- local_bare_remote_with_commit()
  dest <- withr::local_tempdir()
  processx::run(fixture$git, c("-C", dest, "init", "-q", "-b", "main"), error_on_status = TRUE)

  result <- .git_clone_repo(dest, fixture$git, fixture$remote_dir)
  expect_false(result$ok)
  expect_equal(result$code, "ALREADY_A_REPOSITORY")
})

test_that(".git_clone_repo fails clearly when the destination already has other files in it", {
  fixture <- local_bare_remote_with_commit()
  dest <- withr::local_tempdir()
  writeLines("in the way", file.path(dest, "existing.txt"))

  result <- .git_clone_repo(dest, fixture$git, fixture$remote_dir)
  expect_false(result$ok)
  expect_equal(result$code, "CLONE_DESTINATION_NOT_EMPTY")
})

test_that(".git_clone_repo rejects a blank URL", {
  git <- unname(Sys.which("git"))
  skip_if(!nzchar(git), "git not available")
  dest <- file.path(withr::local_tempdir(), "project")

  result <- .git_clone_repo(dest, git, "")
  expect_false(result$ok)
  expect_equal(result$code, "INVALID_REMOTE_URL")
})
