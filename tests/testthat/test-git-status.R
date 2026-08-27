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

test_that(".git_root finds the working tree root, and NULL outside one", {
  repo <- local_git_repo()
  expect_equal(fs::path_real(.git_root(repo$dir, repo$git)), fs::path_real(repo$dir))

  outside <- withr::local_tempdir()
  expect_null(.git_root(outside, repo$git))
})

test_that(".git_status reports a clean repo with no upstream", {
  repo <- local_git_repo()
  writeLines("hello", file.path(repo$dir, "file.txt"))
  repo$run("add", "file.txt")
  repo$run("commit", "-q", "-m", "initial commit")

  status <- .git_status(repo$dir, repo$git)
  expect_equal(status$branch, "main")
  expect_null(status$upstream)
  expect_false(status$has_changes)
  expect_equal(.primary_state(status, git_ok = TRUE), "NO_UPSTREAM")
})

test_that(".git_status reports unstaged and untracked changes", {
  repo <- local_git_repo()
  writeLines("hello", file.path(repo$dir, "file.txt"))
  repo$run("add", "file.txt")
  repo$run("commit", "-q", "-m", "initial commit")

  writeLines("hello again", file.path(repo$dir, "file.txt"))
  writeLines("new", file.path(repo$dir, "new.txt"))

  status <- .git_status(repo$dir, repo$git)
  expect_equal(status$unstaged_count, 1L)
  expect_equal(status$untracked_count, 1L)
  expect_true(status$has_changes)
})

test_that(".primary_state derives GIT_UNAVAILABLE and NOT_REPOSITORY", {
  expect_equal(.primary_state(NULL, git_ok = FALSE), "GIT_UNAVAILABLE")
  expect_equal(.primary_state(NULL, git_ok = TRUE), "NOT_REPOSITORY")
})
