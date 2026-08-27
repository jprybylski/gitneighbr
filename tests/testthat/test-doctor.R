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

test_that(".parse_git_version handles vendor suffixes", {
  expect_equal(.parse_git_version("git version 2.42.0"), "2.42.0")
  expect_equal(.parse_git_version("git version 2.42.0 (Apple Git-140)"), "2.42.0")
  expect_null(.parse_git_version("not a version string"))
})

test_that(".git_repo_kind distinguishes worktree, bare, and neither", {
  repo <- local_git_repo()
  expect_equal(.git_repo_kind(repo$dir, repo$git), "worktree")

  bare_dir <- withr::local_tempdir()
  processx::run(repo$git, c("init", "-q", "--bare", bare_dir), error_on_status = TRUE)
  expect_equal(.git_repo_kind(bare_dir, repo$git), "bare")

  outside <- withr::local_tempdir()
  expect_equal(.git_repo_kind(outside, repo$git), "none")
})

test_that(".is_github_remote recognizes https and ssh GitHub URLs, rejects others", {
  expect_true(.is_github_remote("https://github.com/user/repo.git"))
  expect_true(.is_github_remote("git@github.com:user/repo.git"))
  expect_false(.is_github_remote("https://gitlab.com/user/repo.git"))
  expect_false(.is_github_remote(NULL))
})

test_that(".active_hooks lists only non-sample, executable hooks", {
  repo <- local_git_repo()
  hooks_dir <- .git_hooks_dir(repo$dir, repo$git)
  expect_true(fs::dir_exists(hooks_dir))
  expect_equal(.active_hooks(repo$dir, repo$git), character())

  writeLines("#!/bin/sh\nexit 0", file.path(hooks_dir, "pre-commit"))
  Sys.chmod(file.path(hooks_dir, "pre-commit"), "0755")
  expect_equal(.active_hooks(repo$dir, repo$git), "pre-commit")
})

test_that("doctor() reports a clean, working repo as ok with advisories only", {
  repo <- local_git_repo()
  writeLines("hello", file.path(repo$dir, "file.txt"))
  repo$run("add", "file.txt")
  repo$run("commit", "-q", "-m", "initial commit")

  report <- suppressMessages(utils::capture.output(result <- doctor(path = repo$dir, git = repo$git)))
  expect_true(result$ok)
  expect_equal(result$checks$git_found$status, "ok")
  expect_equal(result$checks$repository$status, "ok")
  expect_equal(result$checks$upstream$status, "advisory")
  expect_equal(result$checks$remote$status, "advisory")
})

test_that("doctor() reports NOT_REPOSITORY-equivalent failure outside a repo", {
  git <- unname(Sys.which("git"))
  skip_if(!nzchar(git), "git not available")
  outside <- withr::local_tempdir()

  utils::capture.output(result <- doctor(path = outside, git = git))
  expect_false(result$ok)
  expect_equal(result$checks$repository$status, "fail")
})

test_that("doctor() reports a bare repository distinctly from no repository", {
  git <- unname(Sys.which("git"))
  skip_if(!nzchar(git), "git not available")
  bare_dir <- withr::local_tempdir()
  processx::run(git, c("init", "-q", "--bare", bare_dir), error_on_status = TRUE)

  utils::capture.output(result <- doctor(path = bare_dir, git = git))
  expect_false(result$ok)
  expect_match(result$checks$repository$message, "bare repository")
})

test_that("doctor() fails cleanly when no git executable is configured", {
  utils::capture.output(result <- doctor(path = ".", git = ""))
  expect_false(result$ok)
  expect_equal(result$checks$git_found$status, "fail")
  expect_equal(result$checks$git_runs$status, "skipped")
})
