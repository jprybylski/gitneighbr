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

test_that(".git_version and .git_hooks_dir return NULL when the git binary can't run", {
  false_bin <- unname(Sys.which("false"))
  skip_if(!nzchar(false_bin), "no 'false' binary available")
  expect_null(.git_version(false_bin))
  expect_null(.git_hooks_dir(withr::local_tempdir(), false_bin))
})

test_that("doctor() reports FAIL when git_runs but the version can't be determined or is too old", {
  git <- unname(Sys.which("git"))
  skip_if(!nzchar(git), "git not available")

  testthat::local_mocked_bindings(.git_version = function(git_bin) NULL)
  utils::capture.output(unknown <- doctor(path = ".", git = git))
  expect_false(unknown$ok)
  expect_equal(unknown$checks$git_version$status, "fail")
  expect_match(unknown$checks$git_version$message, "Could not determine")

  testthat::local_mocked_bindings(.git_version = function(git_bin) "1.0.0")
  utils::capture.output(too_old <- doctor(path = ".", git = git))
  expect_false(too_old$ok)
  expect_equal(too_old$checks$git_version$status, "fail")
  expect_match(too_old$checks$git_version$message, "too old")
})

test_that("doctor() reports an upstream, a GitHub remote, and a credential helper as ok", {
  remote_dir <- withr::local_tempdir()
  git <- unname(Sys.which("git"))
  skip_if(!nzchar(git), "git not available")
  processx::run(git, c("init", "-q", "--bare", "-b", "main", remote_dir), error_on_status = TRUE)

  repo <- local_git_repo()
  writeLines("hello", file.path(repo$dir, "file.txt"))
  repo$run("add", "file.txt")
  repo$run("commit", "-q", "-m", "initial commit")
  # Push to the real bare remote first to establish upstream tracking (a
  # fake github.com URL can't actually be pushed to); the URL is swapped
  # afterward so `.is_github_remote()` sees it, since tracking state lives
  # in `branch.main.{remote,merge}` and is unaffected by later changing the
  # remote's URL.
  repo$run("remote", "add", "origin", remote_dir)
  repo$run("push", "-q", "-u", "origin", "main")
  repo$run("remote", "set-url", "origin", "https://github.com/example-org/example-repo.git")
  repo$run("config", "credential.helper", "store")

  utils::capture.output(result <- doctor(path = repo$dir, git = repo$git))
  expect_equal(result$checks$upstream$status, "ok")
  expect_equal(result$checks$remote$status, "ok")
  expect_match(result$checks$remote$message, "GitHub")
  expect_equal(result$checks$credential_helper$status, "ok")
  expect_match(result$checks$credential_helper$message, "store")
})

test_that("doctor() advises when an HTTPS remote has no credential helper configured", {
  repo <- local_git_repo()
  repo$run("remote", "add", "origin", "https://github.com/example-org/example-repo.git")
  withr::local_envvar(HOME = withr::local_tempdir()) # a fresh HOME has no global credential.helper
  testthat::local_mocked_bindings(.git_credential_helpers = function(...) character())

  utils::capture.output(result <- doctor(path = repo$dir, git = repo$git))
  expect_equal(result$checks$credential_helper$status, "advisory")
})

test_that("doctor() checks the SSH agent for an SSH remote", {
  repo <- local_git_repo()
  repo$run("remote", "add", "origin", "git@github.com:example-org/example-repo.git")

  testthat::local_mocked_bindings(.ssh_agent_status = function(...) list(has_keys = TRUE, key_count = 2L))
  utils::capture.output(with_keys <- doctor(path = repo$dir, git = repo$git))
  expect_equal(with_keys$checks$ssh_agent$status, "ok")
  expect_match(with_keys$checks$ssh_agent$message, "2 key")

  testthat::local_mocked_bindings(.ssh_agent_status = function(...) list(has_keys = FALSE, detail = "No agent running."))
  utils::capture.output(without_keys <- doctor(path = repo$dir, git = repo$git))
  expect_equal(without_keys$checks$ssh_agent$status, "advisory")
  expect_match(without_keys$checks$ssh_agent$message, "No agent running")
})

test_that("doctor() flags active hooks as advisory", {
  repo <- local_git_repo()
  hooks_dir <- .git_hooks_dir(repo$dir, repo$git)
  writeLines("#!/bin/sh\nexit 0", file.path(hooks_dir, "pre-commit"))
  Sys.chmod(file.path(hooks_dir, "pre-commit"), "0755")

  utils::capture.output(result <- doctor(path = repo$dir, git = repo$git))
  expect_equal(result$checks$hooks$status, "advisory")
  expect_match(result$checks$hooks$message, "pre-commit")
})

test_that("doctor() reports missing and partially-configured Git identity", {
  git <- unname(Sys.which("git"))
  skip_if(!nzchar(git), "git not available")
  dir <- withr::local_tempdir()
  processx::run(git, c("-C", dir, "init", "-q", "-b", "main"), error_on_status = TRUE)
  withr::local_envvar(
    HOME = withr::local_tempdir(),
    GIT_AUTHOR_NAME = NA, GIT_AUTHOR_EMAIL = NA,
    GIT_COMMITTER_NAME = NA, GIT_COMMITTER_EMAIL = NA
  )

  utils::capture.output(missing <- doctor(path = dir, git = git))
  expect_equal(missing$checks$identity$status, "advisory")
  expect_match(missing$checks$identity$message, "No Git identity")

  processx::run(git, c("-C", dir, "config", "user.name", "Only Name"), error_on_status = TRUE)
  utils::capture.output(partial <- doctor(path = dir, git = git))
  expect_equal(partial$checks$identity$status, "advisory")
  expect_match(partial$checks$identity$message, "partially configured")
})

test_that("print.gitneighbr_doctor_report renders every check with its status symbol", {
  repo <- local_git_repo()
  writeLines("hello", file.path(repo$dir, "file.txt"))
  repo$run("add", "file.txt")
  repo$run("commit", "-q", "-m", "initial commit")
  utils::capture.output(result <- doctor(path = repo$dir, git = repo$git))

  out <- paste(utils::capture.output(print(result)), collapse = "\n")
  expect_match(out, "gitneighbr doctor")
  expect_match(out, "Overall: ready\\.")
})
