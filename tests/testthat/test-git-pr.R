test_that(".validate_branch_name verifies valid Git ref names", {
  git <- unname(Sys.which("git"))
  skip_if(!nzchar(git), "git not available")

  expect_true(.validate_branch_name(git, "feature-1"))
  expect_true(.validate_branch_name(git, "update/2026-08-29"))
  expect_true(.validate_branch_name(git, "fix/issue_123"))

  expect_false(.validate_branch_name(git, ""))
  expect_false(.validate_branch_name(git, "feature 1"))
  expect_false(.validate_branch_name(git, "feature..1"))
  expect_false(.validate_branch_name(git, "feature~1"))
  expect_false(.validate_branch_name(git, "feature^1"))
  expect_false(.validate_branch_name(git, "feature:1"))
  expect_false(.validate_branch_name(git, "feature?1"))
  expect_false(.validate_branch_name(git, "feature*1"))
  expect_false(.validate_branch_name(git, NULL))
})

test_that(".git_create_pull_request requires GitHub remote and auth token", {
  git <- unname(Sys.which("git"))
  skip_if(!nzchar(git), "git not available")

  dir <- withr::local_tempdir()
  processx::run(git, c("-C", dir, "init", "-q", "-b", "main"), error_on_status = TRUE)
  processx::run(git, c("-C", dir, "config", "user.name", "Test"), error_on_status = TRUE)
  processx::run(git, c("-C", dir, "config", "user.email", "test@example.com"), error_on_status = TRUE)
  writeLines("hello", file.path(dir, "test.txt"))
  processx::run(git, c("-C", dir, "add", "test.txt"), error_on_status = TRUE)
  processx::run(git, c("-C", dir, "commit", "-q", "-m", "initial commit"), error_on_status = TRUE)

  # Non-GitHub remote
  processx::run(git, c("-C", dir, "remote", "add", "origin", "https://example.com/not-github.git"), error_on_status = TRUE)
  res_non_gh <- .git_create_pull_request(dir, git)
  expect_false(res_non_gh$ok)
  expect_equal(res_non_gh$code, "REMOTE_NOT_GITHUB")

  # Change remote to GitHub
  processx::run(git, c("-C", dir, "remote", "set-url", "origin", "https://github.com/my-org/my-repo.git"), error_on_status = TRUE)

  # No auth token
  state_no_auth <- new.env(parent = emptyenv())
  state_no_auth$github_token <- NULL

  withr::with_envvar(c(GITHUB_PAT = "", GITHUB_TOKEN = "", GH_TOKEN = ""), {
    res_no_auth <- .git_create_pull_request(dir, git, session_state = state_no_auth, gh_bin = "")
    expect_false(res_no_auth$ok)
    expect_equal(res_no_auth$code, "GITHUB_AUTH_REQUIRED")
  })
})

test_that(".git_create_pull_request validates branch names and collisions", {
  git <- unname(Sys.which("git"))
  skip_if(!nzchar(git), "git not available")

  dir <- withr::local_tempdir()
  processx::run(git, c("-C", dir, "init", "-q", "-b", "main"), error_on_status = TRUE)
  processx::run(git, c("-C", dir, "config", "user.name", "Test"), error_on_status = TRUE)
  processx::run(git, c("-C", dir, "config", "user.email", "test@example.com"), error_on_status = TRUE)
  writeLines("hello", file.path(dir, "test.txt"))
  processx::run(git, c("-C", dir, "add", "test.txt"), error_on_status = TRUE)
  processx::run(git, c("-C", dir, "commit", "-q", "-m", "initial commit"), error_on_status = TRUE)
  processx::run(git, c("-C", dir, "remote", "add", "origin", "https://github.com/my-org/my-repo.git"), error_on_status = TRUE)

  state <- new.env(parent = emptyenv())
  state$github_token <- "mock_token"

  # Invalid branch name
  res_invalid <- .git_create_pull_request(dir, git, pr_branch = "invalid branch with spaces", session_state = state)
  expect_false(res_invalid$ok)
  expect_equal(res_invalid$code, "INVALID_BRANCH_NAME")

  # Existing branch name
  processx::run(git, c("-C", dir, "branch", "existing-feature"), error_on_status = TRUE)
  res_exists <- .git_create_pull_request(dir, git, pr_branch = "existing-feature", session_state = state)
  expect_false(res_exists$ok)
  expect_equal(res_exists$code, "BRANCH_EXISTS")
})

local_github_repo <- function(env = parent.frame()) {
  git <- unname(Sys.which("git"))
  testthat::skip_if(!nzchar(git), "git not available")
  dir <- withr::local_tempdir(.local_envir = env)
  run <- function(...) processx::run(git, c("-C", dir, ...), error_on_status = TRUE)
  run("init", "-q", "-b", "main")
  run("config", "user.email", "test@example.com")
  run("config", "user.name", "Test")
  run("remote", "add", "origin", "https://github.com/my-org/my-repo.git")
  list(dir = dir, git = git, run = run)
}

test_that(".git_create_pull_request refuses a detached HEAD", {
  repo <- local_github_repo()
  writeLines("hello", file.path(repo$dir, "a.txt"))
  repo$run("add", "a.txt")
  repo$run("commit", "-q", "-m", "initial")
  repo$run("checkout", "-q", "--detach", "HEAD")

  result <- .git_create_pull_request(repo$dir, repo$git, session_state = list2env(list(github_token = "t")))
  expect_false(result$ok)
  expect_equal(result$code, "DETACHED_HEAD")
})

test_that(".git_create_pull_request falls back to a generic title and fails cleanly with no commits yet", {
  repo <- local_github_repo() # no commits at all: HEAD is unborn
  state <- list2env(list(github_token = "t"))

  result <- .git_create_pull_request(repo$dir, repo$git, pr_branch = "update/test", session_state = state)
  expect_false(result$ok)
  expect_equal(result$code, "COMMAND_FAILED")
  expect_match(result$message, "Failed to create feature branch")
})

test_that(".git_create_pull_request uses the latest commit's body when no body is supplied", {
  remote_dir <- withr::local_tempdir()
  git <- unname(Sys.which("git"))
  skip_if(!nzchar(git), "git not available")
  processx::run(git, c("init", "-q", "--bare", "-b", "main", remote_dir), error_on_status = TRUE)

  repo <- local_github_repo()
  repo$run("remote", "set-url", "origin", remote_dir)
  writeLines("hello", file.path(repo$dir, "a.txt"))
  repo$run("add", "a.txt")
  repo$run("commit", "-q", "-m", "Subject line\n\nA longer explanatory body.")
  state <- list2env(list(github_token = "t"))

  seen_body <- NULL
  testthat::local_mocked_bindings(
    .parse_github_slug = function(...) list(owner = "my-org", repo = "my-repo", host = "github.com", is_enterprise = FALSE),
    .github_api_call = function(path, method = "GET", body = NULL, ...) {
      seen_body <<- body
      list(ok = TRUE, data = list(number = 1L, html_url = "u", title = "t", state = "open"))
    }
  )
  result <- .git_create_pull_request(repo$dir, repo$git, pr_branch = "update/test", session_state = state)
  expect_true(result$ok)
  expect_match(seen_body$body, "A longer explanatory body.", fixed = TRUE)
})

test_that(".git_create_pull_request reports a classified push failure and cleans up the local branch", {
  skip_on_os("windows")
  repo <- local_github_repo()
  writeLines("hello", file.path(repo$dir, "a.txt"))
  repo$run("add", "a.txt")
  repo$run("commit", "-q", "-m", "initial")
  state <- list2env(list(github_token = "t"))
  failing_git <- .make_failing_git(repo$git, "push")

  result <- .git_create_pull_request(repo$dir, failing_git, pr_branch = "update/test", title = "T", session_state = state)
  expect_false(result$ok)
  expect_equal(result$code, "COMMAND_FAILED")
  expect_false(.git_branch_exists_locally(repo$dir, repo$git, "update/test")) # cleaned up on exit
})

test_that(".git_create_pull_request reports PR_CREATION_FAILED when the GitHub API call fails", {
  remote_dir <- withr::local_tempdir()
  git <- unname(Sys.which("git"))
  skip_if(!nzchar(git), "git not available")
  processx::run(git, c("init", "-q", "--bare", "-b", "main", remote_dir), error_on_status = TRUE)

  repo <- local_github_repo()
  repo$run("remote", "set-url", "origin", remote_dir)
  writeLines("hello", file.path(repo$dir, "a.txt"))
  repo$run("add", "a.txt")
  repo$run("commit", "-q", "-m", "initial")
  state <- list2env(list(github_token = "t"))

  testthat::local_mocked_bindings(
    .parse_github_slug = function(...) list(owner = "my-org", repo = "my-repo", host = "github.com", is_enterprise = FALSE),
    .github_api_call = function(...) list(ok = FALSE, error = "422 Unprocessable")
  )
  result <- .git_create_pull_request(repo$dir, repo$git, pr_branch = "update/test", title = "T", session_state = state)
  expect_false(result$ok)
  expect_equal(result$code, "PR_CREATION_FAILED")
  expect_match(result$message, "422")
})
