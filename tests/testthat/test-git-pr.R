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
