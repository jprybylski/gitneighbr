test_that(".git_tag_annotation extracts message from an annotated tag", {
  git <- unname(Sys.which("git"))
  skip_if(!nzchar(git), "git not available")

  dir <- withr::local_tempdir()
  processx::run(git, c("-C", dir, "init", "-q", "-b", "main"), error_on_status = TRUE)
  processx::run(git, c("-C", dir, "config", "user.name", "Test"), error_on_status = TRUE)
  processx::run(git, c("-C", dir, "config", "user.email", "test@example.com"), error_on_status = TRUE)
  writeLines("hello", file.path(dir, "test.txt"))
  processx::run(git, c("-C", dir, "add", "test.txt"), error_on_status = TRUE)
  processx::run(git, c("-C", dir, "commit", "-q", "-m", "initial commit"), error_on_status = TRUE)

  processx::run(git, c("-C", dir, "tag", "-a", "v1.2.0", "-m", "Release 1.2.0 notes\n\n- Fix bugs\n- Add features"), error_on_status = TRUE)

  annotation <- .git_tag_annotation(dir, git, "v1.2.0")
  expect_match(annotation, "Release 1.2.0 notes")
  expect_match(annotation, "Fix bugs")
})

test_that(".git_create_release verifies tag existence and GitHub requirements", {
  git <- unname(Sys.which("git"))
  skip_if(!nzchar(git), "git not available")

  dir <- withr::local_tempdir()
  processx::run(git, c("-C", dir, "init", "-q", "-b", "main"), error_on_status = TRUE)
  processx::run(git, c("-C", dir, "config", "user.name", "Test"), error_on_status = TRUE)
  processx::run(git, c("-C", dir, "config", "user.email", "test@example.com"), error_on_status = TRUE)
  writeLines("hello", file.path(dir, "test.txt"))
  processx::run(git, c("-C", dir, "add", "test.txt"), error_on_status = TRUE)
  processx::run(git, c("-C", dir, "commit", "-q", "-m", "initial commit"), error_on_status = TRUE)

  # Tag does not exist
  res_no_tag <- .git_create_release(dir, git, tag_name = "nonexistent-tag")
  expect_false(res_no_tag$ok)
  expect_equal(res_no_tag$code, "TAG_NOT_FOUND")

  # Create tag
  processx::run(git, c("-C", dir, "tag", "-a", "v1.0.0", "-m", "v1.0.0"), error_on_status = TRUE)

  # Non-GitHub remote
  processx::run(git, c("-C", dir, "remote", "add", "origin", "https://example.com/not-github.git"), error_on_status = TRUE)
  res_non_gh <- .git_create_release(dir, git, tag_name = "v1.0.0")
  expect_false(res_non_gh$ok)
  expect_equal(res_non_gh$code, "REMOTE_NOT_GITHUB")

  # Remote is GitHub, but no auth token
  processx::run(git, c("-C", dir, "remote", "set-url", "origin", "https://github.com/my-org/my-repo.git"), error_on_status = TRUE)

  state_no_auth <- new.env(parent = emptyenv())
  state_no_auth$github_token <- NULL

  withr::with_envvar(c(GITHUB_PAT = "", GITHUB_TOKEN = "", GH_TOKEN = ""), {
    res_no_auth <- .git_create_release(dir, git, tag_name = "v1.0.0", session_state = state_no_auth, gh_bin = "")
    expect_false(res_no_auth$ok)
    expect_equal(res_no_auth$code, "GITHUB_AUTH_REQUIRED")
  })
})

test_that(".git_tag_annotation returns an empty string for a tag that doesn't exist", {
  git <- unname(Sys.which("git"))
  skip_if(!nzchar(git), "git not available")
  dir <- withr::local_tempdir()
  processx::run(git, c("-C", dir, "init", "-q", "-b", "main"), error_on_status = TRUE)
  expect_equal(.git_tag_annotation(dir, git, "no-such-tag"), "")
})

test_that(".git_create_release requires a non-blank tag name", {
  git <- unname(Sys.which("git"))
  skip_if(!nzchar(git), "git not available")
  expect_equal(.git_create_release(".", git, tag_name = NULL)$code, "INVALID_TAG")
  expect_equal(.git_create_release(".", git, tag_name = "   ")$code, "INVALID_TAG")
})

test_that(".git_create_release uses the supplied name and body as-is when given", {
  remote_dir <- withr::local_tempdir()
  git <- unname(Sys.which("git"))
  skip_if(!nzchar(git), "git not available")
  processx::run(git, c("init", "-q", "--bare", "-b", "main", remote_dir), error_on_status = TRUE)

  dir <- withr::local_tempdir()
  run <- function(...) processx::run(git, c("-C", dir, ...), error_on_status = TRUE)
  run("init", "-q", "-b", "main")
  run("config", "user.name", "Test")
  run("config", "user.email", "test@example.com")
  writeLines("hello", file.path(dir, "test.txt"))
  run("add", "test.txt")
  run("commit", "-q", "-m", "initial commit")
  run("tag", "-a", "v1.0.0", "-m", "auto annotation")
  run("remote", "add", "origin", remote_dir)
  run("push", "-q", "-u", "origin", "main")

  seen_payload <- NULL
  testthat::local_mocked_bindings(
    .parse_github_slug = function(...) list(owner = "my-org", repo = "my-repo", host = "github.com", is_enterprise = FALSE),
    .github_api_call = function(path, method = "GET", body = NULL, ...) {
      seen_payload <<- body
      list(ok = TRUE, data = list(id = 1L, html_url = "u"))
    }
  )
  state <- list2env(list(github_token = "t"))
  result <- .git_create_release(dir, git, tag_name = "v1.0.0", name = "My Release", body = "Custom notes", session_state = state)
  expect_true(result$ok)
  expect_equal(seen_payload$name, "My Release")
  expect_equal(seen_payload$body, "Custom notes")
})

test_that(".git_create_release fails when pushing the tag first fails", {
  skip_on_os("windows")
  git <- unname(Sys.which("git"))
  skip_if(!nzchar(git), "git not available")
  remote_dir <- withr::local_tempdir()
  processx::run(git, c("init", "-q", "--bare", "-b", "main", remote_dir), error_on_status = TRUE)

  dir <- withr::local_tempdir()
  run <- function(...) processx::run(git, c("-C", dir, ...), error_on_status = TRUE)
  run("init", "-q", "-b", "main")
  run("config", "user.name", "Test")
  run("config", "user.email", "test@example.com")
  writeLines("hello", file.path(dir, "test.txt"))
  run("add", "test.txt")
  run("commit", "-q", "-m", "initial commit")
  run("tag", "-a", "v1.0.0", "-m", "notes")
  run("remote", "add", "origin", remote_dir)
  run("push", "-q", "-u", "origin", "main")

  testthat::local_mocked_bindings(
    .parse_github_slug = function(...) list(owner = "my-org", repo = "my-repo", host = "github.com", is_enterprise = FALSE)
  )
  state <- list2env(list(github_token = "t"))
  failing_git <- .make_failing_git(git, "push")
  result <- .git_create_release(dir, failing_git, tag_name = "v1.0.0", session_state = state)
  expect_false(result$ok)
  expect_match(result$message, "Failed to push tag")
})

test_that(".git_create_release reports RELEASE_CREATION_FAILED when the GitHub API call fails", {
  git <- unname(Sys.which("git"))
  skip_if(!nzchar(git), "git not available")
  remote_dir <- withr::local_tempdir()
  processx::run(git, c("init", "-q", "--bare", "-b", "main", remote_dir), error_on_status = TRUE)

  dir <- withr::local_tempdir()
  run <- function(...) processx::run(git, c("-C", dir, ...), error_on_status = TRUE)
  run("init", "-q", "-b", "main")
  run("config", "user.name", "Test")
  run("config", "user.email", "test@example.com")
  writeLines("hello", file.path(dir, "test.txt"))
  run("add", "test.txt")
  run("commit", "-q", "-m", "initial commit")
  run("tag", "-a", "v1.0.0", "-m", "notes")
  run("remote", "add", "origin", remote_dir)
  run("push", "-q", "-u", "origin", "main")

  testthat::local_mocked_bindings(
    .parse_github_slug = function(...) list(owner = "my-org", repo = "my-repo", host = "github.com", is_enterprise = FALSE),
    .github_api_call = function(...) list(ok = FALSE, error = "422 Validation Failed")
  )
  state <- list2env(list(github_token = "t"))
  result <- .git_create_release(dir, git, tag_name = "v1.0.0", session_state = state)
  expect_false(result$ok)
  expect_equal(result$code, "RELEASE_CREATION_FAILED")
  expect_match(result$message, "422")
})

test_that(".git_list_releases reports REMOTE_NOT_GITHUB for a non-GitHub remote", {
  git <- unname(Sys.which("git"))
  skip_if(!nzchar(git), "git not available")
  dir <- withr::local_tempdir()
  processx::run(git, c("-C", dir, "init", "-q", "-b", "main"), error_on_status = TRUE)
  processx::run(git, c("-C", dir, "remote", "add", "origin", "https://example.com/not-github.git"), error_on_status = TRUE)

  result <- .git_list_releases(dir, git)
  expect_false(result$ok)
  expect_equal(result$code, "REMOTE_NOT_GITHUB")
})
