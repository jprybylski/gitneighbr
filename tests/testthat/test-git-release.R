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
