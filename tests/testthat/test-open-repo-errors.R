test_that("open_repo() raises a coded GIT_UNAVAILABLE error when git can't be found", {
  err <- tryCatch(open_repo(path = ".", browse = FALSE, git = ""), error = function(e) e)
  expect_s3_class(err, "gitneighbr_error_git_unavailable")
  expect_equal(err$code, "GIT_UNAVAILABLE")
})

test_that("open_repo() raises a coded GIT_UNAVAILABLE error when the configured git can't run", {
  bogus <- withr::local_tempfile()
  writeLines("not an executable", bogus)
  err <- tryCatch(open_repo(path = ".", browse = FALSE, git = bogus), error = function(e) e)
  expect_s3_class(err, "gitneighbr_error_git_unavailable")
  expect_equal(err$code, "GIT_UNAVAILABLE")
})

test_that("open_repo() raises a coded NOT_REPOSITORY error outside a working tree", {
  git <- unname(Sys.which("git"))
  skip_if(!nzchar(git), "git not available")
  outside <- withr::local_tempdir()

  err <- tryCatch(open_repo(path = outside, browse = FALSE, git = git), error = function(e) e)
  expect_s3_class(err, "gitneighbr_error_not_repository")
  expect_equal(err$code, "NOT_REPOSITORY")
})

test_that("open_repo() raises a coded BARE_REPOSITORY error for a bare repository", {
  git <- unname(Sys.which("git"))
  skip_if(!nzchar(git), "git not available")
  bare_dir <- withr::local_tempdir()
  processx::run(git, c("init", "-q", "--bare", bare_dir), error_on_status = TRUE)

  err <- tryCatch(open_repo(path = bare_dir, browse = FALSE, git = git), error = function(e) e)
  expect_s3_class(err, "gitneighbr_error_bare_repository")
  expect_equal(err$code, "BARE_REPOSITORY")
})

test_that("open_repo() raises a coded GIT_TOO_OLD error when the version is below the minimum", {
  git <- unname(Sys.which("git"))
  skip_if(!nzchar(git), "git not available")

  testthat::local_mocked_bindings(.git_version = function(git_bin) "1.0.0")
  err <- tryCatch(open_repo(path = ".", browse = FALSE, git = git), error = function(e) e)
  expect_s3_class(err, "gitneighbr_error_git_too_old")
  expect_equal(err$code, "GIT_TOO_OLD")
})
