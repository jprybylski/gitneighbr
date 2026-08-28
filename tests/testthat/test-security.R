test_that(".sanitize_git_output redacts embedded URL credentials", {
  x <- .sanitize_git_output("fatal: unable to access 'https://alice:hunter2@github.com/x/y.git/'")
  expect_false(grepl("hunter2", x, fixed = TRUE))
  expect_match(x, "https://\\*\\*\\*@github\\.com")
})

test_that(".sanitize_git_output redacts Authorization: Bearer tokens", {
  x <- .sanitize_git_output("remote: Authorization: Bearer abc123.def456-ghi")
  expect_false(grepl("abc123", x, fixed = TRUE))
  expect_match(x, "Bearer \\*\\*\\*")
})

test_that(".sanitize_git_output redacts GitHub PAT-shaped tokens", {
  token <- paste0("ghp_", strrep("a", 36))
  x <- .sanitize_git_output(paste("remote: rejected token", token))
  expect_false(grepl(token, x, fixed = TRUE))
})

test_that(".sanitize_git_output redacts common key=value / key: value credential fields", {
  expect_false(grepl("s3kr1t", .sanitize_git_output("password=s3kr1t"), fixed = TRUE))
  expect_false(grepl("s3kr1t", .sanitize_git_output("token: s3kr1t"), fixed = TRUE))
})

test_that(".sanitize_git_output collapses the home directory to '~'", {
  home <- path.expand("~")
  skip_if(!nzchar(home), "no home directory available")
  x <- .sanitize_git_output(paste0("fatal: could not open '", home, "/repo/.git/config'"))
  expect_false(grepl(home, x, fixed = TRUE))
  expect_match(x, "^fatal: could not open '~/repo/\\.git/config'$")
})

test_that(".sanitize_git_output tolerates NULL, NA, and empty input", {
  expect_equal(.sanitize_git_output(NULL), "")
  expect_equal(.sanitize_git_output(NA_character_), "")
  expect_equal(.sanitize_git_output(""), "")
})

test_that(".sanitize_git_output leaves ordinary stderr untouched", {
  x <- "error: pathspec 'nope.txt' did not match any file(s) known to git"
  expect_equal(.sanitize_git_output(x), x)
})
