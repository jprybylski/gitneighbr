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

test_that(".valid_host_header accepts either loopback spelling on the bound port, rejects everything else", {
  expect_true(.valid_host_header("127.0.0.1:4123", 4123L))
  expect_true(.valid_host_header("localhost:4123", 4123L))
  expect_false(.valid_host_header("127.0.0.1:9999", 4123L)) # wrong port
  expect_false(.valid_host_header("evil.example.com:4123", 4123L)) # rebound hostname
  expect_false(.valid_host_header(NULL, 4123L))
})

test_that(".valid_origin_header allows an absent Origin, requires an exact match when present", {
  expect_true(.valid_origin_header(NULL, 4123L))
  expect_true(.valid_origin_header("", 4123L))
  expect_true(.valid_origin_header("http://127.0.0.1:4123", 4123L))
  expect_true(.valid_origin_header("http://localhost:4123", 4123L))
  expect_false(.valid_origin_header("http://127.0.0.1:9999", 4123L))
  expect_false(.valid_origin_header("https://127.0.0.1:4123", 4123L)) # never https locally
  expect_false(.valid_origin_header("http://evil.example.com", 4123L))
})

test_that(".new_operation_id returns distinct, non-empty opaque IDs", {
  a <- .new_operation_id()
  b <- .new_operation_id()
  expect_true(is.character(a) && nzchar(a))
  expect_false(identical(a, b))
})

test_that(".acquire_mutation_lock / .release_mutation_lock enforce single-mutation serialization", {
  session_state <- new.env(parent = emptyenv())
  session_state$mutation_lock <- FALSE
  headers <- new.env(parent = emptyenv())
  response <- list(set_header = function(name, value) assign(name, value, envir = headers))

  first <- .acquire_mutation_lock(session_state, response)
  expect_true(is.character(first) && nzchar(first))
  expect_equal(get("X-Operation-Id", envir = headers), first)

  competing <- .acquire_mutation_lock(session_state, response)
  expect_null(competing)

  .release_mutation_lock(session_state)
  expect_false(session_state$mutation_lock)

  second <- .acquire_mutation_lock(session_state, response)
  expect_true(is.character(second) && nzchar(second))
  expect_false(identical(second, first))
})
