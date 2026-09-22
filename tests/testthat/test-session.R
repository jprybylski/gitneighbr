# GitneighbrSession never needs a real processx process to test: it only
# ever calls is_alive()/kill_tree()/read_output_lines()/read_error_lines()
# on it, so a tiny fake with those four methods exercises every branch
# without spawning anything (unlike a real open_repo() session, whose
# server process is invisible to covr -- see helper-server.R).
.fake_process <- function(alive = TRUE, out = character(), err = character()) {
  state <- new.env(parent = emptyenv())
  state$alive <- alive
  state$killed <- FALSE
  list(
    is_alive = function() isTRUE(state$alive),
    kill_tree = function() {
      state$alive <- FALSE
      state$killed <- TRUE
    },
    read_output_lines = function(n = -1) out,
    read_error_lines = function(n = -1) err,
    .state = state
  )
}

test_that("url() includes the token fragment only when redact = FALSE", {
  s <- GitneighbrSession$new(.fake_process(), host = "127.0.0.1", port = 4123L, token = "secrettoken", repo_root = "/tmp/repo")
  expect_equal(s$url(), "http://127.0.0.1:4123/")
  expect_equal(s$url(redact = TRUE), "http://127.0.0.1:4123/")
  expect_equal(s$url(redact = FALSE), "http://127.0.0.1:4123/#token=secrettoken")
})

test_that("repo_path() and is_alive() reflect the wrapped process", {
  s <- GitneighbrSession$new(.fake_process(alive = TRUE), host = "127.0.0.1", port = 1L, token = "t", repo_root = "/tmp/repo")
  expect_equal(s$repo_path(), "/tmp/repo")
  expect_true(s$is_alive())
})

test_that("print() never reveals the token, and reports repo/url/alive", {
  s <- GitneighbrSession$new(.fake_process(alive = TRUE), host = "127.0.0.1", port = 4123L, token = "verysecret", repo_root = "/tmp/repo")
  out <- capture.output(print(s))
  text <- paste(out, collapse = "\n")
  expect_false(grepl("verysecret", text, fixed = TRUE))
  expect_true(grepl("/tmp/repo", text, fixed = TRUE))
  expect_true(grepl("alive: TRUE", text, fixed = TRUE))
})

test_that("stop() kills the process tree only while alive, and is a no-op otherwise", {
  s <- GitneighbrSession$new(.fake_process(alive = TRUE), host = "127.0.0.1", port = 1L, token = "t", repo_root = "/tmp/repo")
  s$stop()
  expect_false(s$is_alive())

  # Calling stop() again on an already-stopped session must not error.
  expect_no_error(s$stop())
})

test_that("logs() combines and tails stdout/stderr", {
  s <- GitneighbrSession$new(
    .fake_process(out = c("out1", "out2"), err = c("err1")),
    host = "127.0.0.1", port = 1L, token = "t", repo_root = "/tmp/repo"
  )
  expect_equal(s$logs(), c("out1", "out2", "err1"))
  expect_equal(s$logs(n = 1), "err1")
})

test_that("browse() errors when the session is no longer running", {
  s <- GitneighbrSession$new(.fake_process(alive = FALSE), host = "127.0.0.1", port = 1L, token = "t", repo_root = "/tmp/repo")
  expect_error(s$browse(), "no longer running")
})

test_that("browse() prefers an RStudio/Positron viewer over the system browser", {
  s <- GitneighbrSession$new(.fake_process(alive = TRUE), host = "127.0.0.1", port = 4123L, token = "tok", repo_root = "/tmp/repo")
  seen <- NULL
  withr::local_options(viewer = function(url) seen <<- url)
  ret <- s$browse()
  expect_equal(seen, s$url(redact = FALSE))
  expect_identical(ret, s)
})

test_that("browse() falls back to the system browser when no viewer option is set", {
  s <- GitneighbrSession$new(.fake_process(alive = TRUE), host = "127.0.0.1", port = 4123L, token = "tok", repo_root = "/tmp/repo")
  withr::local_options(viewer = NULL)
  seen <- NULL
  testthat::local_mocked_bindings(browseURL = function(url, ...) seen <<- url, .package = "utils")
  s$browse()
  expect_equal(seen, s$url(redact = FALSE))
})
