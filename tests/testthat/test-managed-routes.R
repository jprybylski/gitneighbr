# Managed routes in this process, with no network: the shutdown route, the
# cross-site refusal, and the guards in front of the remote routes.

.managed_routes_api <- function(dir, shutdown_token = NULL, session_state = NULL, env = parent.frame()) {
  git <- unname(Sys.which("git"))
  testthat::skip_if(!nzchar(git), "git not available")
  config <- withr::local_tempfile(.local_envir = env, fileext = ".gitconfig")
  managed <- gitneighbr:::.managed_config(shutdown_token = shutdown_token, features = "local,remote")
  git_bin <- gitneighbr:::.managed_git(git, config, remote = TRUE)
  api <- .test_api(dir, git_bin, managed = managed, session_state = session_state, env = env)
  api$control <- managed$control
  api
}

.remote_post <- function(api, path, body = list(), version = .api_version(api)) {
  .api_req(api, path, method = "post", headers = .api_auth(api), body = c(body, list(status_version = version)))
}

test_that("the shutdown route needs the shutdown token, then asks the server to stop", {
  repo <- .new_test_repo()
  api <- .managed_routes_api(repo$dir, shutdown_token = strrep("s", 32))
  refused <- .api_req(api, "/api/v1/shutdown", method = "post", headers = .api_auth(api))
  expect_identical(refused$status, 403L)
  expect_false(isTRUE(api$control$stop_requested))
  ok <- .api_req(api, "/api/v1/shutdown",
    method = "post",
    headers = c(.api_auth(api), list(`X-Gitneighbr-Shutdown-Token` = strrep("s", 32)))
  )
  expect_true(isTRUE(ok$json$data$stopping))
  expect_true(api$control$stop_requested)

  # Without a shutdown token there is no shutdown route at all.
  plain <- .managed_routes_api(repo$dir)
  expect_false(identical(.api_req(plain, "/api/v1/shutdown", method = "post", headers = .api_auth(plain))$status, 200L))
})

test_that("a managed session refuses cross-site requests", {
  repo <- .new_test_repo()
  api <- .managed_routes_api(repo$dir)
  resp <- .api_req(api, "/api/v1/status", headers = c(.api_auth(api), list(`Sec-Fetch-Site` = "cross-site")))
  expect_identical(resp$status, 403L)
})

test_that("remote routes need a repository, a fresh list and a free lock", {
  empty <- withr::local_tempdir()
  api <- .managed_routes_api(empty)
  expect_false(isTRUE(.remote_post(api, "/api/v1/remote/connect", list(url = "https://example.com/r.git"))$json$ok))
  expect_false(isTRUE(.api_req(api, "/api/v1/send/preview", headers = .api_auth(api))$json$ok))
  expect_false(dir.exists(file.path(empty, ".git")))

  repo <- .new_test_repo()
  api <- .managed_routes_api(repo$dir)
  stale <- .remote_post(api, "/api/v1/remote/connect", list(url = "https://example.com/r.git"), version = -1L)
  expect_false(isTRUE(stale$json$ok))
  expect_identical(stale$json$error$code, "STATE_CHANGED")

  state <- .new_session_state()
  state$mutation_lock <- TRUE
  busy <- .managed_routes_api(repo$dir, session_state = state)
  locked <- .remote_post(busy, "/api/v1/remote/connect", list(url = "https://example.com/r.git"))
  expect_identical(locked$status, 423L)
  expect_identical(locked$json$error$code, "OPERATION_IN_PROGRESS")
  expect_error(processx::run(repo$git, c("-C", repo$dir, "config", "--get", "remote.origin.url")))
})

test_that("connecting twice keeps the address, and a different one is refused", {
  repo <- .new_test_repo()
  api <- .managed_routes_api(repo$dir)
  url <- "https://example.com/team/study.git"
  expect_true(isTRUE(.remote_post(api, "/api/v1/remote/connect", list(url = url))$json$ok))
  expect_true(isTRUE(.remote_post(api, "/api/v1/remote/connect", list(url = url))$json$ok))
  other <- .remote_post(api, "/api/v1/remote/connect", list(url = "https://example.com/other.git"))
  expect_identical(other$json$error$code, "REMOTE_ALREADY_SET")
  expect_identical(
    trimws(processx::run(repo$git, c("-C", repo$dir, "config", "--get", "remote.origin.url"))$stdout),
    url
  )
})

test_that("the token route forgets, refuses odd tokens and needs a destination", {
  repo <- .new_test_repo()
  api <- .managed_routes_api(repo$dir)
  token <- function(value) {
    .api_req(api, "/api/v1/remote/token", method = "post", headers = .api_auth(api), body = list(token = value))
  }
  expect_false(isTRUE(token("ghp_example_token")$json$ok))
  expect_false(isTRUE(token("")$json$data$has_token))
  .remote_post(api, "/api/v1/remote/connect", list(url = "https://example.com/team/study.git"))
  bad <- token("two words")
  expect_identical(bad$json$error$code, "INVALID_TOKEN")
  expect_false(grepl("two words", bad$body, fixed = TRUE))
  expect_true(isTRUE(token("ghp_example_token")$json$data$has_token))
  expect_false(isTRUE(token("")$json$data$has_token))
})

test_that("Send and Update explain what's missing before touching the network", {
  repo <- .new_test_repo()
  writeLines("a", file.path(repo$dir, "a.txt"))
  repo$run("add", "a.txt")
  repo$run("commit", "-q", "-m", "first")
  api <- .managed_routes_api(repo$dir)
  .remote_post(api, "/api/v1/remote/connect", list(url = "https://example.com/team/study.git"))
  code <- function(path) .api_req(api, path, headers = .api_auth(api))$json$error$code

  # Update needs a destination check first; nothing was fetched.
  expect_identical(code("/api/v1/receive/preview"), "INCOMING_NOT_INSPECTABLE")
  received <- .remote_post(api, "/api/v1/receive", list(preview_id = "not-a-preview", confirm = TRUE))
  expect_identical(received$json$error$code, "STATE_CHANGED")

  repo$run("config", "remote.origin.pushurl", "https://example.com/elsewhere.git")
  expect_identical(code("/api/v1/send/preview"), "REMOTE_PUSHURL")
  repo$run("config", "--unset", "remote.origin.pushurl")

  repo$run("checkout", "-q", "--detach")
  expect_identical(code("/api/v1/send/preview"), "DETACHED_HEAD")
})
