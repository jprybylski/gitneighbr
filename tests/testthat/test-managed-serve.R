# A real `scripts/serve.R` child: the launcher contract end to end over HTTP
# (ready line, token header, cross-site refusal, static files under a base
# path, two-token shutdown).

.serve_script <- function() {
  script <- system.file("scripts", "serve.R", package = "gitneighbr")
  testthat::skip_if(!nzchar(script), "scripts/serve.R not found")
  script
}

# Start serve.R in a child that loads this source tree (so the test runs the
# code under test, not an older installed copy).
.start_managed <- function(repo_dir, git, args = character(), env = parent.frame(),
                           api_token = strrep("a", 32), shutdown_token = strrep("s", 32)) {
  testthat::skip_on_cran()
  root <- testthat::test_path("..", "..")
  port <- gitneighbr:::.find_free_port()
  expr <- sprintf(
    "pkgload::load_all(%s, quiet = TRUE, export_all = FALSE); source(%s)",
    deparse(normalizePath(root)), deparse(.serve_script())
  )
  proc <- processx::process$new(
    file.path(R.home("bin"), "Rscript"),
    c(
      "--vanilla", "-e", expr,
      paste0("--repo=", repo_dir), paste0("--port=", port), paste0("--git=", git),
      "--api-token-env=GN_TEST_API", "--shutdown-token-env=GN_TEST_SHUTDOWN",
      args
    ),
    env = c("current", GN_TEST_API = api_token, GN_TEST_SHUTDOWN = shutdown_token),
    stdout = "|", stderr = "|", cleanup_tree = TRUE
  )
  withr::defer(if (proc$is_alive()) proc$kill_tree(), envir = env)
  list(proc = proc, port = port, api_token = api_token, shutdown_token = shutdown_token)
}

.read_ready_line <- function(proc, timeout = 60) {
  deadline <- Sys.time() + timeout
  while (Sys.time() < deadline) {
    proc$poll_io(500)
    for (line in proc$read_output_lines()) {
      if (startsWith(line, "GITNEIGHBR_READY ")) {
        return(jsonlite::fromJSON(sub("^GITNEIGHBR_READY ", "", line)))
      }
    }
    if (!proc$is_alive()) {
      stop("serve.R exited: ", proc$read_all_error())
    }
  }
  stop("no ready line within ", timeout, " s")
}

.http <- function(port, path, method = "GET", headers = list(), body = NULL) {
  h <- curl::new_handle()
  all_headers <- utils::modifyList(list(Host = paste0("127.0.0.1:", port)), headers)
  curl::handle_setheaders(h, .list = all_headers)
  if (!identical(method, "GET")) {
    curl::handle_setopt(h, customrequest = method, postfields = body %||% "{}")
  }
  res <- curl::curl_fetch_memory(paste0("http://127.0.0.1:", port, path), handle = h)
  res$text <- rawToChar(res$content)
  res$json <- tryCatch(jsonlite::fromJSON(res$text), error = function(e) NULL)
  res
}

test_that("serve.R prints a ready line and serves the app under its base path", {
  repo <- .new_test_repo()
  child <- .start_managed(repo$dir, repo$git, "--base-path=/apps/gitneighbr/")
  ready <- .read_ready_line(child$proc)

  expect_identical(ready$url, sprintf("http://127.0.0.1:%d/apps/gitneighbr/", child$port))
  expect_identical(ready$version, as.character(utils::packageVersion("gitneighbr")))
  expect_false(grepl(child$api_token, jsonlite::toJSON(ready), fixed = TRUE))

  health <- .http(child$port, "/apps/gitneighbr/api/v1/health")
  expect_identical(health$status_code, 200L)
  expect_true(health$json$data$managed)
  expect_false(health$json$data$busy)

  page <- .http(child$port, "/apps/gitneighbr/")
  expect_identical(page$status_code, 200L)
  expect_match(page$text, '<base href="/apps/gitneighbr/" />', fixed = TRUE)
  asset <- regmatches(page$text, regexpr("assets/index-[^\"]+\\.js", page$text))
  expect_identical(.http(child$port, paste0("/apps/gitneighbr/", asset))$status_code, 200L)
})

test_that("a managed session takes its token only from the launcher header", {
  repo <- .new_test_repo()
  child <- .start_managed(repo$dir, repo$git)
  .read_ready_line(child$proc)
  token <- list(`X-Gitneighbr-Token` = child$api_token)

  expect_identical(.http(child$port, "/api/v1/status")$status_code, 401L)
  bearer <- list(Authorization = paste("Bearer", child$api_token))
  expect_identical(.http(child$port, "/api/v1/status", headers = bearer)$status_code, 401L)
  expect_identical(.http(child$port, "/api/v1/status", headers = token)$status_code, 200L)

  # A browser request another site made: refused even with the token.
  cross <- c(token, list(`Sec-Fetch-Site` = "cross-site"))
  expect_identical(.http(child$port, "/api/v1/status", headers = cross)$status_code, 403L)
  form <- c(token, list(`Sec-Fetch-Site` = "cross-site", `Content-Type` = "application/x-www-form-urlencoded"))
  expect_identical(.http(child$port, "/api/v1/commit", method = "POST", headers = form)$status_code, 403L)
  # A rebound host name carrying the token.
  evil <- c(token, list(Host = "evil.example:80"))
  expect_identical(.http(child$port, "/api/v1/status", headers = evil)$status_code, 400L)
})

test_that("the shutdown route needs both tokens and the server then exits cleanly", {
  repo <- .new_test_repo()
  child <- .start_managed(repo$dir, repo$git)
  .read_ready_line(child$proc)
  token <- list(`X-Gitneighbr-Token` = child$api_token)

  expect_identical(.http(child$port, "/api/v1/shutdown", method = "POST", headers = token)$status_code, 403L)
  wrong <- c(token, list(`X-Gitneighbr-Shutdown-Token` = strrep("x", 32)))
  expect_identical(.http(child$port, "/api/v1/shutdown", method = "POST", headers = wrong)$status_code, 403L)
  expect_true(child$proc$is_alive())

  right <- c(token, list(`X-Gitneighbr-Shutdown-Token` = child$shutdown_token))
  res <- .http(child$port, "/api/v1/shutdown", method = "POST", headers = right)
  expect_identical(res$status_code, 200L)
  expect_true(res$json$data$stopping)
  child$proc$wait(15000)
  expect_false(child$proc$is_alive())
  expect_identical(child$proc$get_exit_status(), 0L)
})

test_that("serve.R refuses unknown, repeated and missing options without serving", {
  repo <- .new_test_repo()
  for (extra in list("--bogus=1", c("--host=127.0.0.1", "--host=localhost"), "--host=0.0.0.0", "--features=local,warp")) {
    child <- .start_managed(repo$dir, repo$git, extra)
    child$proc$wait(30000)
    expect_identical(child$proc$get_exit_status(), 2L, info = paste(extra, collapse = " "))
    expect_false(any(startsWith(child$proc$read_output_lines(), "GITNEIGHBR_READY")))
    expect_false(grepl(child$api_token, child$proc$read_all_error(), fixed = TRUE))
  }
})
