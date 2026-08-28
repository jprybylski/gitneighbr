test_that("open_repo launches non-blocking, serves real status, and stops cleanly", {
  skip_on_cran()
  skip_if_not_installed("httr2")
  git <- unname(Sys.which("git"))
  skip_if(!nzchar(git), "git not available")

  dir <- withr::local_tempdir()
  run <- function(...) processx::run(git, c("-C", dir, ...), error_on_status = TRUE)
  run("init", "-q", "-b", "main")
  run("config", "user.email", "test@example.com")
  run("config", "user.name", "Test")
  writeLines("hello", file.path(dir, "file.txt"))
  run("add", "file.txt")
  run("commit", "-q", "-m", "initial commit")

  session <- open_repo(path = dir, browse = FALSE)
  withr::defer(session$stop())

  expect_true(session$is_alive())

  full_url <- session$url(redact = FALSE)
  token <- sub(".*token=", "", full_url)
  base_url <- sub("#.*", "", full_url)

  health <- httr2::request(paste0(base_url, "api/v1/health")) |>
    httr2::req_perform() |>
    httr2::resp_body_json()
  expect_true(health$ok)

  unauthorized <- httr2::request(paste0(base_url, "api/v1/status")) |>
    httr2::req_error(is_error = function(resp) FALSE) |>
    httr2::req_perform()
  expect_equal(httr2::resp_status(unauthorized), 401L)

  status <- httr2::request(paste0(base_url, "api/v1/status")) |>
    httr2::req_auth_bearer_token(token) |>
    httr2::req_perform() |>
    httr2::resp_body_json()
  expect_true(status$ok)
  expect_equal(status$data$primary_state, "NO_UPSTREAM")
  expect_equal(status$data$branch, "main")
  expect_true(is.numeric(status$status_version))

  session$stop()
  Sys.sleep(0.5)
  expect_false(session$is_alive())
})

test_that("POST /api/v1/commit saves exactly the selected files over HTTP", {
  skip_on_cran()
  skip_if_not_installed("httr2")
  git <- unname(Sys.which("git"))
  skip_if(!nzchar(git), "git not available")

  dir <- withr::local_tempdir()
  run <- function(...) processx::run(git, c("-C", dir, ...), error_on_status = TRUE)
  run("init", "-q", "-b", "main")
  run("config", "user.email", "test@example.com")
  run("config", "user.name", "Test")
  writeLines("keep me", file.path(dir, "keep.txt"))
  writeLines("leave me unstaged", file.path(dir, "skip.txt"))

  session <- open_repo(path = dir, browse = FALSE)
  withr::defer(session$stop())

  full_url <- session$url(redact = FALSE)
  token <- sub(".*token=", "", full_url)
  base_url <- sub("#.*", "", full_url)

  fetch_version <- function() {
    httr2::request(paste0(base_url, "api/v1/status")) |>
      httr2::req_auth_bearer_token(token) |>
      httr2::req_perform() |>
      httr2::resp_body_json() |>
      (\(resp) resp$status_version)()
  }

  stale <- httr2::request(paste0(base_url, "api/v1/commit")) |>
    httr2::req_auth_bearer_token(token) |>
    httr2::req_body_json(list(paths = list("keep.txt"), summary = "Add keep.txt", status_version = -1)) |>
    httr2::req_error(is_error = function(resp) FALSE) |>
    httr2::req_perform()
  expect_equal(httr2::resp_status(stale), 409L)
  stale_body <- httr2::resp_body_json(stale)
  expect_false(stale_body$ok)
  expect_equal(stale_body$error$code, "STATE_CHANGED")
  expect_false(is.null(stale_body$data))

  bad_summary <- httr2::request(paste0(base_url, "api/v1/commit")) |>
    httr2::req_auth_bearer_token(token) |>
    httr2::req_body_json(list(paths = list("keep.txt"), summary = "ab", status_version = fetch_version())) |>
    httr2::req_error(is_error = function(resp) FALSE) |>
    httr2::req_perform() |>
    httr2::resp_body_json()
  expect_false(bad_summary$ok)
  expect_equal(bad_summary$error$code, "INVALID_SUMMARY")

  saved <- httr2::request(paste0(base_url, "api/v1/commit")) |>
    httr2::req_auth_bearer_token(token) |>
    httr2::req_body_json(list(paths = list("keep.txt"), summary = "Add keep.txt", status_version = fetch_version())) |>
    httr2::req_perform() |>
    httr2::resp_body_json()
  expect_true(saved$ok)
  expect_match(saved$data$sha, "^[0-9a-f]+$")

  status <- httr2::request(paste0(base_url, "api/v1/status")) |>
    httr2::req_auth_bearer_token(token) |>
    httr2::req_perform() |>
    httr2::resp_body_json()
  expect_equal(status$data$untracked_count, 1L) # skip.txt was never selected
  expect_true(file.exists(file.path(dir, "skip.txt")))
})

test_that("POST /api/v1/refresh-remote and /api/v1/push send a local commit to GitHub over HTTP", {
  skip_on_cran()
  skip_if_not_installed("httr2")
  git <- unname(Sys.which("git"))
  skip_if(!nzchar(git), "git not available")

  remote_dir <- withr::local_tempdir()
  processx::run(git, c("init", "-q", "--bare", "-b", "main", remote_dir), error_on_status = TRUE)

  seed_dir <- withr::local_tempdir()
  seed_run <- function(...) processx::run(git, c("-C", seed_dir, ...), error_on_status = TRUE)
  seed_run("init", "-q", "-b", "main")
  seed_run("config", "user.email", "seed@example.com")
  seed_run("config", "user.name", "Seed")
  writeLines("seed", file.path(seed_dir, "seed.txt"))
  seed_run("add", "seed.txt")
  seed_run("commit", "-q", "-m", "seed commit")
  seed_run("remote", "add", "origin", remote_dir)
  seed_run("push", "-q", "-u", "origin", "main")

  dir <- withr::local_tempdir()
  processx::run(git, c("clone", "-q", remote_dir, dir), error_on_status = TRUE)
  run <- function(...) processx::run(git, c("-C", dir, ...), error_on_status = TRUE)
  run("config", "user.email", "test@example.com")
  run("config", "user.name", "Test")
  writeLines("local change", file.path(dir, "local.txt"))
  run("add", "local.txt")
  run("commit", "-q", "-m", "local commit")

  session <- open_repo(path = dir, browse = FALSE)
  withr::defer(session$stop())

  full_url <- session$url(redact = FALSE)
  token <- sub(".*token=", "", full_url)
  base_url <- sub("#.*", "", full_url)

  fetch_version <- function() {
    httr2::request(paste0(base_url, "api/v1/status")) |>
      httr2::req_auth_bearer_token(token) |>
      httr2::req_perform() |>
      httr2::resp_body_json() |>
      (\(resp) resp$status_version)()
  }

  refreshed <- httr2::request(paste0(base_url, "api/v1/refresh-remote")) |>
    httr2::req_method("POST") |>
    httr2::req_auth_bearer_token(token) |>
    httr2::req_body_json(list(status_version = fetch_version())) |>
    httr2::req_perform() |>
    httr2::resp_body_json()
  expect_true(refreshed$ok)
  expect_equal(refreshed$data$ahead, 1L)
  expect_equal(refreshed$data$behind, 0L)

  pushed <- httr2::request(paste0(base_url, "api/v1/push")) |>
    httr2::req_method("POST") |>
    httr2::req_auth_bearer_token(token) |>
    httr2::req_body_json(list(status_version = fetch_version())) |>
    httr2::req_perform() |>
    httr2::resp_body_json()
  expect_true(pushed$ok)
  expect_equal(pushed$data$remote, "origin")
  expect_equal(pushed$data$pushed_count, 1L)

  remote_log <- processx::run(git, c("-C", remote_dir, "log", "-1", "--format=%s", "main"), error_on_status = TRUE)
  expect_equal(trimws(remote_log$stdout), "local commit")
})
