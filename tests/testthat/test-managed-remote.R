# Managed Send and Update against a real HTTPS remote: every outgoing and
# incoming commit is listed, nothing beyond the preview is transferred, the
# token never appears on a command line, and nothing touches the network
# until the user asks.

.commit_file <- function(repo, path, text, message = paste("Change", path)) {
  dir.create(dirname(file.path(repo$dir, path)), showWarnings = FALSE, recursive = TRUE)
  writeLines(as.character(text), file.path(repo$dir, path))
  repo$run("add", "--", path)
  repo$run("commit", "-q", "-m", message)
  trimws(repo$run("rev-parse", "HEAD")$stdout)
}

.post <- function(api, path, body = list()) {
  .api_req(api, path, method = "post", headers = .api_auth(api), body = c(body, list(status_version = .api_version(api))))
}

.connect <- function(api, remote, token = TRUE) {
  connect <- .post(api, "/api/v1/remote/connect", list(url = remote$url))
  testthat::expect_true(isTRUE(connect$json$ok))
  if (token) {
    t <- .api_req(api, "/api/v1/remote/token", method = "post", headers = .api_auth(api), body = list(token = remote$password))
    testthat::expect_true(isTRUE(t$json$data$has_token))
  }
}

test_that("Send lists the whole outgoing history and sends exactly the previewed commit", {
  remote <- .local_git_https_remote()
  repo <- .new_test_repo()
  shas <- vapply(1:3, function(i) .commit_file(repo, paste0("f", i, ".txt"), i), character(1))
  api <- .test_remote_api(repo$dir, remote)

  # Polling status with a remote configured makes no network request.
  .connect(api, remote)
  for (i in 1:3) .api_req(api, "/api/v1/status", headers = .api_auth(api))
  expect_length(remote$requests(), 0L)

  # No preview without a destination check.
  early <- .api_req(api, "/api/v1/send/preview", headers = .api_auth(api))
  expect_identical(early$json$error$code, "OUTGOING_NOT_INSPECTABLE")

  check <- .post(api, "/api/v1/remote/check")
  expect_true(isTRUE(check$json$ok))
  expect_false(check$json$data$exists)
  expect_gt(length(remote$requests()), 0L)
  expect_true(all(grepl(" auth$", remote$requests())))

  preview <- .api_req(api, "/api/v1/send/preview", headers = .api_auth(api))$json$data
  expect_identical(preview$commits$sha, shas)
  expect_identical(preview$totals$commits, 3L)
  expect_identical(preview$destination$branch, "main")
  expect_true(preview$can_send)

  # A commit made after the preview is not sent: the preview is refused.
  .commit_file(repo, "late.txt", "late")
  stale <- .post(api, "/api/v1/send", list(preview_id = preview$preview_id, confirm = TRUE))
  expect_identical(stale$json$error$code, "STATE_CHANGED")
  expect_false(nzchar(trimws(processx::run(remote$git, c("-C", remote$bare, "branch", "--list", "main"))$stdout)))

  .post(api, "/api/v1/remote/check")
  preview <- .api_req(api, "/api/v1/send/preview", headers = .api_auth(api))$json$data
  expect_identical(preview$totals$commits, 4L)
  unconfirmed <- .post(api, "/api/v1/send", list(preview_id = preview$preview_id))
  expect_identical(unconfirmed$json$error$code, "CONFIRMATION_REQUIRED")
  sent <- .post(api, "/api/v1/send", list(preview_id = preview$preview_id, confirm = TRUE))
  expect_true(isTRUE(sent$json$ok))
  remote_head <- trimws(processx::run(remote$git, c("-C", remote$bare, "rev-parse", "main"))$stdout)
  expect_identical(remote_head, trimws(repo$run("rev-parse", "HEAD")$stdout))
  expect_identical(trimws(repo$run("config", "branch.main.remote")$stdout), "origin")

  # Only what's new is listed next time.
  .commit_file(repo, "f5.txt", "5")
  .post(api, "/api/v1/remote/check")
  preview <- .api_req(api, "/api/v1/send/preview", headers = .api_auth(api))$json$data
  expect_identical(preview$totals$commits, 1L)
  expect_identical(preview$commits$files[[1]]$path, "f5.txt")
})

test_that("without a token the destination check fails cleanly and changes nothing", {
  remote <- .local_git_https_remote()
  repo <- .new_test_repo()
  head <- .commit_file(repo, "a.txt", "a")
  api <- .test_remote_api(repo$dir, remote)
  .connect(api, remote, token = FALSE)

  check <- .post(api, "/api/v1/remote/check")
  expect_identical(check$json$error$code, "AUTH_REQUIRED")
  expect_identical(trimws(repo$run("rev-parse", "HEAD")$stdout), head)
  status <- .api_req(api, "/api/v1/status", headers = .api_auth(api))$json$data
  expect_identical(status$primary_state, "AUTH_REQUIRED")
  expect_false(status$remote$has_token)

  wrong <- .api_req(api, "/api/v1/remote/token", method = "post", headers = .api_auth(api), body = list(token = "not-the-token"))
  expect_true(isTRUE(wrong$json$ok))
  expect_identical(.post(api, "/api/v1/remote/check")$json$error$code, "AUTH_REQUIRED")
})

test_that("the token never reaches a command line, the page or an error", {
  skip_on_os("windows") # the argv-logging stand-in for git is a shell script
  remote <- .local_git_https_remote()
  repo <- .new_test_repo()
  .commit_file(repo, "a.txt", "a")
  argv_log <- withr::local_tempfile()
  wrapper <- withr::local_tempfile(fileext = ".sh")
  writeLines(c("#!/bin/sh", paste0("printf '%s\\n' \"$*\" >> '", argv_log, "'"), paste0("exec '", remote$git, "' \"$@\"")), wrapper)
  Sys.chmod(wrapper, "755")

  config <- withr::local_tempfile(fileext = ".gitconfig")
  .write_ca_config(config, remote$cert)
  managed <- .managed_config(features = "local,remote")
  api <- .test_api(repo$dir, .managed_git(wrapper, config, remote = TRUE), managed = managed)
  .connect(api, remote)
  check <- .post(api, "/api/v1/remote/check")
  preview <- .api_req(api, "/api/v1/send/preview", headers = .api_auth(api))
  sent <- .post(api, "/api/v1/send", list(preview_id = preview$json$data$preview_id, confirm = TRUE))
  expect_true(isTRUE(sent$json$ok))

  basic <- openssl::base64_encode(charToRaw(paste0(remote$user, ":", remote$password)))
  argv <- paste(readLines(argv_log), collapse = "\n")
  expect_match(argv, "push", fixed = TRUE)
  expect_false(grepl(remote$password, argv, fixed = TRUE))
  expect_false(grepl(basic, argv, fixed = TRUE))
  for (resp in list(check, preview, sent, .api_req(api, "/api/v1/status", headers = .api_auth(api)))) {
    expect_false(grepl(remote$password, resp$body, fixed = TRUE))
    expect_false(grepl(basic, resp$body, fixed = TRUE))
  }
})

test_that("review labels need an extra confirmation and blocked paths can't be sent", {
  remote <- .local_git_https_remote()
  repo <- .new_test_repo()
  .commit_file(repo, "data/subjects.csv", "id,value")
  policy <- list(
    schema_version = 1L,
    review = list(list(id = "data", label = "Study data", message = "May identify people.", globs = list("data/**")))
  )
  api <- .test_remote_api(repo$dir, remote, policy = policy)
  .connect(api, remote)
  .post(api, "/api/v1/remote/check")
  preview <- .api_req(api, "/api/v1/send/preview", headers = .api_auth(api))$json$data
  expect_identical(unlist(preview$review), "data")
  refused <- .post(api, "/api/v1/send", list(preview_id = preview$preview_id, confirm = TRUE))
  expect_identical(refused$json$error$code, "CONFIRMATION_REQUIRED")
  ok <- .post(api, "/api/v1/send", list(preview_id = preview$preview_id, confirm = TRUE, confirm_review = TRUE))
  expect_true(isTRUE(ok$json$ok))

  repo2 <- .new_test_repo()
  .commit_file(repo2, "secrets/key.txt", "k")
  api2 <- .test_remote_api(repo2$dir, remote, policy = list(schema_version = 1L, send_blocked = list("secrets/**")))
  .post(api2, "/api/v1/remote/connect", list(url = sub("study.git", "other.git", remote$url)))
  processx::run(remote$git, c("init", "-q", "--bare", file.path(dirname(remote$bare), "other.git")))
  processx::run(remote$git, c("-C", file.path(dirname(remote$bare), "other.git"), "config", "http.receivepack", "true"))
  .api_req(api2, "/api/v1/remote/token", method = "post", headers = .api_auth(api2), body = list(token = remote$password))
  .post(api2, "/api/v1/remote/check")
  blocked <- .api_req(api2, "/api/v1/send/preview", headers = .api_auth(api2))$json$data
  expect_false(blocked$can_send)
  expect_identical(unlist(blocked$blocked), "secrets/key.txt")
  expect_identical(.post(api2, "/api/v1/send", list(preview_id = blocked$preview_id, confirm = TRUE))$json$error$code, "SEND_BLOCKED")
})

test_that("Update previews incoming commits, refuses protected paths and local work, then fast-forwards", {
  remote <- .local_git_https_remote()
  # Someone else's copy publishes the history.
  other <- .new_test_repo()
  .commit_file(other, "runs/r1.txt", "v1")
  base <- .commit_file(other, "notes.txt", "n1")
  processx::run(other$git, c("-C", other$dir, "push", "-q", remote$bare, "main"))

  repo <- .new_test_repo()
  # This copy is checked out with the machine's Git, whose system settings a
  # managed Git ignores; on Windows those turn on core.autocrlf, so the
  # managed Git would see every checked-out file as changed. Fix it locally.
  repo$run("config", "core.autocrlf", "false")
  processx::run(repo$git, c("-C", repo$dir, "fetch", "-q", remote$bare, "main"))
  repo$run("merge", "-q", "--ff-only", "FETCH_HEAD")
  policy <- list(schema_version = 1L, protected = list("runs/**"))
  api <- .test_remote_api(repo$dir, remote, policy = policy)
  .connect(api, remote)
  repo$run("config", "branch.main.remote", "origin")
  repo$run("config", "branch.main.merge", "refs/heads/main")

  # An incoming change to a protected path is refused.
  .commit_file(other, "runs/r1.txt", "v2")
  processx::run(other$git, c("-C", other$dir, "push", "-q", remote$bare, "main"))
  .post(api, "/api/v1/remote/check")
  incoming <- .api_req(api, "/api/v1/receive/preview", headers = .api_auth(api))$json$data
  expect_false(incoming$can_update)
  expect_identical(incoming$blocked$reason, "protected")
  expect_identical(.post(api, "/api/v1/receive", list(preview_id = incoming$preview_id, confirm = TRUE))$json$error$code, "UPDATE_BLOCKED")
  expect_identical(readLines(file.path(repo$dir, "runs", "r1.txt")), "v1")

  # A fresh copy without protection, but with local work on an incoming file.
  api_open <- .test_remote_api(repo$dir, remote)
  .api_req(api_open, "/api/v1/remote/token", method = "post", headers = .api_auth(api_open), body = list(token = remote$password))
  writeLines("local edit", file.path(repo$dir, "runs", "r1.txt"))
  .post(api_open, "/api/v1/remote/check")
  dirty <- .api_req(api_open, "/api/v1/receive/preview", headers = .api_auth(api_open))$json$data
  expect_identical(dirty$blocked$reason, "local_changes")
  repo$run("checkout", "--", "runs/r1.txt")

  incoming <- .api_req(api_open, "/api/v1/receive/preview", headers = .api_auth(api_open))$json$data
  expect_true(incoming$can_update)
  expect_identical(incoming$commits$summary, "Change runs/r1.txt")
  done <- .post(api_open, "/api/v1/receive", list(preview_id = incoming$preview_id, confirm = TRUE))
  expect_true(isTRUE(done$json$ok))
  expect_identical(readLines(file.path(repo$dir, "runs", "r1.txt")), "v2")

  # Diverged histories stop with guidance, changing nothing.
  .commit_file(other, "notes.txt", "theirs")
  processx::run(other$git, c("-C", other$dir, "push", "-q", remote$bare, "main"))
  mine <- .commit_file(repo, "mine.txt", "mine")
  .post(api_open, "/api/v1/remote/check")
  diverged <- .api_req(api_open, "/api/v1/receive/preview", headers = .api_auth(api_open))
  expect_identical(diverged$json$error$code, "DIVERGED")
  send <- .api_req(api_open, "/api/v1/send/preview", headers = .api_auth(api_open))
  expect_identical(send$json$error$code, "DIVERGED")
  expect_identical(trimws(repo$run("rev-parse", "HEAD")$stdout), mine)
})

test_that("only plain HTTPS addresses are accepted", {
  repo <- .new_test_repo()
  .commit_file(repo, "a.txt", "a")
  api <- .test_managed_api(repo$dir, features = "local,remote")
  for (url in c("git@github.com:o/r.git", "ssh://github.com/o/r.git", "https://user:pw@github.com/o/r.git", "file:///tmp/r.git", "http://example.com/r.git", "-https://x/y")) {
    res <- .post(api, "/api/v1/remote/connect", list(url = url))
    expect_identical(res$json$error$code, "INVALID_REMOTE_URL", info = url)
  }
  repo$run("remote", "add", "origin", "git@github.com:o/r.git")
  check <- .post(api, "/api/v1/remote/check")
  expect_identical(check$json$error$code, "REMOTE_NOT_HTTPS")
  expect_identical(.api_req(api, "/api/v1/status", headers = .api_auth(api))$json$data$remote$problem$code, "REMOTE_NOT_HTTPS")
})
