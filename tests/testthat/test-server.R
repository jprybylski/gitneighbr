# Route coverage for R/server.R via .build_api()'s in-process test_request()
# seam (see helper-server.R for why real open_repo() sessions can't be used
# for this: their route handlers run in a separate Rscript child process,
# invisible to covr).

# The mutating (POST) endpoints and whether their handler requires an
# existing repository before it will run any business logic. Every one of
# these validates Host -> auth -> Origin -> git availability -> [existing
# repo] -> body -> freshness -> mutation lock, in that order (confirmed by
# reading R/server.R), which is what lets the shared-gate tests below use
# one representative body per route without knowing its business semantics.
.mutating_routes <- list(
  list(path = "/api/v1/identity", needs_repo = TRUE),
  list(path = "/api/v1/commit", needs_repo = TRUE),
  list(path = "/api/v1/refresh-remote", needs_repo = TRUE),
  list(path = "/api/v1/push", needs_repo = TRUE),
  list(path = "/api/v1/update", needs_repo = TRUE),
  list(path = "/api/v1/tag", needs_repo = TRUE),
  list(path = "/api/v1/push-tag", needs_repo = TRUE),
  list(path = "/api/v1/restore", needs_repo = TRUE),
  list(path = "/api/v1/trash", needs_repo = TRUE),
  list(path = "/api/v1/ignore", needs_repo = TRUE),
  list(path = "/api/v1/init", needs_repo = FALSE),
  list(path = "/api/v1/clone", needs_repo = FALSE),
  list(path = "/api/v1/publish", needs_repo = TRUE),
  list(path = "/api/v1/pull-request", needs_repo = TRUE),
  list(path = "/api/v1/github/release", needs_repo = TRUE)
)

test_that("every mutating endpoint rejects a mismatched Origin before touching the repository", {
  repo <- .new_test_repo()
  api <- .test_api(repo$dir, repo$git)

  for (route in .mutating_routes) {
    resp <- .api_req(
      api, route$path,
      method = "post",
      headers = c(.api_auth(api), list(Origin = "http://evil.example.com")),
      body = list(status_version = -1)
    )
    expect_equal(resp$status, 403L, info = route$path)
  }
})

test_that("every mutating endpoint that needs a repository refuses a non-repository directory", {
  dir <- withr::local_tempdir()
  git <- unname(Sys.which("git"))
  skip_if(!nzchar(git), "git not available")
  api <- .test_api(dir, git)

  for (route in .mutating_routes) {
    if (!route$needs_repo) next
    resp <- .api_req(api, route$path, method = "post", headers = .api_auth(api), body = list(status_version = -1))
    expect_equal(resp$json$error$code, "NOT_REPOSITORY", info = route$path)
    expect_false(resp$json$ok, info = route$path)
  }
})

test_that("every mutating endpoint reports GIT_UNAVAILABLE when the git binary can't run", {
  repo <- .new_test_repo()
  api <- .test_api(repo$dir, git_bin = "/definitely/not/a/real/git/binary")

  for (route in .mutating_routes) {
    resp <- .api_req(api, route$path, method = "post", headers = .api_auth(api), body = list(status_version = -1))
    expect_equal(resp$json$error$code, "GIT_UNAVAILABLE", info = route$path)
  }
})

test_that("every mutating endpoint requires the current status_version (STATE_CHANGED) and carries fresh data", {
  repo <- .new_test_repo()
  api <- .test_api(repo$dir, repo$git)

  for (route in .mutating_routes) {
    resp <- .api_req(api, route$path, method = "post", headers = .api_auth(api), body = list(status_version = -1))
    expect_equal(resp$status, 409L, info = route$path)
    expect_equal(resp$json$error$code, "STATE_CHANGED", info = route$path)
    expect_false(is.null(resp$json$data), info = route$path)
  }
})

test_that("every mutating endpoint returns 423 OPERATION_IN_PROGRESS when the session's lock is already held", {
  repo <- .new_test_repo()
  state <- .new_session_state()
  state$mutation_lock <- TRUE
  api <- .test_api(repo$dir, repo$git, session_state = state)

  current_version <- .api_version(api)
  for (route in .mutating_routes) {
    resp <- .api_req(
      api, route$path,
      method = "post", headers = .api_auth(api),
      body = list(status_version = current_version)
    )
    expect_equal(resp$status, 423L, info = route$path)
    expect_equal(resp$json$error$code, "OPERATION_IN_PROGRESS", info = route$path)
  }
})

test_that("a malformed JSON body is reported as COMMAND_FAILED, not a raw parse error", {
  repo <- .new_test_repo()
  api <- .test_api(repo$dir, repo$git)
  resp <- .api_req(
    api, "/api/v1/commit",
    method = "post",
    headers = c(.api_auth(api), list("Content-Type" = "application/json")),
    body = NULL
  )
  # Sending literally broken JSON: bypass the body-encoding helper.
  rook <- reqres:::mock_rook(
    url = paste0("http://", api$host, "/api/v1/commit"),
    method = "post",
    headers = c(.api_auth(api), list(Host = api$host, "Content-Type" = "application/json")),
    content = "{not valid json"
  )
  raw <- api$server$test_request(rook)
  body_json <- jsonlite::fromJSON(raw$body, simplifyVector = TRUE)
  expect_false(body_json$ok)
  expect_equal(body_json$error$code, "COMMAND_FAILED")
})

# ---- GET endpoints --------------------------------------------------------

test_that("GET /api/v1/health ignores auth and returns ok without touching the repo", {
  api <- .test_api(withr::local_tempdir(), unname(Sys.which("git")))
  resp <- .api_req(api, "/api/v1/health")
  expect_equal(resp$status, 200L)
  expect_true(resp$json$ok)
})

test_that("a bad Host header is rejected on a GET before auth is even checked", {
  api <- .test_api(withr::local_tempdir(), unname(Sys.which("git")))
  resp <- .api_req(api, "/api/v1/health", host = "evil.example.com:1234")
  expect_equal(resp$status, 400L)
})

test_that("GET /api/v1/status requires auth and reports show_ignored", {
  repo <- .new_test_repo()
  writeLines("ignored", file.path(repo$dir, "ignored.log"))
  writeLines("*.log\n", file.path(repo$dir, ".gitignore"))
  api <- .test_api(repo$dir, repo$git)

  unauthed <- .api_req(api, "/api/v1/status")
  expect_equal(unauthed$status, 401L)

  ok <- .api_req(api, "/api/v1/status", headers = .api_auth(api))
  expect_true(ok$json$ok)
  expect_equal(ok$json$data$primary_state, "NO_UPSTREAM")

  shown <- .api_req(api, "/api/v1/status?show_ignored=true", headers = .api_auth(api))
  expect_true(shown$json$ok)
})

test_that("GET /api/v1/changes lists a modified file, and 404s without a repository", {
  repo <- .new_test_repo()
  writeLines("v1", file.path(repo$dir, "f.txt"))
  repo$run("add", "f.txt")
  repo$run("commit", "-q", "-m", "initial")
  writeLines("v2", file.path(repo$dir, "f.txt"))
  api <- .test_api(repo$dir, repo$git)

  resp <- .api_req(api, "/api/v1/changes", headers = .api_auth(api))
  expect_true(resp$json$ok)
  expect_equal(length(resp$json$data$changes$path), 1L)

  non_repo <- .test_api(withr::local_tempdir(), repo$git)
  err <- .api_req(non_repo, "/api/v1/changes", headers = .api_auth(non_repo))
  expect_equal(err$json$error$code, "NOT_REPOSITORY")
})

test_that("GET /api/v1/diff serves a unified diff and rejects a path outside the repository", {
  repo <- .new_test_repo()
  writeLines("v1", file.path(repo$dir, "f.txt"))
  repo$run("add", "f.txt")
  repo$run("commit", "-q", "-m", "initial")
  writeLines("v2", file.path(repo$dir, "f.txt"))
  api <- .test_api(repo$dir, repo$git)

  ok <- .api_req(api, "/api/v1/diff?path=f.txt", headers = .api_auth(api))
  expect_true(ok$json$ok)

  outside <- .api_req(api, "/api/v1/diff?path=../outside.txt", headers = .api_auth(api))
  expect_equal(outside$json$error$code, "PATH_OUTSIDE_REPOSITORY")

  missing <- .api_req(api, "/api/v1/diff?path=nope.txt", headers = .api_auth(api))
  expect_equal(missing$json$error$code, "COMMAND_FAILED")

  negative_offset <- .api_req(api, "/api/v1/diff?path=f.txt&offset_lines=-5", headers = .api_auth(api))
  expect_true(negative_offset$json$ok)
})

test_that("GET /api/v1/identity, /credential-diagnosis, and /diagnostic-report return structured data", {
  repo <- .new_test_repo()
  api <- .test_api(repo$dir, repo$git)

  identity <- .api_req(api, "/api/v1/identity", headers = .api_auth(api))
  expect_true(identity$json$ok)
  expect_equal(identity$json$data$name, "Test")

  diag <- .api_req(api, "/api/v1/credential-diagnosis", headers = .api_auth(api))
  expect_true(diag$json$ok)

  report <- .api_req(api, "/api/v1/diagnostic-report", headers = .api_auth(api))
  expect_true(report$json$ok)
  expect_equal(report$json$data$report$branch, "main")
  expect_true(is.list(report$json$data$report$commands))
})

test_that("GET /api/v1/policy reflects a .gitneighbr.json policy file", {
  repo <- .new_test_repo()
  writeLines('{"require_pull_request": true}', file.path(repo$dir, ".gitneighbr.json"))
  api <- .test_api(repo$dir, repo$git)
  resp <- .api_req(api, "/api/v1/policy", headers = .api_auth(api))
  expect_true(resp$json$ok)
  expect_true(resp$json$data$require_pull_request)
})

# ---- Mutating business logic ----------------------------------------------

test_that("POST /api/v1/identity sets repo-local Git identity", {
  repo <- .new_test_repo()
  api <- .test_api(repo$dir, repo$git)
  resp <- .api_req(
    api, "/api/v1/identity",
    method = "post", headers = .api_auth(api),
    body = list(name = "New Name", email = "new@example.com", scope = "local", status_version = .api_version(api))
  )
  expect_true(resp$json$ok)
  expect_equal(resp$json$data$name, "New Name")
})

test_that("POST /api/v1/restore discards unsaved changes in one tracked file", {
  repo <- .new_test_repo()
  writeLines("v1", file.path(repo$dir, "f.txt"))
  repo$run("add", "f.txt")
  repo$run("commit", "-q", "-m", "initial")
  writeLines("v2", file.path(repo$dir, "f.txt"))
  api <- .test_api(repo$dir, repo$git)

  resp <- .api_req(
    api, "/api/v1/restore",
    method = "post", headers = .api_auth(api),
    body = list(path = "f.txt", status_version = .api_version(api))
  )
  expect_true(resp$json$ok)
  expect_equal(readLines(file.path(repo$dir, "f.txt")), "v1")
})

test_that("POST /api/v1/trash moves an untracked file out of the working tree", {
  repo <- .new_test_repo()
  writeLines("scratch", file.path(repo$dir, "scratch.txt"))
  api <- .test_api(repo$dir, repo$git)
  testthat::local_mocked_bindings(.trash_file = function(full_path) list(ok = TRUE), .package = "gitneighbr")

  resp <- .api_req(
    api, "/api/v1/trash",
    method = "post", headers = .api_auth(api),
    body = list(path = "scratch.txt", status_version = .api_version(api))
  )
  expect_true(resp$json$ok)
})

test_that("POST /api/v1/ignore appends a .gitignore rule", {
  repo <- .new_test_repo()
  writeLines("junk", file.path(repo$dir, "junk.log"))
  api <- .test_api(repo$dir, repo$git)

  resp <- .api_req(
    api, "/api/v1/ignore",
    method = "post", headers = .api_auth(api),
    body = list(path = "junk.log", status_version = .api_version(api))
  )
  expect_true(resp$json$ok)
  expect_true(file.exists(file.path(repo$dir, ".gitignore")))
})

test_that("POST /api/v1/tag and /api/v1/push-tag create and push an annotated tag", {
  remote_dir <- withr::local_tempdir()
  git <- unname(Sys.which("git"))
  skip_if(!nzchar(git), "git not available")
  processx::run(git, c("init", "-q", "--bare", "-b", "main", remote_dir), error_on_status = TRUE)

  repo <- .new_test_repo()
  writeLines("v1", file.path(repo$dir, "f.txt"))
  repo$run("add", "f.txt")
  repo$run("commit", "-q", "-m", "initial")
  repo$run("remote", "add", "origin", remote_dir)
  repo$run("push", "-q", "-u", "origin", "main")
  api <- .test_api(repo$dir, repo$git)

  tagged <- .api_req(
    api, "/api/v1/tag",
    method = "post", headers = .api_auth(api),
    body = list(name = "v1.0.0", annotation = "First release", status_version = .api_version(api))
  )
  expect_true(tagged$json$ok)
  expect_equal(tagged$json$data$name, "v1.0.0")

  pushed <- .api_req(
    api, "/api/v1/push-tag",
    method = "post", headers = .api_auth(api),
    body = list(name = "v1.0.0", status_version = .api_version(api))
  )
  expect_true(pushed$json$ok)
})

test_that("POST /api/v1/update fast-forwards from a bare remote", {
  remote_dir <- withr::local_tempdir()
  git <- unname(Sys.which("git"))
  skip_if(!nzchar(git), "git not available")
  processx::run(git, c("init", "-q", "--bare", "-b", "main", remote_dir), error_on_status = TRUE)

  seed_dir <- withr::local_tempdir()
  seed_run <- function(...) processx::run(git, c("-C", seed_dir, ...), error_on_status = TRUE)
  seed_run("init", "-q", "-b", "main")
  seed_run("config", "user.email", "seed@example.com")
  seed_run("config", "user.name", "Seed")
  writeLines("seed", file.path(seed_dir, "seed.txt"))
  seed_run("add", "seed.txt")
  seed_run("commit", "-q", "-m", "seed")
  seed_run("remote", "add", "origin", remote_dir)
  seed_run("push", "-q", "-u", "origin", "main")

  dir <- withr::local_tempdir()
  processx::run(git, c("clone", "-q", remote_dir, dir), error_on_status = TRUE)
  run <- function(...) processx::run(git, c("-C", dir, ...), error_on_status = TRUE)

  writeLines("more", file.path(seed_dir, "more.txt"))
  seed_run("add", "more.txt")
  seed_run("commit", "-q", "-m", "more")
  seed_run("push", "-q", "origin", "main")

  api <- .test_api(dir, git)
  updated <- .api_req(
    api, "/api/v1/update",
    method = "post", headers = .api_auth(api),
    body = list(status_version = .api_version(api))
  )
  expect_true(updated$json$ok)
  expect_true(file.exists(file.path(dir, "more.txt")))
})

test_that("POST /api/v1/commit saves exactly the selected files, validating the summary first", {
  repo <- .new_test_repo()
  writeLines("keep me", file.path(repo$dir, "keep.txt"))
  writeLines("leave me", file.path(repo$dir, "skip.txt"))
  api <- .test_api(repo$dir, repo$git)

  bad_summary <- .api_req(
    api, "/api/v1/commit",
    method = "post", headers = .api_auth(api),
    body = list(paths = list("keep.txt"), summary = "ab", status_version = .api_version(api))
  )
  expect_false(bad_summary$json$ok)
  expect_equal(bad_summary$json$error$code, "INVALID_SUMMARY")

  saved <- .api_req(
    api, "/api/v1/commit",
    method = "post", headers = .api_auth(api),
    body = list(paths = list("keep.txt"), summary = "Add keep.txt", status_version = .api_version(api))
  )
  expect_true(saved$json$ok)
  expect_match(saved$json$data$sha, "^[0-9a-f]+$")

  status <- .api_req(api, "/api/v1/status", headers = .api_auth(api))
  expect_equal(status$json$data$untracked_count, 1L)
})

test_that("POST /api/v1/refresh-remote and /api/v1/push send a local commit to a bare remote", {
  remote_dir <- withr::local_tempdir()
  git <- unname(Sys.which("git"))
  skip_if(!nzchar(git), "git not available")
  processx::run(git, c("init", "-q", "--bare", "-b", "main", remote_dir), error_on_status = TRUE)

  seed_dir <- withr::local_tempdir()
  seed_run <- function(...) processx::run(git, c("-C", seed_dir, ...), error_on_status = TRUE)
  seed_run("init", "-q", "-b", "main")
  seed_run("config", "user.email", "seed@example.com")
  seed_run("config", "user.name", "Seed")
  writeLines("seed", file.path(seed_dir, "seed.txt"))
  seed_run("add", "seed.txt")
  seed_run("commit", "-q", "-m", "seed")
  seed_run("remote", "add", "origin", remote_dir)
  seed_run("push", "-q", "-u", "origin", "main")

  dir <- withr::local_tempdir()
  processx::run(git, c("clone", "-q", remote_dir, dir), error_on_status = TRUE)
  run <- function(...) processx::run(git, c("-C", dir, ...), error_on_status = TRUE)
  run("config", "user.email", "test@example.com")
  run("config", "user.name", "Test")
  writeLines("local", file.path(dir, "local.txt"))
  run("add", "local.txt")
  run("commit", "-q", "-m", "local commit")

  api <- .test_api(dir, git)
  refreshed <- .api_req(
    api, "/api/v1/refresh-remote",
    method = "post", headers = .api_auth(api), body = list(status_version = .api_version(api))
  )
  expect_true(refreshed$json$ok)
  expect_equal(refreshed$json$data$ahead, 1L)

  pushed <- .api_req(
    api, "/api/v1/push",
    method = "post", headers = .api_auth(api), body = list(status_version = .api_version(api))
  )
  expect_true(pushed$json$ok)
  expect_equal(pushed$json$data$pushed_count, 1L)
})

test_that("POST /api/v1/init and /api/v1/clone onboard a non-repository directory", {
  git <- unname(Sys.which("git"))
  skip_if(!nzchar(git), "git not available")

  dir <- withr::local_tempdir()
  api <- .test_api(dir, git)

  before <- .api_req(api, "/api/v1/status", headers = .api_auth(api))
  expect_equal(before$json$data$primary_state, "NOT_REPOSITORY")

  initialized <- .api_req(
    api, "/api/v1/init",
    method = "post", headers = .api_auth(api), body = list(status_version = .api_version(api))
  )
  expect_true(initialized$json$ok)

  again <- .api_req(
    api, "/api/v1/init",
    method = "post", headers = .api_auth(api), body = list(status_version = .api_version(api))
  )
  expect_false(again$json$ok)
  expect_equal(again$json$error$code, "ALREADY_A_REPOSITORY")

  remote_dir <- withr::local_tempdir()
  processx::run(git, c("init", "-q", "--bare", "-b", "main", remote_dir), error_on_status = TRUE)
  seed_dir <- withr::local_tempdir()
  seed_run <- function(...) processx::run(git, c("-C", seed_dir, ...), error_on_status = TRUE)
  seed_run("init", "-q", "-b", "main")
  seed_run("config", "user.email", "seed@example.com")
  seed_run("config", "user.name", "Seed")
  writeLines("seed", file.path(seed_dir, "seed.txt"))
  seed_run("add", "seed.txt")
  seed_run("commit", "-q", "-m", "seed")
  seed_run("remote", "add", "origin", remote_dir)
  seed_run("push", "-q", "-u", "origin", "main")

  dest <- file.path(withr::local_tempdir(), "project")
  clone_api <- .test_api(dest, git)
  cloned <- .api_req(
    clone_api, "/api/v1/clone",
    method = "post", headers = .api_auth(clone_api),
    body = list(url = remote_dir, status_version = .api_version(clone_api))
  )
  expect_true(cloned$json$ok)
  expect_true(file.exists(file.path(dest, "seed.txt")))
})

test_that("POST /api/v1/publish connects a local-only repository to a remote and pushes", {
  remote_dir <- withr::local_tempdir()
  git <- unname(Sys.which("git"))
  skip_if(!nzchar(git), "git not available")
  processx::run(git, c("init", "-q", "--bare", "-b", "main", remote_dir), error_on_status = TRUE)

  repo <- .new_test_repo()
  writeLines("hello", file.path(repo$dir, "hello.txt"))
  repo$run("add", "hello.txt")
  repo$run("commit", "-q", "-m", "initial commit")
  api <- .test_api(repo$dir, repo$git)

  published <- .api_req(
    api, "/api/v1/publish",
    method = "post", headers = .api_auth(api),
    body = list(url = remote_dir, status_version = .api_version(api))
  )
  expect_true(published$json$ok)
  expect_equal(published$json$data$pushed_count, 1L)

  status <- .api_req(api, "/api/v1/status", headers = .api_auth(api))
  expect_equal(status$json$data$primary_state, "READY")
})

test_that("GitHub endpoints: connect, status, disconnect, pull-request, release, and releases", {
  # `.git_create_pull_request()`/`.git_create_release()` push real branches
  # and tags with `processx::run()` -- a fake `github.com` remote URL would
  # make that push fail before the (mocked) GitHub API is ever called. A
  # real local bare remote lets the push succeed; `.parse_github_slug()` is
  # mocked separately so the repo is still recognized as a GitHub remote.
  remote_dir <- withr::local_tempdir()
  git <- unname(Sys.which("git"))
  skip_if(!nzchar(git), "git not available")
  processx::run(git, c("init", "-q", "--bare", "-b", "main", remote_dir), error_on_status = TRUE)

  repo <- .new_test_repo()
  writeLines("v1", file.path(repo$dir, "f.txt"))
  repo$run("add", "f.txt")
  repo$run("commit", "-q", "-m", "initial")
  repo$run("remote", "add", "origin", remote_dir)
  repo$run("push", "-q", "-u", "origin", "main")
  repo$run("tag", "-a", "v1.0.0", "-m", "First release")
  api <- .test_api(repo$dir, repo$git)

  fake_slug <- list(owner = "example-org", repo = "example-repo", host = "github.com", is_enterprise = FALSE)
  testthat::local_mocked_bindings(
    .github_get_user = function(...) list(login = "octocat", name = "The Octocat"),
    .parse_github_slug = function(...) fake_slug,
    .package = "gitneighbr"
  )
  connected <- .api_req(
    api, "/api/v1/github/connect",
    method = "post", headers = .api_auth(api),
    body = list(token = "ghp_faketoken")
  )
  expect_true(connected$json$ok)
  expect_true(connected$json$data$connected)

  bad_token <- .api_req(
    api, "/api/v1/github/connect",
    method = "post", headers = .api_auth(api),
    body = list(token = "")
  )
  expect_false(bad_token$json$ok)
  expect_equal(bad_token$json$error$code, "INVALID_SUMMARY")

  status <- .api_req(api, "/api/v1/github/status", headers = .api_auth(api))
  expect_true(status$json$ok)
  expect_true(status$json$data$is_github)

  testthat::local_mocked_bindings(
    .github_api_call = function(path, method = "GET", ...) {
      if (grepl("/pulls$", path)) {
        list(ok = TRUE, data = list(number = 7L, html_url = "https://github.com/example-org/example-repo/pull/7", title = "t", state = "open"))
      } else if (grepl("/releases$", path) && identical(method, "POST")) {
        list(ok = TRUE, data = list(id = 42L, html_url = "https://github.com/example-org/example-repo/releases/tag/v1.0.0"))
      } else if (grepl("/releases", path)) {
        list(ok = TRUE, data = list(list(id = 42L, tag_name = "v1.0.0", name = "v1.0.0", html_url = "u", draft = FALSE, prerelease = FALSE, published_at = "2024-01-01")))
      } else {
        list(ok = FALSE, error = "unexpected call")
      }
    },
    .package = "gitneighbr"
  )

  pr <- .api_req(
    api, "/api/v1/pull-request",
    method = "post", headers = .api_auth(api),
    body = list(target_branch = "main", title = "A title", status_version = .api_version(api))
  )
  expect_true(pr$json$ok)
  expect_equal(pr$json$data$pr_number, 7L)

  release <- .api_req(
    api, "/api/v1/github/release",
    method = "post", headers = .api_auth(api),
    body = list(tag_name = "v1.0.0", status_version = .api_version(api))
  )
  expect_true(release$json$ok)
  expect_equal(release$json$data$tag_name, "v1.0.0")

  releases <- .api_req(api, "/api/v1/github/releases", headers = .api_auth(api))
  expect_true(releases$json$ok)
  expect_equal(releases$json$data$releases$tag_name[[1]], "v1.0.0")

  disc <- .api_req(api, "/api/v1/github/disconnect", method = "post", headers = .api_auth(api))
  expect_true(disc$json$ok)
  expect_false(disc$json$data$connected)
})

test_that("GET /api/v1/github/releases reports GITHUB_API_ERROR when the API call fails", {
  repo <- .new_test_repo()
  repo$run("remote", "add", "origin", "https://github.com/example-org/example-repo.git")
  api <- .test_api(repo$dir, repo$git)
  testthat::local_mocked_bindings(
    .github_api_call = function(...) list(ok = FALSE, error = "boom"),
    .package = "gitneighbr"
  )
  resp <- .api_req(api, "/api/v1/github/releases", headers = .api_auth(api))
  expect_false(resp$json$ok)
  expect_equal(resp$json$error$code, "GITHUB_API_ERROR")
})
