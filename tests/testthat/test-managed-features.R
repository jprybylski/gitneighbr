# A managed session's feature profile and exact-folder rules, enforced by the
# server rather than only by the page.

test_that("every route of a disabled feature is refused before doing anything", {
  repo <- .new_test_repo()
  api <- .test_managed_api(repo$dir, features = "local")
  version <- .api_version(api)
  for (path in names(.ROUTE_FEATURES)) {
    method <- if (path %in% c("api/v1/github/status", "api/v1/github/releases", "api/v1/credential-diagnosis")) "get" else "post"
    resp <- .api_req(
      api, paste0("/", path),
      method = method, headers = .api_auth(api),
      body = if (method == "post") list(status_version = version)
    )
    expect_identical(resp$status, 403L, info = path)
    expect_identical(resp$json$error$code, "FEATURE_DISABLED", info = path)
  }
  # Local routes still work.
  expect_identical(.api_req(api, "/api/v1/changes", headers = .api_auth(api))$status, 200L)
})

test_that("a local-only session never starts gh or reads GitHub tokens while polling", {
  repo <- .new_test_repo()
  repo$run("remote", "add", "origin", "https://github.com/example/repo.git")
  bin <- withr::local_tempdir()
  log <- file.path(withr::local_tempdir(), "gh-calls")
  writeLines(c("#!/bin/sh", paste0("echo called >> '", log, "'"), "echo token"), file.path(bin, "gh"))
  Sys.chmod(file.path(bin, "gh"), "755")
  withr::local_envvar(PATH = paste(bin, Sys.getenv("PATH"), sep = .Platform$path.sep), GITHUB_TOKEN = "ghp_notarealtoken0000000000000000")

  api <- .test_managed_api(repo$dir, features = "local")
  for (i in 1:5) {
    status <- .api_req(api, "/api/v1/status", headers = .api_auth(api))
    expect_identical(status$status, 200L)
  }
  expect_false(file.exists(log))
  expect_false(status$json$data$github$connected)
  expect_true(status$json$data$managed)
  expect_identical(unlist(status$json$data$features), "local")
})

test_that("a managed session inside another repository shows the boundary and changes nothing", {
  repo <- .new_test_repo()
  sub <- file.path(repo$dir, "study")
  dir.create(sub)
  writeLines("x", file.path(sub, "x.txt"))
  api <- .test_managed_api(sub)

  status <- .api_req(api, "/api/v1/status", headers = .api_auth(api))
  expect_identical(status$json$data$primary_state, "ENCLOSING_REPOSITORY")
  expect_identical(status$json$data$repository$check$code, "ENCLOSING_REPOSITORY")
  version <- status$json$status_version

  changes <- .api_req(api, "/api/v1/changes", headers = .api_auth(api))
  expect_identical(changes$json$error$code, "ENCLOSING_REPOSITORY")
  commit <- .api_req(api, "/api/v1/commit", method = "post", headers = .api_auth(api),
    body = list(status_version = version, paths = list("x.txt"), summary = "x"))
  expect_identical(commit$json$error$code, "ENCLOSING_REPOSITORY")
  init <- .api_req(api, "/api/v1/init", method = "post", headers = .api_auth(api), body = list(status_version = version))
  expect_identical(init$json$error$code, "ENCLOSING_REPOSITORY")
  expect_false(dir.exists(file.path(sub, ".git")))
})

test_that("a managed session refuses an unsupported repository but explains why", {
  repo <- .new_test_repo()
  repo$run("config", "include.path", "/tmp/elsewhere")
  api <- .test_managed_api(repo$dir)
  status <- .api_req(api, "/api/v1/status", headers = .api_auth(api))
  expect_identical(status$json$data$primary_state, "UNSUPPORTED_REPOSITORY")
  expect_identical(status$json$data$repository$check$detail, "include.path")
  diff <- .api_req(api, "/api/v1/diff?path=a", headers = .api_auth(api))
  expect_identical(diff$json$error$code, "UNSUPPORTED_CONFIGURATION")
})

test_that("a managed session initializes only a folder that is in no repository", {
  dir <- withr::local_tempdir()
  api <- .test_managed_api(dir)
  status <- .api_req(api, "/api/v1/status", headers = .api_auth(api))
  expect_identical(status$json$data$primary_state, "NOT_REPOSITORY")
  init <- .api_req(api, "/api/v1/init", method = "post", headers = .api_auth(api),
    body = list(status_version = status$json$status_version))
  expect_true(isTRUE(init$json$ok))
  expect_true(dir.exists(file.path(dir, ".git")))
  # No hooks or sample files come from a template.
  expect_length(list.files(file.path(dir, ".git", "hooks")), 0L)

  again <- .api_req(api, "/api/v1/init", method = "post", headers = .api_auth(api),
    body = list(status_version = .api_version(api)))
  expect_identical(again$json$error$code, "ALREADY_REPOSITORY")
})

test_that("identity set in a managed session goes to the managed config, not the user's", {
  repo <- .new_test_repo()
  repo$run("config", "--unset", "user.name")
  repo$run("config", "--unset", "user.email")
  home <- withr::local_tempdir()
  withr::local_envvar(HOME = home)
  api <- .test_managed_api(repo$dir)
  resp <- .api_req(api, "/api/v1/identity", method = "post", headers = .api_auth(api),
    body = list(status_version = .api_version(api), name = "Ana", email = "ana@example.com", scope = "global"))
  expect_true(isTRUE(resp$json$ok))
  expect_false(file.exists(file.path(home, ".gitconfig")))
})

test_that("repository policy is enforced on the server, not only in the page", {
  repo <- .new_test_repo()
  writeLines('{"require_pull_request": true}', file.path(repo$dir, ".gitneighbr.json"))
  writeLines("x", file.path(repo$dir, "x.txt"))
  repo$run("add", ".")
  repo$run("commit", "-q", "-m", "x")
  api <- .test_api(repo$dir, repo$git)
  push <- .api_req(api, "/api/v1/push", method = "post", headers = .api_auth(api), body = list(status_version = .api_version(api)))
  expect_identical(push$json$error$code, "PULL_REQUEST_REQUIRED")

  writeLines('{"disallow_untracked_trash": true}', file.path(repo$dir, ".gitneighbr.json"))
  writeLines("y", file.path(repo$dir, "y.txt"))
  trash <- .api_req(api, "/api/v1/trash", method = "post", headers = .api_auth(api),
    body = list(status_version = .api_version(api), path = "y.txt"))
  expect_identical(trash$json$error$code, "TRASH_DISALLOWED")
  expect_true(file.exists(file.path(repo$dir, "y.txt")))
})
