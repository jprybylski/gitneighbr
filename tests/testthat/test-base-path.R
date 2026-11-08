test_that(".normalize_base_path() accepts plain paths and adds the slashes", {
  expect_identical(.normalize_base_path(NULL), "/")
  expect_identical(.normalize_base_path(""), "/")
  expect_identical(.normalize_base_path("/"), "/")
  expect_identical(.normalize_base_path("apps/gitneighbr"), "/apps/gitneighbr/")
  expect_identical(.normalize_base_path("/apps//gitneighbr/"), "/apps/gitneighbr/")
  expect_identical(.normalize_base_path(" /a.b~c-d_e/ "), "/a.b~c-d_e/")
})

test_that(".normalize_base_path() refuses anything that isn't a plain URL path", {
  for (bad in list("/apps/../etc/", "/./", "/apps/<x>/", "/a b/", "http://x/", "/a?b/", NA_character_, 1, c("/a/", "/b/"))) {
    expect_error(.normalize_base_path(bad), "base_path")
  }
})

test_that(".prepare_www() leaves the root untouched and adds <base href> under a prefix", {
  www <- withr::local_tempdir()
  writeLines(c("<!doctype html>", "<html>", "  <head>", "    <title>x</title>", "  </head>", "</html>"), file.path(www, "index.html"))
  dir.create(file.path(www, "assets"))
  writeLines("x", file.path(www, "assets", "a.js"))

  expect_identical(.prepare_www(www, "/"), www)

  served <- .prepare_www(www, "/apps/gitneighbr/")
  withr::defer(unlink(served, recursive = TRUE))
  expect_false(identical(served, www))
  index <- readLines(file.path(served, "index.html"))
  expect_identical(index[[4]], '    <base href="/apps/gitneighbr/" />')
  expect_true(file.exists(file.path(served, "assets", "a.js")))
  expect_false(any(grepl("<base", readLines(file.path(www, "index.html")))))
})

test_that(".prepare_www() refuses an index.html without <head>", {
  www <- withr::local_tempdir()
  writeLines("<html></html>", file.path(www, "index.html"))
  expect_error(.prepare_www(www, "/apps/x/"), "no <head>")
})

test_that("the shipped bundle uses only relative asset and API URLs", {
  www <- system.file("www", package = "gitneighbr")
  skip_if(!nzchar(www), "installed www/ not found")
  files <- list.files(www, pattern = "\\.(html|js|css)$", recursive = TRUE, full.names = TRUE)
  expect_gt(length(files), 0L)
  for (f in files) {
    text <- paste(readLines(f, warn = FALSE), collapse = "\n")
    expect_false(grepl("[\"'`(]/(api|assets)/", text), info = f)
    expect_false(grepl("(href|src)=\"/[^/]", text), info = f)
  }
})

test_that("every route answers both at the root and under the base path", {
  repo <- .new_test_repo()
  api <- .test_api(repo$dir, repo$git, www_dir = system.file("www", package = "gitneighbr"), base_path = "/apps/gitneighbr/")

  for (prefix in c("", "/apps/gitneighbr")) {
    health <- .api_req(api, paste0(prefix, "/api/v1/health"))
    expect_identical(health$status, 200L)
    status <- .api_req(api, paste0(prefix, "/api/v1/status"), headers = .api_auth(api))
    expect_identical(status$status, 200L)
    expect_true(isTRUE(status$json$ok))
    commit <- .api_req(
      api, paste0(prefix, "/api/v1/commit"),
      method = "post", headers = .api_auth(api), body = list(status_version = -1)
    )
    expect_identical(commit$json$error$code, "STATE_CHANGED")
  }
  expect_identical(.api_req(api, "/other/api/v1/health")$status, 404L)
})
