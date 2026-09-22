# A minimal, real HTTP server for testing R/github-api.R's `.github_api_call()`
# and its callers, which use `curl::curl_fetch_memory()` -- a synchronous,
# libcurl-level call that deadlocks against an httpuv server started
# in-process (curl blocks the one R thread httpuv would need to dispatch the
# request handler on). Spawning it as a real, separate OS process sidesteps
# that entirely, the same way `R/serve.R` spawns the real app server.
#
# Routes, matched on request path:
#  - /ok      -> 200 {"ok": true, "method": "<METHOD>", "body": <parsed request body or null>}
#  - /error   -> 422 {"message": "intentional test failure"}
#  - /not-json -> 200 with a non-JSON body
#  (anything else -> 404)
.local_test_http_server <- function(env = parent.frame()) {
  port <- gitneighbr:::.find_free_port()
  script <- c(
    "library(httpuv)",
    "app <- list(call = function(req) {",
    "  body_raw <- req$rook.input$read()",
    "  path <- req$PATH_INFO",
    "  if (startsWith(path, '/error-no-message')) {",
    "    list(status = 500L, headers = list('Content-Type' = 'application/json'), body = '{\"errors\": [\"bad\"]}')",
    "  } else if (startsWith(path, '/error')) {",
    "    list(status = 422L, headers = list('Content-Type' = 'application/json'),",
    "         body = '{\"message\": \"intentional test failure\"}')",
    "  } else if (identical(path, '/ok')) {",
    "    parsed_body <- if (length(body_raw) > 0) jsonlite::fromJSON(rawToChar(body_raw), simplifyVector = FALSE) else NULL",
    "    payload <- list(ok = TRUE, method = req$REQUEST_METHOD, body = parsed_body)",
    "    list(status = 200L, headers = list('Content-Type' = 'application/json'),",
    "         body = jsonlite::toJSON(payload, auto_unbox = TRUE, null = 'null'))",
    "  } else if (identical(path, '/not-json')) {",
    "    list(status = 200L, headers = list('Content-Type' = 'text/plain'), body = 'not json at all')",
    "  } else if (identical(path, '/user')) {",
    "    list(status = 200L, headers = list('Content-Type' = 'application/json'),",
    "         body = '{\"login\": \"octocat\", \"name\": \"The Octocat\", \"avatar_url\": \"a\", \"html_url\": \"h\"}')",
    "  } else if (grepl('/branches/', path, fixed = TRUE)) {",
    "    list(status = 200L, headers = list('Content-Type' = 'application/json'), body = '{\"protected\": true}')",
    "  } else {",
    "    list(status = 404L, headers = list('Content-Type' = 'text/plain'), body = 'not found')",
    "  }",
    "})",
    sprintf("startServer('127.0.0.1', %dL, app)", port),
    "while (TRUE) { later::run_now(1); Sys.sleep(0.05) }"
  )
  process <- processx::process$new(
    command = file.path(R.home("bin"), "Rscript"),
    args = c("--vanilla", "-e", paste(script, collapse = "\n")),
    stdout = "|", stderr = "|", cleanup = TRUE, cleanup_tree = TRUE
  )
  deadline <- Sys.time() + 10
  ready <- FALSE
  while (Sys.time() < deadline) {
    if (!process$is_alive()) {
      stop("test HTTP server exited during startup:\n", process$read_all_error(), call. = FALSE)
    }
    if (.port_is_open("127.0.0.1", port)) {
      ready <- TRUE
      break
    }
    Sys.sleep(0.1)
  }
  if (!ready) {
    process$kill_tree()
    stop("test HTTP server did not become ready in time.", call. = FALSE)
  }
  withr::defer(process$kill_tree(), envir = env)
  sprintf("http://127.0.0.1:%d", port)
}
