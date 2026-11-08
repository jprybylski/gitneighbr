# gitneighbr_serve() in this process: its argument checks, and one real run
# that binds, prints the ready line and stops after its first service cycle.
# (test-managed-serve.R drives the same contract through a serve.R child.)

.serve_args <- function(repo, ...) {
  utils::modifyList(
    list(
      repo = repo$dir, port = gitneighbr:::.find_free_port(), git = repo$git,
      api_token = strrep("a", 32)
    ),
    list(...)
  )
}

.serve_error <- function(args) {
  tryCatch(
    {
      do.call(gitneighbr_serve, args)
      NA_character_
    },
    error = function(e) conditionMessage(e)
  )
}

test_that("gitneighbr_serve() refuses bad arguments before serving anything", {
  repo <- .new_test_repo()
  outside <- withr::local_tempdir()
  inside_policy <- file.path(repo$dir, "policy.json")
  writeLines('{"schema_version": 1}', inside_policy)
  cases <- list(
    list(list(host = "0.0.0.0"), "only supports binding"),
    list(list(port = "http"), "TCP port"),
    list(list(port = 70000L), "TCP port"),
    list(list(repo = file.path(outside, "missing")), "existing folder"),
    list(list(git = "git"), "absolute path"),
    list(list(api_token = "short"), "at least 16"),
    list(list(shutdown_token = "short"), "at least 16"),
    list(list(api_token = NULL), "`api_token` is required"),
    list(list(pass_env = "GIT_DIR"), "other than GIT_"),
    list(list(pass_env = "not valid"), "other than GIT_"),
    list(list(git_config = "relative.gitconfig"), "absolute file path"),
    list(list(git_config = outside), "absolute file path"),
    list(list(git_config = file.path(repo$dir, "cfg")), "outside the folder"),
    list(list(policy_file = inside_policy), "outside the folder"),
    list(list(features = "local,teleport"), "Unknown feature"),
    list(list(features = "remote"), "must include \"local\""),
    list(list(git_exec_path = "libexec"), "existing absolute folder"),
    list(list(git_exec_path = file.path(outside, "missing")), "existing absolute folder")
  )
  for (case in cases) {
    args <- .serve_args(repo)
    for (name in names(case[[1]])) args[name] <- list(case[[1]][[name]])
    expect_match(.serve_error(args), case[[2]], fixed = TRUE, info = paste(names(case[[1]]), collapse = ","))
  }
})

test_that("gitneighbr_serve() binds, prints the ready line and stops when asked", {
  repo <- .new_test_repo()
  outside <- withr::local_tempdir()
  policy_file <- file.path(outside, "policy.json")
  jsonlite::write_json(list(schema_version = 1L, protected = list("runs/**")), policy_file, auto_unbox = TRUE)
  seen <- new.env()
  real_config <- gitneighbr:::.managed_config
  # A stop already requested ends the run after its first service cycle.
  local_mocked_bindings(.managed_config = function(...) {
    config <- real_config(...)
    config$control$stop_requested <- TRUE
    seen$config <- config
    config
  })
  args <- .serve_args(repo,
    base_path = "/apps/gitneighbr", shutdown_token = strrep("s", 32),
    features = "local,remote", git_config = file.path(outside, "gitconfig"),
    policy_file = policy_file, pass_env = "HTTPS_PROXY", git_exec_path = outside
  )
  out <- utils::capture.output(result <- do.call(gitneighbr_serve, args))

  expect_identical(result, 0L)
  line <- grep("^GITNEIGHBR_READY ", out, value = TRUE)
  expect_length(line, 1L)
  ready <- jsonlite::fromJSON(sub("^GITNEIGHBR_READY ", "", line))
  expect_identical(ready$url, sprintf("http://127.0.0.1:%d/apps/gitneighbr/", args$port))
  expect_identical(ready$pid, Sys.getpid())
  expect_identical(ready$version, as.character(utils::packageVersion("gitneighbr")))
  expect_false(grepl(args$api_token, line, fixed = TRUE))
  expect_false(grepl(args$shutdown_token, line, fixed = TRUE))
  expect_identical(seen$config$features, c("local", "remote"))
  expect_identical(seen$config$policy$protected, "runs/**")
})

test_that("without a git_config, a session uses a temporary one", {
  repo <- .new_test_repo()
  seen <- new.env()
  local_mocked_bindings(
    .serve_run = function(api, host, port, base_path, control) invisible(0L),
    .managed_git = function(git, config_file, ...) {
      seen$config_file <- config_file
      git
    }
  )
  expect_identical(do.call(gitneighbr_serve, .serve_args(repo)), 0L)
  expect_true(startsWith(normalizePath(dirname(seen$config_file)), normalizePath(tempdir())))
  expect_false(startsWith(seen$config_file, repo$dir))
})

test_that("routes name the features they need", {
  expect_identical(gitneighbr:::.route_features("api/v1/status"), "local")
  expect_identical(gitneighbr:::.route_features("api/v1/send"), "remote")
  expect_identical(gitneighbr:::.route_features("api/v1/push"), c("remote", "standalone"))
  expect_identical(gitneighbr:::.parse_features(NULL), "local")
  expect_identical(gitneighbr:::.parse_features(" local , remote,,local"), c("local", "remote"))
  expect_true(gitneighbr:::.is_absolute_path("C:\\Git\\cmd\\git.exe"))
  expect_false(gitneighbr:::.is_absolute_path("cmd/git.exe"))
})
