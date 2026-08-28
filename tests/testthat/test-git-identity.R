local_identity_repo <- function(env = parent.frame()) {
  git <- unname(Sys.which("git"))
  skip_if(!nzchar(git), "git not available")

  # Isolate from the developer's real ~/.gitconfig (and any system config)
  # so "identity unset" scenarios are actually unset, regardless of what
  # machine the tests run on.
  withr::local_envvar(
    GIT_CONFIG_GLOBAL = file.path(withr::local_tempdir(.local_envir = env), "no-such-gitconfig"),
    GIT_CONFIG_SYSTEM = file.path(withr::local_tempdir(.local_envir = env), "no-such-gitconfig-system"),
    .local_envir = env
  )

  dir <- withr::local_tempdir(.local_envir = env)
  run <- function(...) processx::run(git, c("-C", dir, ...), error_on_status = TRUE)
  run("init", "-q", "-b", "main")

  list(dir = dir, git = git, run = run)
}

test_that(".validate_identity_name accepts a real name and rejects blank/newline input", {
  expect_true(.validate_identity_name("Ada Lovelace"))
  expect_false(.validate_identity_name(""))
  expect_false(.validate_identity_name("   "))
  expect_false(.validate_identity_name(NULL))
  expect_false(.validate_identity_name("two\nlines"))
  expect_false(.validate_identity_name(strrep("x", 201)))
})

test_that(".validate_identity_email accepts a well-formed address and rejects malformed ones", {
  expect_true(.validate_identity_email("ada@example.com"))
  expect_false(.validate_identity_email("not-an-email"))
  expect_false(.validate_identity_email("ada@"))
  expect_false(.validate_identity_email(""))
  expect_false(.validate_identity_email(NULL))
  expect_false(.validate_identity_email("has space@example.com"))
})

test_that(".git_identity reports unset when no scope has a value", {
  repo <- local_identity_repo()
  identity <- .git_identity(repo$dir, repo$git)
  expect_null(identity$name)
  expect_null(identity$email)
  expect_false(identity$complete)
})

test_that(".git_identity reports the scope a value came from and completeness", {
  repo <- local_identity_repo()
  repo$run("config", "--local", "user.name", "Test User")
  repo$run("config", "--local", "user.email", "test@example.com")

  identity <- .git_identity(repo$dir, repo$git)
  expect_equal(identity$name, "Test User")
  expect_equal(identity$name_scope, "local")
  expect_equal(identity$email, "test@example.com")
  expect_equal(identity$email_scope, "local")
  expect_true(identity$complete)
})

test_that(".git_identity reports incomplete when only one of name/email is set", {
  repo <- local_identity_repo()
  repo$run("config", "--local", "user.email", "test@example.com")

  identity <- .git_identity(repo$dir, repo$git)
  expect_null(identity$name)
  expect_equal(identity$email, "test@example.com")
  expect_false(identity$complete)
})

test_that(".git_set_identity refuses an invalid name or email without touching config", {
  repo <- local_identity_repo()

  bad_name <- .git_set_identity(repo$dir, repo$git, name = "", email = "ada@example.com")
  expect_false(bad_name$ok)
  expect_equal(bad_name$code, "INVALID_NAME")

  bad_email <- .git_set_identity(repo$dir, repo$git, name = "Ada", email = "not-an-email")
  expect_false(bad_email$ok)
  expect_equal(bad_email$code, "INVALID_EMAIL")

  expect_false(.git_identity(repo$dir, repo$git)$complete)
})

test_that(".git_set_identity writes to the local scope on request, leaving global unset", {
  repo <- local_identity_repo()
  result <- .git_set_identity(repo$dir, repo$git, name = "Ada Lovelace", email = "ada@example.com", scope = "local")

  expect_true(result$ok)
  expect_equal(result$scope, "local")
  identity <- .git_identity(repo$dir, repo$git)
  expect_equal(identity$name_scope, "local")
  expect_true(identity$complete)

  global_value <- processx::run(
    repo$git, c("config", "--global", "--get", "user.name"),
    error_on_status = FALSE, timeout = 5
  )
  expect_false(identical(global_value$status, 0L))
})

test_that(".git_set_identity writes to the global scope by default", {
  repo <- local_identity_repo()
  result <- .git_set_identity(repo$dir, repo$git, name = "Ada Lovelace", email = "ada@example.com")

  expect_true(result$ok)
  expect_equal(result$scope, "global")
  identity <- .git_identity(repo$dir, repo$git)
  expect_equal(identity$name_scope, "global")
  expect_equal(identity$email_scope, "global")
})

test_that("doctor() flags a missing identity as advisory and a complete one as ok", {
  repo <- local_identity_repo()
  writeLines("hello", file.path(repo$dir, "file.txt"))

  missing <- utils::capture.output(result <- doctor(path = repo$dir, git = repo$git))
  expect_equal(result$checks$identity$status, "advisory")

  repo$run("config", "--local", "user.name", "Test User")
  repo$run("config", "--local", "user.email", "test@example.com")
  utils::capture.output(result2 <- doctor(path = repo$dir, git = repo$git))
  expect_equal(result2$checks$identity$status, "ok")
})

test_that(".status_notices reports IDENTITY_INCOMPLETE until name and email are both set", {
  repo <- local_identity_repo()
  # Commit as a bystander identity, distinct from the one under test, so
  # this isn't circular with the repo-local identity being asserted on.
  repo$run("-c", "user.name=Bystander", "-c", "user.email=bystander@example.com", "commit", "--allow-empty", "-q", "-m", "seed")

  status <- .git_status(repo$dir, repo$git)
  state <- new.env(parent = emptyenv())
  state$pending_tags <- character()
  state$pushed_tags <- character()

  before <- vapply(.status_notices(repo$dir, repo$git, status, state), `[[`, character(1), "code")
  expect_true("IDENTITY_INCOMPLETE" %in% before)

  repo$run("config", "--local", "user.name", "Test User")
  repo$run("config", "--local", "user.email", "test@example.com")
  after <- vapply(.status_notices(repo$dir, repo$git, status, state), `[[`, character(1), "code")
  expect_false("IDENTITY_INCOMPLETE" %in% after)
})

test_that("GET/POST /api/v1/identity read and write identity over HTTP", {
  skip_on_cran()
  skip_if_not_installed("httr2")
  repo <- local_identity_repo()
  writeLines("hello", file.path(repo$dir, "file.txt"))

  session <- open_repo(path = repo$dir, browse = FALSE)
  withr::defer(session$stop())

  full_url <- session$url(redact = FALSE)
  token <- sub(".*token=", "", full_url)
  base_url <- sub("#.*", "", full_url)

  unset <- httr2::request(paste0(base_url, "api/v1/identity")) |>
    httr2::req_auth_bearer_token(token) |>
    httr2::req_perform() |>
    httr2::resp_body_json()
  expect_true(unset$ok)
  expect_false(unset$data$complete)

  status <- httr2::request(paste0(base_url, "api/v1/status")) |>
    httr2::req_auth_bearer_token(token) |>
    httr2::req_perform() |>
    httr2::resp_body_json()

  set_result <- httr2::request(paste0(base_url, "api/v1/identity")) |>
    httr2::req_auth_bearer_token(token) |>
    httr2::req_body_json(list(
      name = "Ada Lovelace", email = "ada@example.com", scope = "local",
      status_version = status$status_version
    )) |>
    httr2::req_perform() |>
    httr2::resp_body_json()
  expect_true(set_result$ok)
  expect_equal(set_result$data$name, "Ada Lovelace")
  expect_equal(set_result$data$scope, "local")

  confirmed <- httr2::request(paste0(base_url, "api/v1/identity")) |>
    httr2::req_auth_bearer_token(token) |>
    httr2::req_perform() |>
    httr2::resp_body_json()
  expect_true(confirmed$data$complete)
  expect_equal(confirmed$data$email, "ada@example.com")
})
