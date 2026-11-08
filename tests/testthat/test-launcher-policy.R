.example_policy <- function() {
  list(
    schema_version = 1L,
    groups = list(
      list(id = "sources", label = "Editable sources", description = "Recipes and reports you edit.", globs = list("recipe/**", "report/*.qmd")),
      list(id = "generated", label = "Generated", globs = list("OUTPUTS/", "**/*.log"))
    ),
    default_group = list(id = "other", label = "Other files"),
    protected = list("runs/**"),
    review = list(list(id = "data", label = "Study data", message = "May identify people.", globs = list("data/**"))),
    ignore_template = list(
      list(rule = "OUTPUTS/", reason = "Generated outputs; rebuilt from a run."),
      list(rule = "*.log", reason = "Logs.")
    ),
    max_diff_bytes = 5000L
  )
}

test_that("policy globs match folders, files and any depth", {
  expect_true(.glob_match("recipe/a.json", "recipe/**"))
  expect_true(.glob_match("recipe/x/y.json", "recipe/**"))
  expect_false(.glob_match("recipes/a.json", "recipe/**"))
  expect_true(.glob_match("report/r.qmd", "report/*.qmd"))
  expect_false(.glob_match("report/sub/r.qmd", "report/*.qmd"))
  expect_true(.glob_match("OUTPUTS/a/b.csv", "OUTPUTS/"))
  expect_false(.glob_match("OUTPUTS", "OUTPUTS/"))
  expect_true(.glob_match("a.log", "**/*.log"))
  expect_true(.glob_match("x/y/a.log", "**/*.log"))
  expect_true(.glob_match(".opncafyr/runs/r1/x", ".opncafyr/runs/**"))
  expect_true(.glob_match("a.b", "a.b"))
  expect_false(.glob_match("aXb", "a.b"))
  expect_true(.glob_match("f1.txt", "f?.txt"))
  expect_false(.glob_match("f/x.txt", "f?x.txt"))
})

test_that("an invalid policy is refused with the field named", {
  base <- .example_policy()
  bad_cases <- list(
    list(field = "schema_version", value = 2L, error = "schema_version"),
    list(field = "extra", value = TRUE, error = "unknown field"),
    list(field = "protected", value = list("/abs/**"), error = "relative"),
    list(field = "protected", value = list("../up/**"), error = "\\.\\. part"),
    list(field = "protected", value = list("a/[bc]"), error = "unsupported glob"),
    list(field = "max_diff_bytes", value = 1L, error = "max_diff_bytes")
  )
  for (case in bad_cases) {
    p <- base
    p[[case$field]] <- case$value
    expect_error(.validate_launcher_policy(p), case$error, info = case$field)
  }
  p <- base
  p$groups[[2]]$id <- "sources"
  expect_error(.validate_launcher_policy(p), "unique")
  p <- base
  p$ignore_template[[1]]$rule <- "a\nb"
  expect_error(.validate_launcher_policy(p), "one .gitignore line")
  expect_silent(.validate_launcher_policy(base))
})

test_that("changes are grouped and flagged as the policy says", {
  repo <- .new_test_repo()
  for (f in c("recipe/r.json", "OUTPUTS/o.csv", "data/d.csv", "runs/x.txt", "notes.txt")) {
    dir.create(dirname(file.path(repo$dir, f)), showWarnings = FALSE, recursive = TRUE)
    writeLines("x", file.path(repo$dir, f))
  }
  api <- .test_managed_api(repo$dir, policy = .example_policy())
  changes <- .api_req(api, "/api/v1/changes", headers = .api_auth(api))$json$data$changes
  row <- function(path) changes[changes$path == path, , drop = FALSE]
  expect_identical(row("recipe/r.json")$group, "sources")
  expect_identical(row("OUTPUTS/o.csv")$group, "generated")
  expect_identical(row("notes.txt")$group, "other")
  expect_true(row("runs/x.txt")$protected)
  expect_false(row("notes.txt")$protected)
  expect_identical(unlist(row("data/d.csv")$review), "data")

  status <- .api_req(api, "/api/v1/status", headers = .api_auth(api))$json$data
  expect_identical(status$layout$groups$id, c("sources", "generated", "other"))
  expect_null(status$layout$groups$globs)
  expect_false(status$layout$gitignore_exists)
})

test_that("working-tree actions refuse protected paths", {
  repo <- .new_test_repo()
  dir.create(file.path(repo$dir, "runs"))
  writeLines("v1", file.path(repo$dir, "runs", "x.txt"))
  repo$run("add", ".")
  repo$run("commit", "-q", "-m", "x")
  writeLines("v2", file.path(repo$dir, "runs", "x.txt"))
  writeLines("new", file.path(repo$dir, "runs", "y.txt"))
  api <- .test_managed_api(repo$dir, features = "local,restore,trash", policy = .example_policy())

  restore <- .api_req(api, "/api/v1/restore", method = "post", headers = .api_auth(api),
    body = list(status_version = .api_version(api), path = "runs/x.txt"))
  expect_identical(restore$json$error$code, "PATH_PROTECTED")
  trash <- .api_req(api, "/api/v1/trash", method = "post", headers = .api_auth(api),
    body = list(status_version = .api_version(api), path = "runs/y.txt"))
  expect_identical(trash$json$error$code, "PATH_PROTECTED")
  expect_identical(readLines(file.path(repo$dir, "runs", "x.txt")), "v2")
  expect_true(file.exists(file.path(repo$dir, "runs", "y.txt")))
})

test_that("init writes only the chosen template rules, and never over an existing .gitignore", {
  dir <- withr::local_tempdir()
  api <- .test_managed_api(dir, policy = .example_policy())
  status <- .api_req(api, "/api/v1/status", headers = .api_auth(api))$json
  expect_identical(status$data$layout$ignore_template$rule, c("OUTPUTS/", "*.log"))

  bogus <- .api_req(api, "/api/v1/init", method = "post", headers = .api_auth(api),
    body = list(status_version = status$status_version, ignore_rules = list("*")))
  expect_false(isTRUE(bogus$json$ok))

  init <- .api_req(api, "/api/v1/init", method = "post", headers = .api_auth(api),
    body = list(status_version = .api_version(api), ignore_rules = list("OUTPUTS/")))
  expect_true(isTRUE(init$json$ok))
  expect_true(init$json$data$gitignore_written)
  lines <- readLines(file.path(dir, ".gitignore"))
  expect_true("OUTPUTS/" %in% lines)
  expect_false("*.log" %in% lines)
  expect_true("# Generated outputs; rebuilt from a run." %in% lines)

  other <- withr::local_tempdir()
  writeLines("mine", file.path(other, ".gitignore"))
  api2 <- .test_managed_api(other, policy = .example_policy())
  refused <- .api_req(api2, "/api/v1/init", method = "post", headers = .api_auth(api2),
    body = list(status_version = .api_version(api2), ignore_rules = list("OUTPUTS/")))
  expect_identical(refused$json$error$code, "GITIGNORE_EXISTS")
  expect_identical(readLines(file.path(other, ".gitignore")), "mine")
  expect_false(dir.exists(file.path(other, ".git")))
})

test_that("every malformed policy part is refused", {
  base <- .example_policy()
  expect_error(.validate_launcher_policy(list(1, 2)), "JSON object")
  expect_error(.validate_launcher_policy(utils::modifyList(base, list(protected = "runs/**"))), "list of globs")
  p <- base
  p$protected <- list("")
  expect_error(.validate_launcher_policy(p), "non-empty string")
  p <- base
  p$groups[[1]]$id <- "Sources!"
  expect_error(.validate_launcher_policy(p), "short lower-case id")
  p <- base
  p$default_group <- list("other")
  expect_error(.validate_launcher_policy(p), "must be an object")
  p <- base
  p$review[[1]]$colour <- "red"
  expect_error(.validate_launcher_policy(p), "unknown field")
  expect_false(.glob_match("a.txt", character()))
  expect_null(.protected_path("runs/x", NULL))
  expect_null(.protected_path("notes.md", .validate_launcher_policy(base)))
})

test_that("only offered ignore rules are written, and never over a .gitignore", {
  dir <- withr::local_tempdir()
  policy <- .validate_launcher_policy(.example_policy())
  expect_identical(.write_ignore_template(dir, policy, list()), list(ok = TRUE, written = FALSE))
  expect_identical(.write_ignore_template(dir, policy, list("*"))$code, "COMMAND_FAILED")
  expect_false(file.exists(file.path(dir, ".gitignore")))
  expect_true(.write_ignore_template(dir, policy, list("*.log"))$written)
  expect_identical(readLines(file.path(dir, ".gitignore"))[3:4], c("# Logs.", "*.log"))
  expect_identical(.write_ignore_template(dir, policy, list("*.log"))$code, "GITIGNORE_EXISTS")
  # A .gitignore that appears while writing is kept, and the temporary file removed.
  other <- withr::local_tempdir()
  local_mocked_bindings(file.rename = function(from, to) {
    writeLines("theirs", to)
    FALSE
  }, .package = "base")
  expect_identical(.write_ignore_template(other, policy, list("*.log"))$code, "GITIGNORE_EXISTS")
  expect_identical(readLines(file.path(other, ".gitignore")), "theirs")
  expect_identical(list.files(other, all.files = TRUE, no.. = TRUE), ".gitignore")
})
