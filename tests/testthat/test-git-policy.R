test_that(".default_repo_policy returns expected standard defaults", {
  defaults <- .default_repo_policy()
  expect_false(defaults$has_policy_file)
  expect_null(defaults$policy_file_path)
  expect_true(defaults$valid)
  expect_false(defaults$require_pull_request)
  expect_equal(defaults$protected_branches, character(0))
  expect_equal(defaults$default_tag_prefix, "v")
  expect_false(defaults$require_version_tags)
  expect_false(defaults$disallow_untracked_trash)
  expect_equal(defaults$pr_branch_prefix, "update/")
  expect_null(defaults$github_host)
  expect_null(defaults$github_api_url)
})

test_that(".read_repo_policy reads .gitneighbr.json when present", {
  dir <- withr::local_tempdir()
  config <- list(
    require_pull_request = TRUE,
    protected_branches = c("main", "production", "release/*"),
    default_tag_prefix = "rel-",
    require_version_tags = TRUE,
    disallow_untracked_trash = TRUE,
    pr_branch_prefix = "patch/",
    github_host = "github.company.internal",
    github_api_url = "https://github.company.internal/api/v3/"
  )
  writeLines(jsonlite::toJSON(config, auto_unbox = TRUE), file.path(dir, ".gitneighbr.json"))

  policy <- .read_repo_policy(dir)
  expect_true(policy$has_policy_file)
  expect_equal(policy$policy_file_path, ".gitneighbr.json")
  expect_true(policy$valid)
  expect_true(policy$require_pull_request)
  expect_equal(policy$protected_branches, c("main", "production", "release/*"))
  expect_equal(policy$default_tag_prefix, "rel-")
  expect_true(policy$require_version_tags)
  expect_true(policy$disallow_untracked_trash)
  expect_equal(policy$pr_branch_prefix, "patch/")
  expect_equal(policy$github_host, "github.company.internal")
  expect_equal(policy$github_api_url, "https://github.company.internal/api/v3")
})

test_that(".read_repo_policy falls back to .github/gitneighbr.json", {
  dir <- withr::local_tempdir()
  gh_dir <- file.path(dir, ".github")
  dir.create(gh_dir)
  config <- list(require_pull_request = TRUE)
  writeLines(jsonlite::toJSON(config, auto_unbox = TRUE), file.path(gh_dir, "gitneighbr.json"))

  policy <- .read_repo_policy(dir)
  expect_true(policy$has_policy_file)
  expect_true(policy$require_pull_request)
})

test_that(".read_repo_policy handles malformed JSON gracefully", {
  dir <- withr::local_tempdir()
  writeLines("NOT_VALID_JSON{{{", file.path(dir, ".gitneighbr.json"))

  policy <- .read_repo_policy(dir)
  expect_true(policy$has_policy_file)
  expect_false(policy$valid)
  expect_true(nzchar(policy$error))
  expect_false(policy$require_pull_request) # fallback defaults preserved
})

test_that(".is_branch_protected matches exact and glob patterns", {
  policy <- list(
    protected_branches = c("main", "master", "release/*", "v[0-9]*")
  )

  expect_true(.is_branch_protected("main", policy))
  expect_true(.is_branch_protected("master", policy))
  expect_true(.is_branch_protected("release/1.0", policy))
  expect_true(.is_branch_protected("release/2026-spring", policy))
  expect_false(.is_branch_protected("feature/login", policy))
  expect_false(.is_branch_protected("bugfix-123", policy))
  expect_false(.is_branch_protected(NULL, policy))

  # github_protected flag overrides
  expect_true(.is_branch_protected("feature/login", policy, github_protected = TRUE))
})
