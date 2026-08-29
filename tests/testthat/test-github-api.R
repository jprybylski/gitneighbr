test_that(".parse_github_slug correctly parses various GitHub remote URLs", {
  # HTTPS
  expect_equal(
    .parse_github_slug("https://github.com/jprybylski/gitneighbr.git"),
    list(owner = "jprybylski", repo = "gitneighbr", host = "github.com", is_enterprise = FALSE)
  )
  expect_equal(
    .parse_github_slug("https://github.com/posit-dev/plumber2"),
    list(owner = "posit-dev", repo = "plumber2", host = "github.com", is_enterprise = FALSE)
  )

  # SSH scp-like syntax
  expect_equal(
    .parse_github_slug("git@github.com:jprybylski/gitneighbr.git"),
    list(owner = "jprybylski", repo = "gitneighbr", host = "github.com", is_enterprise = FALSE)
  )
  expect_equal(
    .parse_github_slug("git@github.com:owner-name/repo_name"),
    list(owner = "owner-name", repo = "repo_name", host = "github.com", is_enterprise = FALSE)
  )

  # SSH URL scheme
  expect_equal(
    .parse_github_slug("ssh://git@github.com/jprybylski/gitneighbr.git"),
    list(owner = "jprybylski", repo = "gitneighbr", host = "github.com", is_enterprise = FALSE)
  )

  # GitHub Enterprise Server & Cloud
  expect_equal(
    .parse_github_slug("https://github.corp.internal/org/project.git"),
    list(owner = "org", repo = "project", host = "github.corp.internal", is_enterprise = TRUE)
  )
  expect_equal(
    .parse_github_slug("git@github.company.com:org/repo.git"),
    list(owner = "org", repo = "repo", host = "github.company.com", is_enterprise = TRUE)
  )
  expect_equal(
    .parse_github_slug("https://my-org.ghe.com/team/app.git"),
    list(owner = "team", repo = "app", host = "my-org.ghe.com", is_enterprise = TRUE)
  )

  # Policy-configured custom host
  custom_policy <- list(github_host = "git.internal.example")
  expect_equal(
    .parse_github_slug("git@git.internal.example:dept/tool.git", policy = custom_policy),
    list(owner = "dept", repo = "tool", host = "git.internal.example", is_enterprise = TRUE)
  )

  # Non-GitHub URLs
  expect_null(.parse_github_slug("https://gitlab.com/owner/repo.git"))
  expect_null(.parse_github_slug("git@gitlab.com:owner/repo.git"))
  expect_null(.parse_github_slug("https://example.com/foo.git"))
  expect_null(.parse_github_slug(NULL))
  expect_null(.parse_github_slug(""))
})

test_that(".github_api_base dynamically calculates base endpoint", {
  # Public GitHub
  gh_slug <- list(host = "github.com", is_enterprise = FALSE)
  expect_equal(.github_api_base(gh_slug), "https://api.github.com")

  # GHE Cloud
  ghec_slug <- list(host = "my-org.ghe.com", is_enterprise = TRUE)
  expect_equal(.github_api_base(ghec_slug), "https://api.my-org.ghe.com")

  # GHE Server (on-prem)
  ghes_slug <- list(host = "github.corp.internal", is_enterprise = TRUE)
  expect_equal(.github_api_base(ghes_slug), "https://github.corp.internal/api/v3")

  # Policy override
  policy_override <- list(github_api_url = "https://custom.api.internal/v3/")
  expect_equal(.github_api_base(ghes_slug, policy = policy_override), "https://custom.api.internal/v3")

  # Env var override
  withr::with_envvar(c(GITHUB_API_URL = "https://env.api.internal/v3"), {
    expect_equal(.github_api_base(gh_slug), "https://env.api.internal/v3")
  })
})

test_that(".get_github_token respects precedence order and enterprise vars", {
  # 1. Session state token takes priority
  session_state <- new.env(parent = emptyenv())
  session_state$github_token <- "session_secret_token_123"

  withr::with_envvar(c(GITHUB_PAT = "env_token_456"), {
    tok <- .get_github_token(session_state)
    expect_equal(tok$token, "session_secret_token_123")
    expect_equal(tok$source, "session")
  })

  # 2. Environment variable when session token is NULL
  session_empty <- new.env(parent = emptyenv())
  session_empty$github_token <- NULL

  withr::with_envvar(c(GITHUB_PAT = "env_pat_token"), {
    tok <- .get_github_token(session_empty)
    expect_equal(tok$token, "env_pat_token")
    expect_equal(tok$source, "env")
  })

  withr::with_envvar(c(GITHUB_PAT = "", GITHUB_TOKEN = "env_gh_token", GH_TOKEN = ""), {
    tok <- .get_github_token(session_empty)
    expect_equal(tok$token, "env_gh_token")
    expect_equal(tok$source, "env")
  })

  # 3. Enterprise environment variables
  withr::with_envvar(c(GITHUB_ENTERPRISE_TOKEN = "ghe_token_xyz", GITHUB_PAT = "gh_pat"), {
    tok <- .get_github_token(session_empty, host = "github.corp.internal", is_enterprise = TRUE)
    expect_equal(tok$token, "ghe_token_xyz")
    expect_equal(tok$source, "env")
  })
})

test_that(".github_api_call refuses empty or NULL token with clear message", {
  res <- .github_api_call("/user", token = NULL)
  expect_false(res$ok)
  expect_equal(res$status_code, 401L)
  expect_match(res$error, "authentication required", ignore.case = TRUE)
})

test_that(".github_api_status reports repository and token diagnostic state", {
  git <- unname(Sys.which("git"))
  skip_if(!nzchar(git), "git not available")

  dir <- withr::local_tempdir()
  processx::run(git, c("-C", dir, "init", "-q", "-b", "main"), error_on_status = TRUE)
  processx::run(git, c("-C", dir, "remote", "add", "origin", "https://github.com/my-org/my-project.git"), error_on_status = TRUE)

  state <- new.env(parent = emptyenv())
  state$github_token <- NULL

  # With empty env vars and no gh CLI for clean test
  withr::with_envvar(c(GITHUB_PAT = "", GITHUB_TOKEN = "", GH_TOKEN = ""), {
    status <- .github_api_status(dir, git, session_state = state, gh_bin = "")
    expect_true(status$is_github)
    expect_false(status$is_enterprise)
    expect_equal(status$host, "github.com")
    expect_equal(status$api_base, "https://api.github.com")
    expect_equal(status$owner, "my-org")
    expect_equal(status$repo, "my-project")
    expect_false(status$connected)
    expect_null(status$user)
  })
})
