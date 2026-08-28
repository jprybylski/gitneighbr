local_git_repo <- function(env = parent.frame()) {
  git <- unname(Sys.which("git"))
  skip_if(!nzchar(git), "git not available")

  dir <- withr::local_tempdir(.local_envir = env)
  run <- function(...) processx::run(git, c("-C", dir, ...), error_on_status = TRUE)
  run("init", "-q", "-b", "main")
  run("config", "user.email", "test@example.com")
  run("config", "user.name", "Test")

  list(dir = dir, git = git, run = run)
}

# A fake `ssh-add` that always exits with a given status, for testing
# `.ssh_agent_status()` without depending on the machine's real SSH agent.
local_fake_ssh_add <- function(status, stdout = "", env = parent.frame()) {
  skip_on_os("windows")
  path <- withr::local_tempfile(.local_envir = env, fileext = "")
  writeLines(c(
    "#!/bin/sh",
    if (nzchar(stdout)) paste0("echo '", stdout, "'"),
    paste0("exit ", status)
  ), path)
  Sys.chmod(path, "0755")
  path
}

test_that(".git_remote_transport classifies https, ssh, git, and scp-like URLs", {
  expect_equal(.git_remote_transport("https://github.com/user/repo.git"), "https")
  expect_equal(.git_remote_transport("http://example.com/repo.git"), "https")
  expect_equal(.git_remote_transport("ssh://git@github.com/user/repo.git"), "ssh")
  expect_equal(.git_remote_transport("git@github.com:user/repo.git"), "ssh")
  expect_equal(.git_remote_transport("git://example.com/repo.git"), "git")
  expect_equal(.git_remote_transport("file:///tmp/repo"), "other")
  expect_null(.git_remote_transport(NULL))
  expect_null(.git_remote_transport(""))
})

test_that(".platform_name returns one of the recognized platform strings", {
  expect_true(.platform_name() %in% c("windows", "macos", "linux", "other"))
})

test_that(".https_helper_guidance and .ssh_agent_guidance give platform-specific text", {
  for (platform in c("windows", "macos", "linux", "other")) {
    https_msg <- .https_helper_guidance(platform)
    ssh_msg <- .ssh_agent_guidance(platform)
    expect_true(nzchar(https_msg))
    expect_true(nzchar(ssh_msg))
  }
  expect_match(.https_helper_guidance("windows"), "Credential Manager")
  expect_match(.https_helper_guidance("macos"), "osxkeychain")
  expect_match(.https_helper_guidance("linux"), "libsecret")
  expect_match(.ssh_agent_guidance("macos"), "ssh-add")
})

test_that(".ssh_agent_status reports keys loaded when ssh-add exits 0", {
  fake <- local_fake_ssh_add(0L, stdout = "2048 SHA256:abc user@host (ED25519)")
  result <- .ssh_agent_status(ssh_add = fake)
  expect_true(result$available)
  expect_true(result$running)
  expect_true(result$has_keys)
  expect_equal(result$key_count, 1L)
  expect_null(result$detail)
})

test_that(".ssh_agent_status reports an empty agent when ssh-add exits 1", {
  fake <- local_fake_ssh_add(1L)
  result <- .ssh_agent_status(ssh_add = fake)
  expect_true(result$running)
  expect_false(result$has_keys)
  expect_match(result$detail, "no keys loaded")
})

test_that(".ssh_agent_status reports no agent reachable when ssh-add exits 2", {
  fake <- local_fake_ssh_add(2L)
  result <- .ssh_agent_status(ssh_add = fake)
  expect_false(result$running)
  expect_false(result$has_keys)
  expect_match(result$detail, "No SSH agent")
})

test_that(".ssh_agent_status handles a missing ssh-add executable", {
  result <- .ssh_agent_status(ssh_add = "")
  expect_false(result$available)
  expect_false(result$has_keys)
})

test_that(".git_credential_diagnosis flags a missing HTTPS credential helper", {
  repo <- local_git_repo()
  withr::local_envvar(c(HOME = withr::local_tempdir(), XDG_CONFIG_HOME = withr::local_tempdir(), GIT_CONFIG_NOSYSTEM = "1"))
  repo$run("remote", "add", "origin", "https://github.com/example/repo.git")

  diagnosis <- .git_credential_diagnosis(repo$dir, repo$git)
  expect_equal(diagnosis$transport, "https")
  expect_equal(diagnosis$checks$credential_helper$status, "fail")
  expect_length(diagnosis$guidance, 1L)
})

test_that(".git_credential_diagnosis is quiet when a credential helper is configured", {
  repo <- local_git_repo()
  withr::local_envvar(c(HOME = withr::local_tempdir(), XDG_CONFIG_HOME = withr::local_tempdir(), GIT_CONFIG_NOSYSTEM = "1"))
  repo$run("remote", "add", "origin", "https://github.com/example/repo.git")
  repo$run("config", "credential.helper", "store")

  diagnosis <- .git_credential_diagnosis(repo$dir, repo$git)
  expect_equal(diagnosis$checks$credential_helper$status, "ok")
  expect_length(diagnosis$guidance, 0L)
})

test_that(".git_credential_diagnosis flags an SSH remote with no usable agent", {
  repo <- local_git_repo()
  repo$run("remote", "add", "origin", "git@github.com:example/repo.git")
  fake <- local_fake_ssh_add(2L)
  original <- .ssh_agent_status
  testthat::local_mocked_bindings(.ssh_agent_status = function() original(ssh_add = fake))

  diagnosis <- .git_credential_diagnosis(repo$dir, repo$git)
  expect_equal(diagnosis$transport, "ssh")
  expect_equal(diagnosis$checks$ssh_agent$status, "fail")
  expect_length(diagnosis$guidance, 1L)
})

test_that(".git_credential_diagnosis reports no remote as a failing check", {
  repo <- local_git_repo()
  diagnosis <- .git_credential_diagnosis(repo$dir, repo$git)
  expect_null(diagnosis$transport)
  expect_equal(diagnosis$checks$remote$status, "fail")
})

test_that(".git_credential_diagnosis detects an expired/revoked credential hint from stderr", {
  repo <- local_git_repo()
  repo$run("remote", "add", "origin", "https://github.com/example/repo.git")
  repo$run("config", "credential.helper", "store")

  diagnosis <- .git_credential_diagnosis(repo$dir, repo$git, stderr_text = "remote: Bad credentials")
  expect_equal(diagnosis$checks$token$status, "fail")
  expect_true(any(grepl("expired or revoked", diagnosis$guidance)))
})

test_that(".git_credential_diagnosis's checks are plain lists, serializable as JSON", {
  # `.doctor_check()` objects carry class `gitneighbr_doctor_check` for
  # `print.gitneighbr_doctor_report()`'s benefit; jsonlite refuses to
  # serialize a classed object with no registered `asJSON` method,
  # so this diagnosis (which crosses the HTTP boundary, unlike doctor()'s
  # own report) must strip that class before returning.
  repo <- local_git_repo()
  withr::local_envvar(c(HOME = withr::local_tempdir(), XDG_CONFIG_HOME = withr::local_tempdir(), GIT_CONFIG_NOSYSTEM = "1"))
  repo$run("remote", "add", "origin", "https://github.com/example/repo.git")

  diagnosis <- .git_credential_diagnosis(repo$dir, repo$git)
  for (check in diagnosis$checks) {
    expect_null(attr(check, "class"))
  }
  # Exercises the same serializer `.build_api()` registers (spec Sec 14's
  # JSON envelope), rather than a generic `jsonlite::toJSON()` a real
  # request would never actually go through.
  serialize <- (function(...) reqres::format_json(..., auto_unbox = TRUE, null = "null"))()
  expect_no_error(serialize(.ok_envelope(diagnosis)))
})

test_that(".diagnosis_for_failure only attaches a diagnosis for AUTH_REQUIRED", {
  repo <- local_git_repo()
  repo$run("remote", "add", "origin", "https://github.com/example/repo.git")

  expect_null(.diagnosis_for_failure(repo$dir, repo$git, "REMOTE_UNREACHABLE", "fatal: could not resolve host"))
  diagnosis <- .diagnosis_for_failure(repo$dir, repo$git, "AUTH_REQUIRED", "fatal: Authentication failed")
  expect_equal(diagnosis$transport, "https")
})
