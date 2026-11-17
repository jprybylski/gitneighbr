# Issue #50: the 2-second status poll must stay cheap. Every Git process
# costs tens of milliseconds on Windows, so a steady poll (status with the
# changed-file list, no identity) starts at most one: the `git status`.

# A Git wrapper that logs every start, then runs the real Git.
local_counting_git <- function(env = parent.frame()) {
  skip_on_os("windows")
  git <- unname(Sys.which("git"))
  skip_if(!nzchar(git), "git not available")
  dir <- withr::local_tempdir(.local_envir = env)
  log <- file.path(dir, "starts.log")
  wrapper <- file.path(dir, "git")
  writeLines(c(
    "#!/bin/sh",
    sprintf("printf '%%s\\n' \"$*\" >> '%s'", log),
    sprintf("exec '%s' \"$@\"", git)
  ), wrapper)
  Sys.chmod(wrapper, "755")
  list(
    bin = wrapper,
    starts = function() if (file.exists(log)) readLines(log) else character(),
    reset = function() unlink(log)
  )
}

# Repositories in each state the payload has to describe.
poll_repo <- function(state, env = parent.frame()) {
  repo <- .new_test_repo(env)
  write <- function(name, text) writeLines(text, file.path(repo$dir, name))
  commit <- function(msg) {
    repo$run("add", "-A")
    repo$run("commit", "-q", "-m", msg)
  }
  if (state == "unborn") {
    write("first.txt", "not committed yet")
    return(repo)
  }
  write("a.txt", c("one", "two"))
  write("b.txt", "bee")
  commit("initial")
  switch(state,
    clean = NULL,
    dirty = {
      write("a.txt", c("one", "two", "three"))
      write("b.txt", "staged")
      repo$run("add", "b.txt")
      repo$run("mv", "a.txt", "renamed.txt")
    },
    untracked = {
      dir.create(file.path(repo$dir, "sub dir"))
      write("sub dir/new file.txt", c("x", "y"))
    },
    ahead_behind = {
      repo$run("branch", "other")
      write("a.txt", "ours")
      commit("ours")
      repo$run("checkout", "-q", "other")
      write("b.txt", "theirs")
      commit("theirs")
      repo$run("checkout", "-q", "main")
      repo$run("branch", "--set-upstream-to=other")
    },
    detached = repo$run("checkout", "-q", "--detach", "HEAD"),
    merge = {
      repo$run("checkout", "-q", "-b", "other")
      write("a.txt", "theirs")
      commit("theirs")
      repo$run("checkout", "-q", "main")
      write("a.txt", "ours")
      commit("ours")
      processx::run(repo$git, c("-C", repo$dir, "merge", "other"), error_on_status = FALSE)
    },
    rebase = {
      repo$run("checkout", "-q", "-b", "other")
      write("a.txt", "theirs")
      commit("theirs")
      repo$run("checkout", "-q", "main")
      write("a.txt", "ours")
      commit("ours")
      processx::run(repo$git, c("-C", repo$dir, "rebase", "other"), error_on_status = FALSE)
    }
  )
  repo
}

POLL_STATES <- c("clean", "dirty", "untracked", "ahead_behind", "detached", "unborn", "merge", "rebase")

# A managed session over `git`, served in-process.
counting_managed_api <- function(repo, git, env = parent.frame()) {
  config <- withr::local_tempfile(.local_envir = env, fileext = ".gitconfig")
  managed <- .managed_config(features = "local")
  .test_api(repo$dir, .managed_git(git$bin, config), managed = managed, env = env)
}

test_that("a steady managed poll starts at most one Git process", {
  git <- local_counting_git()
  for (state in POLL_STATES) {
    repo <- poll_repo(state)
    api <- counting_managed_api(repo, git)
    warm <- .api_req(api, "/api/v1/status?include=changes", headers = .api_auth(api))
    expect_identical(warm$status, 200L, info = state)
    git$reset()
    poll <- .api_req(api, "/api/v1/status?include=changes", headers = .api_auth(api))
    expect_identical(poll$status, 200L, info = state)
    starts <- git$starts()
    expect_lte(length(starts), 1L)
    expect_true(all(grepl(" status --porcelain=v2 ", starts)), info = paste(state, paste(starts, collapse = "\n")))
  }
})

test_that("a steady standalone poll without GitHub starts at most one Git process", {
  git <- local_counting_git()
  for (state in POLL_STATES) {
    repo <- poll_repo(state)
    session_state <- .new_session_state()
    session_state$features <- setdiff(.STANDALONE_FEATURES, "github")
    first <- .status_payload(repo$dir, git$bin, session_state)
    .git_changes(repo$dir, git$bin, fields = first$fields, session_state = session_state)
    git$reset()
    again <- .status_payload(repo$dir, git$bin, session_state)
    .git_changes(repo$dir, git$bin, fields = again$fields, session_state = session_state)
    expect_lte(length(git$starts()), 1L)
  }
})

test_that("cached polls give exactly the payloads an uncached poll gives", {
  git <- unname(Sys.which("git"))
  skip_if(!nzchar(git), "git not available")
  for (state in POLL_STATES) {
    for (managed in c(FALSE, TRUE)) {
      repo <- poll_repo(state)
      git_bin <- if (managed) .managed_git(git, withr::local_tempfile(fileext = ".gitconfig")) else git
      new_state <- function() {
        s <- .new_session_state()
        s$managed <- managed
        s$features <- if (managed) "local" else setdiff(.STANDALONE_FEATURES, "github")
        s
      }
      poll <- function(s) {
        p <- .status_payload(repo$dir, git_bin, s)
        list(data = p$data, changes = if (!is.null(p$fields)) .git_changes(repo$dir, git_bin, fields = p$fields, session_state = s))
      }
      cached_state <- new_state()
      poll(cached_state)
      cached <- poll(cached_state)
      uncached <- withr::with_options(list(gitneighbr.poll_cache_ttl = 0), poll(new_state()))
      info <- paste(state, if (managed) "managed" else "standalone")
      expect_identical(cached, uncached, info = info)
      # And the same list `GET api/v1/changes` builds on its own.
      if (!is.null(uncached$changes)) {
        expect_identical(uncached$changes, .git_changes(repo$dir, git_bin), info = info)
      }
    }
  }
  # The states really are what they claim to be.
  expect_identical(.status_payload(poll_repo("merge")$dir, git, .new_session_state())$data$in_progress_operation, "merge")
  expect_identical(.status_payload(poll_repo("rebase")$dir, git, .new_session_state())$data$in_progress_operation, "rebase")
  expect_identical(.status_payload(poll_repo("unborn")$dir, git, .new_session_state())$data$head, "")
})

test_that("the poll memo notices changes made outside gitneighbr", {
  repo <- poll_repo("clean")
  session_state <- .new_session_state()
  session_state$features <- setdiff(.STANDALONE_FEATURES, "github")
  codes <- function() vapply(.status_payload(repo$dir, repo$git, session_state)$data$notices, `[[`, character(1), "code")
  added <- function() {
    p <- .status_payload(repo$dir, repo$git, session_state)
    vapply(.git_changes(repo$dir, repo$git, fields = p$fields, session_state = session_state), `[[`, integer(1), "added")
  }

  # A setting changed in a terminal.
  expect_false("SIGNING_ENABLED" %in% codes())
  repo$run("config", "commit.gpgsign", "true")
  expect_true("SIGNING_ENABLED" %in% codes())

  # A file that is already changed gets edited again: same status, new counts.
  writeLines(c("one", "two", "three"), file.path(repo$dir, "a.txt"))
  expect_identical(added(), 1L)
  writeLines(c("one", "two", "three", "four", "five"), file.path(repo$dir, "a.txt"))
  expect_identical(added(), 3L)

  # A commit made elsewhere.
  repo$run("-c", "commit.gpgsign=false", "commit", "-q", "-am", "elsewhere")
  expect_identical(.status_payload(repo$dir, repo$git, session_state)$data$head, trimws(repo$run("rev-parse", "HEAD")$stdout))
})

test_that("a managed poll refuses a filter attribute added after the check was cached", {
  repo <- poll_repo("clean")
  api <- .test_managed_api(repo$dir)
  status <- function() .api_req(api, "/api/v1/status?include=changes", headers = .api_auth(api))$json
  expect_identical(status()$data$primary_state, "NO_UPSTREAM")
  expect_identical(status()$data$primary_state, "NO_UPSTREAM")

  dir.create(file.path(repo$dir, "nested"))
  writeLines("*.bin filter=lfs", file.path(repo$dir, "nested", ".gitattributes"))
  refused <- status()
  expect_identical(refused$data$primary_state, "UNSUPPORTED_REPOSITORY")
  expect_identical(refused$data$repository$check$code, "FILTER_ATTRIBUTE")
  expect_null(refused$changes)
})

test_that("a mutation clears the poll memo", {
  repo <- poll_repo("clean")
  session_state <- .new_session_state()
  api <- .test_api(repo$dir, session_state = session_state)
  status <- function() .api_req(api, "/api/v1/status?include=changes", headers = .api_auth(api))$json
  before <- status()
  expect_identical(session_state$poll_cache$identity$value$name, "Test")

  # Same length as before, and quick enough to share a coarse timestamp:
  # clearing the memo when the lock is released is what makes it show.
  saved <- .api_req(api, "/api/v1/identity",
    method = "post", headers = .api_auth(api),
    body = list(name = "Tess", email = "test@example.com", scope = "local", status_version = before$status_version)
  )
  expect_true(saved$json$ok)
  expect_length(ls(session_state$poll_cache), 0L)
  status()
  expect_identical(session_state$poll_cache$identity$value$name, "Tess")
})

test_that("status?include=changes gives the same list as GET api/v1/changes", {
  repo <- poll_repo("dirty")
  api <- .test_api(repo$dir)
  combined <- .api_req(api, "/api/v1/status?include=changes", headers = .api_auth(api))$json
  separate <- .api_req(api, "/api/v1/changes", headers = .api_auth(api))$json
  expect_identical(combined$changes, separate$data$changes)
  expect_null(.api_req(api, "/api/v1/status", headers = .api_auth(api))$json$changes)
})
