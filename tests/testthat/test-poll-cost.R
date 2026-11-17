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

  # An untracked file is counted again only when it changes.
  writeLines(c("u1", "u2"), file.path(repo$dir, "untracked.txt"))
  expect_identical(sort(added()), c(2L, 3L))
  read <- 0L
  real_stats <- .untracked_file_stats
  testthat::local_mocked_bindings(.untracked_file_stats = function(...) {
    read <<- read + 1L
    real_stats(...)
  })
  expect_identical(sort(added()), c(2L, 3L))
  expect_identical(read, 0L)
  writeLines(c("u1", "u2", "u3", "u4"), file.path(repo$dir, "untracked.txt"))
  expect_identical(sort(added()), c(3L, 4L))
  expect_identical(read, 1L)
  unlink(file.path(repo$dir, "untracked.txt"))

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

test_that("a linked worktree's Git folders are found without Git, matching --git-path", {
  repo <- poll_repo("clean")
  linked <- file.path(withr::local_tempdir(), "linked")
  repo$run("worktree", "add", "-q", "-b", "side", linked)
  dirs <- .git_dirs(linked)
  git_path <- function(name) {
    out <- trimws(processx::run(repo$git, c("-C", linked, "rev-parse", "--git-path", name))$stdout)
    normalizePath(if (fs::is_absolute_path(out)) out else file.path(linked, out), mustWork = FALSE)
  }
  expect_identical(normalizePath(file.path(dirs$git_dir, "MERGE_HEAD"), mustWork = FALSE), git_path("MERGE_HEAD"))
  expect_identical(normalizePath(file.path(dirs$common_dir, "config")), git_path("config"))

  # The pointer and the shared config are part of the signatures.
  expect_true(any(grepl("config|", .repo_fingerprint(linked), fixed = TRUE)))
  writeLines("x", file.path(linked, "a.txt"))
  new_state <- function() {
    s <- .new_session_state()
    s$features <- setdiff(.STANDALONE_FEATURES, "github")
    s
  }
  session_state <- new_state()
  .status_payload(linked, repo$git, session_state)
  cached <- .status_payload(linked, repo$git, session_state)$data
  uncached <- withr::with_options(list(gitneighbr.poll_cache_ttl = 0), .status_payload(linked, repo$git, new_state()))$data
  expect_identical(cached, uncached)
  expect_identical(cached$branch, "side")
  expect_identical(cached$unstaged_count, 1L)
})

test_that("an unreadable .git falls back to asking Git", {
  dir <- withr::local_tempdir()
  expect_null(.git_dirs(dir))
  writeLines("not a pointer", file.path(dir, ".git"))
  expect_null(.git_dirs(dir))
  writeLines("gitdir: missing", file.path(dir, ".git"))
  expect_null(.git_dirs(dir))
  expect_null(.tags_fingerprint(dir))

  # In-progress detection still works through `rev-parse --git-path`.
  repo <- poll_repo("merge")
  testthat::local_mocked_bindings(.git_dirs = function(repo_root) NULL)
  expect_identical(.git_in_progress_operation(repo$dir, repo$git), "merge")
  expect_null(.git_in_progress_operation(withr::local_tempdir(), repo$git))
})

test_that("the memo is off without a session environment, with a zero TTL, and under the lock", {
  calls <- 0L
  compute <- function() {
    calls <<- calls + 1L
    calls
  }
  expect_null(.poll_cache(list()))
  expect_identical(.poll_memo(list(), "x", 1, compute), 1L)
  expect_null(.poll_cache_clear(list()))

  session_state <- .new_session_state()
  expect_identical(.poll_memo(session_state, "x", 1, compute), 2L)
  expect_identical(.poll_memo(session_state, "x", 1, compute), 2L)
  expect_identical(.poll_memo(session_state, "x", 2, compute), 3L)
  withr::with_options(list(gitneighbr.poll_cache_ttl = 0), expect_identical(.poll_memo(session_state, "x", 2, compute), 4L))
  session_state$mutation_lock <- TRUE
  expect_identical(.poll_memo(session_state, "x", 2, compute), 5L)
  session_state$mutation_lock <- FALSE
  expect_identical(.poll_memo(session_state, "x", 2, compute), 3L)
  .poll_cache_clear(session_state)
  expect_identical(.poll_memo(session_state, "x", 2, compute), 6L)

  # Git that isn't there is asked again on the next poll, never remembered.
  expect_false(.cached_git_available(file.path(withr::local_tempdir(), "no-git"), session_state))
  expect_null(session_state$poll_cache$git_available)
})

test_that("the config signature follows the files this Git reads", {
  repo <- poll_repo("clean")
  global <- withr::local_tempfile(fileext = ".gitconfig")
  writeLines("", global)
  withr::local_envvar(GIT_CONFIG_GLOBAL = global, GIT_CONFIG_SYSTEM = "")
  expect_true(any(startsWith(.config_fingerprint(repo$dir, repo$git), paste0(global, "|"))))
  withr::local_envvar(GIT_CONFIG_GLOBAL = "", XDG_CONFIG_HOME = "/xdg")
  expect_true(any(startsWith(.config_fingerprint(repo$dir, repo$git), "/xdg/git/config|")))
  managed <- .managed_git(repo$git, global)
  expect_true(any(startsWith(.outside_config_files(managed), normalizePath(global, winslash = "/"))))
})

test_that("GitHub status from the poll's status names the same branch and remote", {
  repo <- poll_repo("clean")
  repo$run("remote", "add", "origin", "https://github.com/example/repo.git")
  withr::local_envvar(GITHUB_PAT = "", GITHUB_TOKEN = "", GH_TOKEN = "")
  session_state <- .new_session_state()
  status <- .git_status(repo$dir, repo$git)
  from_status <- .github_api_status(repo$dir, repo$git, session_state, gh_bin = "", status = status)
  direct <- .github_api_status(repo$dir, repo$git, session_state, gh_bin = "")
  expect_identical(from_status, direct)
  expect_identical(from_status$repo, "repo")

  repo$run("checkout", "-q", "--detach", "HEAD")
  detached <- .git_status(repo$dir, repo$git)
  expect_identical(
    .github_api_status(repo$dir, repo$git, session_state, gh_bin = "", status = detached),
    .github_api_status(repo$dir, repo$git, session_state, gh_bin = "")
  )
})
