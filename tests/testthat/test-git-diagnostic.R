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

local_repo_with_remote <- function(env = parent.frame()) {
  git <- unname(Sys.which("git"))
  skip_if(!nzchar(git), "git not available")

  remote_dir <- withr::local_tempdir(.local_envir = env)
  processx::run(git, c("init", "-q", "--bare", "-b", "main", remote_dir), error_on_status = TRUE)

  origin_dir <- withr::local_tempdir(.local_envir = env)
  origin_run <- function(...) processx::run(git, c("-C", origin_dir, ...), error_on_status = TRUE)
  origin_run("init", "-q", "-b", "main")
  origin_run("config", "user.email", "seed@example.com")
  origin_run("config", "user.name", "Seed")
  writeLines("seed", file.path(origin_dir, "seed.txt"))
  origin_run("add", "seed.txt")
  origin_run("commit", "-q", "-m", "seed commit")
  origin_run("remote", "add", "origin", remote_dir)
  origin_run("push", "-q", "-u", "origin", "main")

  dir <- withr::local_tempdir(.local_envir = env)
  processx::run(git, c("clone", "-q", remote_dir, dir), error_on_status = TRUE)
  run <- function(...) processx::run(git, c("-C", dir, ...), error_on_status = TRUE)
  run("config", "user.email", "test@example.com")
  run("config", "user.name", "Test")

  list(dir = dir, git = git, run = run, remote_dir = remote_dir, origin_dir = origin_dir, origin_run = origin_run)
}

fake_session_state <- function() {
  env <- new.env(parent = emptyenv())
  env$auth_required <- FALSE
  env
}

test_that(".diagnostic_report describes a diverged repository", {
  repo <- local_repo_with_remote()

  # Advance the remote past what this clone has.
  writeLines("remote-only", file.path(repo$origin_dir, "remote.txt"))
  repo$origin_run("add", "remote.txt")
  repo$origin_run("commit", "-q", "-m", "remote-only commit")
  repo$origin_run("push", "-q", "origin", "main")

  # Give the local clone its own unique commit too.
  writeLines("local-only", file.path(repo$dir, "local.txt"))
  repo$run("add", "local.txt")
  repo$run("commit", "-q", "-m", "local-only commit")
  repo$run("fetch", "-q", "origin")

  report <- .diagnostic_report(repo$dir, repo$git, fake_session_state())

  expect_equal(report$primary_state, "DIVERGED")
  expect_equal(report$branch, "main")
  expect_equal(report$upstream, "origin/main")
  expect_equal(report$ahead, 1L)
  expect_equal(report$behind, 1L)
  expect_length(report$conflicted_files, 0L)
  expect_true(length(report$commands) >= 3L)
  for (cmd in report$commands) {
    expect_true(startsWith(cmd$command, "git "))
    expect_true(is.numeric(cmd$exit_status) || is.na(cmd$exit_status))
  }

  expect_null(report$in_progress_operation)
  headings <- vapply(report$recovery, `[[`, character(1), "heading")
  expect_setequal(headings, c("Combine with a merge", "Combine with a rebase"))
  merge_block <- report$recovery[[which(headings == "Combine with a merge")]]
  expect_equal(unlist(merge_block$commands), "git merge origin/main")
  rebase_block <- report$recovery[[which(headings == "Combine with a rebase")]]
  expect_equal(unlist(rebase_block$commands), "git rebase origin/main")
})

test_that(".diagnostic_report lists conflicted files without attempting a merge", {
  repo <- local_git_repo()
  writeLines("base", file.path(repo$dir, "file.txt"))
  repo$run("add", "file.txt")
  repo$run("commit", "-q", "-m", "base commit")

  repo$run("checkout", "-q", "-b", "feature")
  writeLines("feature change", file.path(repo$dir, "file.txt"))
  repo$run("commit", "-q", "-am", "feature commit")

  repo$run("checkout", "-q", "main")
  writeLines("main change", file.path(repo$dir, "file.txt"))
  repo$run("commit", "-q", "-am", "main commit")

  processx::run(repo$git, c("-C", repo$dir, "merge", "feature"), error_on_status = FALSE)

  report <- .diagnostic_report(repo$dir, repo$git, fake_session_state())

  expect_equal(report$primary_state, "CONFLICTED")
  expect_equal(report$conflicted_files, "file.txt")

  expect_equal(report$in_progress_operation, "merge")
  headings <- vapply(report$recovery, `[[`, character(1), "heading")
  expect_setequal(headings, c("Finish the merge", "Cancel the merge"))
  abort_block <- report$recovery[[which(headings == "Cancel the merge")]]
  expect_equal(unlist(abort_block$commands), "git merge --abort")
})

test_that(".diagnostic_report offers rebase-specific recovery commands during a stopped rebase", {
  repo <- local_git_repo()
  writeLines("base", file.path(repo$dir, "file.txt"))
  repo$run("add", "file.txt")
  repo$run("commit", "-q", "-m", "base commit")

  repo$run("checkout", "-q", "-b", "feature")
  writeLines("feature change", file.path(repo$dir, "file.txt"))
  repo$run("commit", "-q", "-am", "feature commit")

  repo$run("checkout", "-q", "main")
  writeLines("main change", file.path(repo$dir, "file.txt"))
  repo$run("commit", "-q", "-am", "main commit")

  repo$run("checkout", "-q", "feature")
  processx::run(repo$git, c("-C", repo$dir, "rebase", "main"), error_on_status = FALSE)

  report <- .diagnostic_report(repo$dir, repo$git, fake_session_state())

  expect_equal(report$primary_state, "CONFLICTED")
  expect_equal(report$in_progress_operation, "rebase")
  headings <- vapply(report$recovery, `[[`, character(1), "heading")
  expect_setequal(headings, c("Continue the rebase", "Cancel the rebase"))
  abort_block <- report$recovery[[which(headings == "Cancel the rebase")]]
  expect_equal(unlist(abort_block$commands), "git rebase --abort")
})

test_that(".recovery_commands offers no guidance for ordinary states", {
  expect_length(.recovery_commands("READY", NULL, NULL), 0L)
  expect_length(.recovery_commands("CHANGES_ONLY", NULL, NULL), 0L)
})

test_that(".diagnostic_report sanitizes stdout/stderr and never touches the working tree", {
  repo <- local_git_repo()
  writeLines("hello", file.path(repo$dir, "file.txt"))
  repo$run("add", "file.txt")
  repo$run("commit", "-q", "-m", "initial commit")

  before <- .git_status(repo$dir, repo$git)
  report <- .diagnostic_report(repo$dir, repo$git, fake_session_state())
  after <- .git_status(repo$dir, repo$git)

  expect_equal(report$primary_state, "NO_UPSTREAM")
  expect_identical(before, after)
  for (cmd in report$commands) {
    expect_false(grepl("password|token|secret", cmd$stdout, ignore.case = TRUE))
  }
})
