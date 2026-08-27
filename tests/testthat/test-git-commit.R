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

test_that(".validate_commit_summary enforces the 3-72 character rule", {
  expect_false(.validate_commit_summary(NULL))
  expect_false(.validate_commit_summary(""))
  expect_false(.validate_commit_summary("ab"))
  expect_true(.validate_commit_summary("abc"))
  expect_true(.validate_commit_summary(strrep("a", 72)))
  expect_false(.validate_commit_summary(strrep("a", 73)))
})

test_that(".git_commit_selected rejects an invalid summary before touching the index", {
  repo <- local_git_repo()
  writeLines("hello", file.path(repo$dir, "a.txt"))

  result <- .git_commit_selected(repo$dir, repo$git, "a.txt", summary = "ab")
  expect_false(result$ok)
  expect_equal(result$code, "INVALID_SUMMARY")

  status <- .git_status(repo$dir, repo$git)
  expect_equal(status$staged_count, 0L)
})

test_that(".git_commit_selected rejects an empty selection", {
  repo <- local_git_repo()
  result <- .git_commit_selected(repo$dir, repo$git, character(), summary = "A fine summary")
  expect_false(result$ok)
  expect_equal(result$code, "EMPTY_SELECTION")
})

test_that(".git_commit_selected rejects a path with no current pending change", {
  repo <- local_git_repo()
  writeLines("hello", file.path(repo$dir, "a.txt"))

  result <- .git_commit_selected(repo$dir, repo$git, "does-not-exist.txt", summary = "A fine summary")
  expect_false(result$ok)
  expect_equal(result$code, "STATE_CHANGED")
})

test_that(".git_commit_selected commits exactly the selected untracked file", {
  repo <- local_git_repo()
  writeLines("keep me", file.path(repo$dir, "keep.txt"))
  writeLines("leave me alone", file.path(repo$dir, "skip.txt"))

  result <- .git_commit_selected(repo$dir, repo$git, "keep.txt", summary = "Add keep.txt")
  expect_true(result$ok)
  expect_match(result$sha, "^[0-9a-f]+$")

  status <- .git_status(repo$dir, repo$git)
  expect_equal(status$untracked_count, 1L)
  expect_equal(status$staged_count, 0L)
  expect_true(file.exists(file.path(repo$dir, "skip.txt")))

  log <- processx::run(repo$git, c("-C", repo$dir, "show", "-s", "--format=%s"), error_on_status = TRUE)
  expect_equal(trimws(log$stdout), "Add keep.txt")
})

test_that(".git_commit_selected does not include a previously staged, now-deselected path", {
  repo <- local_git_repo()
  writeLines("first", file.path(repo$dir, "a.txt"))
  repo$run("add", "a.txt")
  repo$run("commit", "-q", "-m", "initial commit")

  writeLines("first changed", file.path(repo$dir, "a.txt"))
  writeLines("brand new", file.path(repo$dir, "b.txt"))
  repo$run("add", "a.txt") # pre-stage a.txt, as if staged before gitneighbr started

  result <- .git_commit_selected(repo$dir, repo$git, "b.txt", summary = "Add b.txt only")
  expect_true(result$ok)

  committed <- processx::run(repo$git, c("-C", repo$dir, "show", "--name-only", "--format="), error_on_status = TRUE)
  files <- trimws(strsplit(committed$stdout, "\n")[[1]])
  files <- files[nzchar(files)]
  expect_equal(files, "b.txt")

  status <- .git_status(repo$dir, repo$git)
  expect_equal(status$staged_count, 0L)
  expect_equal(status$unstaged_count, 1L) # a.txt's edit is back to unstaged, not lost
  expect_equal(readLines(file.path(repo$dir, "a.txt")), "first changed")
})

test_that(".git_commit_selected stages a deletion for a selected removed path", {
  repo <- local_git_repo()
  writeLines("bye", file.path(repo$dir, "gone.txt"))
  repo$run("add", "gone.txt")
  repo$run("commit", "-q", "-m", "initial commit")
  file.remove(file.path(repo$dir, "gone.txt"))

  result <- .git_commit_selected(repo$dir, repo$git, "gone.txt", summary = "Remove gone.txt")
  expect_true(result$ok)
  expect_false(file.exists(file.path(repo$dir, "gone.txt")))

  status <- .git_status(repo$dir, repo$git)
  expect_true(status$has_changes == FALSE || status$staged_count == 0L)
})

test_that(".git_commit_selected refuses an empty commit and restores the index", {
  repo <- local_git_repo()
  writeLines("same", file.path(repo$dir, "a.txt"))
  repo$run("add", "a.txt")
  repo$run("commit", "-q", "-m", "initial commit")

  writeLines("different", file.path(repo$dir, "a.txt"))
  repo$run("add", "a.txt") # stage a change
  writeLines("same", file.path(repo$dir, "a.txt")) # then hand-revert it back to HEAD's content

  result <- .git_commit_selected(repo$dir, repo$git, "a.txt", summary = "Should be empty")
  expect_false(result$ok)
  expect_equal(result$code, "EMPTY_COMMIT")

  status <- .git_status(repo$dir, repo$git)
  expect_equal(status$staged_count, 1L) # index restored to its pre-operation (still-staged) state
})

test_that(".git_commit_selected uses the summary and details as the commit message", {
  repo <- local_git_repo()
  writeLines("hi", file.path(repo$dir, "a.txt"))

  result <- .git_commit_selected(
    repo$dir, repo$git, "a.txt",
    summary = "Add a.txt", details = "Some longer explanation."
  )
  expect_true(result$ok)

  log <- processx::run(repo$git, c("-C", repo$dir, "show", "-s", "--format=%B"), error_on_status = TRUE)
  expect_match(log$stdout, "Add a.txt", fixed = TRUE)
  expect_match(log$stdout, "Some longer explanation.", fixed = TRUE)
})

test_that(".git_commit_selected commits a renamed file selected as a unit", {
  repo <- local_git_repo()
  writeLines(c("line1", "line2", "line3"), file.path(repo$dir, "old.txt"))
  repo$run("add", "old.txt")
  repo$run("commit", "-q", "-m", "initial commit")

  file.rename(file.path(repo$dir, "old.txt"), file.path(repo$dir, "new.txt"))
  repo$run("add", "-A") # stage the rename so status reports one paired "RENAMED" entry

  result <- .git_commit_selected(repo$dir, repo$git, "new.txt", summary = "Rename old to new")
  expect_true(result$ok)

  committed <- processx::run(repo$git, c("-C", repo$dir, "show", "--name-status", "--format="), error_on_status = TRUE)
  expect_match(committed$stdout, "R100", fixed = TRUE)
})

test_that(".git_commit_selected refuses to commit while conflicts are present", {
  repo <- local_git_repo()
  writeLines("base", file.path(repo$dir, "a.txt"))
  repo$run("add", "a.txt")
  repo$run("commit", "-q", "-m", "initial commit")
  repo$run("checkout", "-q", "-b", "feature")
  writeLines("feature", file.path(repo$dir, "a.txt"))
  repo$run("commit", "-q", "-am", "feature change")
  repo$run("checkout", "-q", "main")
  writeLines("main", file.path(repo$dir, "a.txt"))
  repo$run("commit", "-q", "-am", "main change")
  processx::run(repo$git, c("-C", repo$dir, "merge", "feature"), error_on_status = FALSE)

  writeLines("unrelated", file.path(repo$dir, "b.txt"))
  result <- .git_commit_selected(repo$dir, repo$git, "b.txt", summary = "Try to save anyway")
  expect_false(result$ok)
  expect_equal(result$code, "CONFLICTS_PRESENT")
})
