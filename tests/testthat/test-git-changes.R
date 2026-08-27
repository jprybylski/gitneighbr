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

find_change <- function(changes, path) {
  Find(function(c) identical(c$path, path), changes)
}

test_that(".split_nul splits on NUL and drops the trailing empty record", {
  expect_equal(.split_nul(as.raw(c(0x61, 0x00, 0x62, 0x63, 0x00))), c("a", "bc"))
  expect_equal(.split_nul(raw()), character())
  expect_equal(.split_nul(as.raw(c(0x00, 0x00))), c("", ""))
})

test_that(".git_changes reports new, changed, deleted, and untracked files", {
  repo <- local_git_repo()
  writeLines("hello", file.path(repo$dir, "changed.txt"))
  writeLines("bye", file.path(repo$dir, "deleted.txt"))
  repo$run("add", "changed.txt", "deleted.txt")
  repo$run("commit", "-q", "-m", "initial commit")

  writeLines(c("hello", "again"), file.path(repo$dir, "changed.txt"))
  file.remove(file.path(repo$dir, "deleted.txt"))
  writeLines("brand new", file.path(repo$dir, "untracked.txt"))

  changes <- .git_changes(repo$dir, repo$git)
  expect_equal(length(changes), 3L)

  changed <- find_change(changes, "changed.txt")
  expect_equal(changed$state, "CHANGED")
  expect_equal(changed$added, 1L)
  expect_equal(changed$deleted, 0L)
  expect_false(changed$untracked)

  deleted <- find_change(changes, "deleted.txt")
  expect_equal(deleted$state, "DELETED")
  expect_false(deleted$untracked)

  new_file <- find_change(changes, "untracked.txt")
  expect_equal(new_file$state, "NEW")
  expect_true(new_file$untracked)
  expect_equal(new_file$added, 1L)
})

test_that(".git_changes reports a staged addition as NEW", {
  repo <- local_git_repo()
  writeLines("first commit", file.path(repo$dir, "a.txt"))
  repo$run("add", "a.txt")
  repo$run("commit", "-q", "-m", "initial commit")

  writeLines("added later", file.path(repo$dir, "b.txt"))
  repo$run("add", "b.txt")

  changes <- .git_changes(repo$dir, repo$git)
  added <- find_change(changes, "b.txt")
  expect_equal(added$state, "NEW")
  expect_false(added$untracked)
})

test_that(".git_changes reports a rename with combined staged+unstaged line stats", {
  repo <- local_git_repo()
  writeLines(c("line1", "line2", "line3"), file.path(repo$dir, "old.txt"))
  repo$run("add", "old.txt")
  repo$run("commit", "-q", "-m", "initial commit")

  file.rename(file.path(repo$dir, "old.txt"), file.path(repo$dir, "new.txt"))
  repo$run("add", "-A")
  writeLines(c("line1", "line2 changed", "line3", "line4"), file.path(repo$dir, "new.txt"))

  changes <- .git_changes(repo$dir, repo$git)
  entry <- find_change(changes, "new.txt")
  expect_equal(entry$state, "RENAMED")
  expect_equal(entry$old_path, "old.txt")
  expect_equal(entry$added, 2L)
  expect_equal(entry$deleted, 1L)
})

test_that(".git_changes flags binary files without line counts", {
  repo <- local_git_repo()
  writeBin(as.raw(c(1L, 2L, 3L)), file.path(repo$dir, "keep.txt"))
  repo$run("add", "keep.txt")
  repo$run("commit", "-q", "-m", "initial commit")

  writeBin(as.raw(0:5), file.path(repo$dir, "tracked.bin"))
  repo$run("add", "tracked.bin")
  repo$run("commit", "-q", "-m", "add binary")
  writeBin(as.raw(6:10), file.path(repo$dir, "tracked.bin"))

  writeBin(as.raw(0:3), file.path(repo$dir, "untracked.bin"))

  changes <- .git_changes(repo$dir, repo$git)

  tracked <- find_change(changes, "tracked.bin")
  expect_true(tracked$binary)
  expect_true(is.na(tracked$added))

  untracked <- find_change(changes, "untracked.bin")
  expect_true(untracked$binary)
  expect_true(untracked$untracked)
})

test_that(".git_diff returns unified diff text for a tracked file", {
  repo <- local_git_repo()
  writeLines(c("line1", "line2"), file.path(repo$dir, "a.txt"))
  repo$run("add", "a.txt")
  repo$run("commit", "-q", "-m", "initial commit")
  writeLines(c("line1", "line2 changed"), file.path(repo$dir, "a.txt"))

  result <- .git_diff(repo$dir, repo$git, "a.txt")
  expect_true(result$found)
  expect_false(result$binary)
  text <- paste(unlist(result$lines), collapse = "\n")
  expect_match(text, "-line2", fixed = TRUE)
  expect_match(text, "+line2 changed", fixed = TRUE)
})

test_that(".git_diff returns unified diff text for an untracked file", {
  repo <- local_git_repo()
  writeLines("first commit", file.path(repo$dir, "a.txt"))
  repo$run("add", "a.txt")
  repo$run("commit", "-q", "-m", "initial commit")
  writeLines(c("hello", "world"), file.path(repo$dir, "new.txt"))

  result <- .git_diff(repo$dir, repo$git, "new.txt")
  expect_true(result$found)
  text <- paste(unlist(result$lines), collapse = "\n")
  expect_match(text, "+hello", fixed = TRUE)
})

test_that(".git_diff on an unborn branch diffs against the empty tree", {
  repo <- local_git_repo()
  writeLines("brand new repo", file.path(repo$dir, "f.txt"))
  repo$run("add", "f.txt")

  result <- .git_diff(repo$dir, repo$git, "f.txt")
  expect_true(result$found)
  text <- paste(unlist(result$lines), collapse = "\n")
  expect_match(text, "+brand new repo", fixed = TRUE)
})

test_that(".git_diff reports binary files as metadata, not text", {
  repo <- local_git_repo()
  writeLines("keep", file.path(repo$dir, "keep.txt"))
  repo$run("add", "keep.txt")
  repo$run("commit", "-q", "-m", "initial commit")
  writeBin(as.raw(0:5), file.path(repo$dir, "bin.dat"))

  result <- .git_diff(repo$dir, repo$git, "bin.dat")
  expect_true(result$found)
  expect_true(result$binary)
  expect_equal(length(result$lines), 0L)
})

test_that(".git_diff returns found = FALSE for a path with no pending changes", {
  repo <- local_git_repo()
  writeLines("hello", file.path(repo$dir, "a.txt"))
  repo$run("add", "a.txt")
  repo$run("commit", "-q", "-m", "initial commit")

  result <- .git_diff(repo$dir, repo$git, "a.txt")
  expect_false(result$found)
})

test_that(".git_diff supports chunked loading via offset_lines", {
  repo <- local_git_repo()
  original <- paste0("line", 1:200)
  writeLines(original, file.path(repo$dir, "big.txt"))
  repo$run("add", "big.txt")
  repo$run("commit", "-q", "-m", "initial commit")
  edited <- original
  edited[100] <- "line100 changed"
  writeLines(edited, file.path(repo$dir, "big.txt"))

  first <- .git_diff(repo$dir, repo$git, "big.txt", max_bytes = 50)
  expect_true(first$truncated)
  expect_true(length(first$lines) < first$total_lines)

  rest <- .git_diff(
    repo$dir, repo$git, "big.txt",
    offset_lines = first$offset_lines + length(first$lines), max_bytes = 100000L
  )
  expect_false(rest$truncated)
  expect_equal(first$offset_lines + length(first$lines) + length(rest$lines), rest$total_lines)
})

test_that(".validate_repo_relative_path rejects escapes and absolute paths", {
  repo <- local_git_repo()
  expect_true(.validate_repo_relative_path(repo$dir, "a.txt"))
  expect_true(.validate_repo_relative_path(repo$dir, "sub/a.txt"))
  expect_false(.validate_repo_relative_path(repo$dir, "../etc/passwd"))
  expect_false(.validate_repo_relative_path(repo$dir, "sub/../../etc/passwd"))
  expect_false(.validate_repo_relative_path(repo$dir, "/etc/passwd"))
  expect_false(.validate_repo_relative_path(repo$dir, ""))
})
