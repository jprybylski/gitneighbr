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

test_that(".git_restore_tracked_file restores an unstaged edit to the last commit", {
  repo <- local_git_repo()
  writeLines("v1", file.path(repo$dir, "f.txt"))
  repo$run("add", "f.txt")
  repo$run("commit", "-q", "-m", "initial")

  writeLines("v2 unsaved", file.path(repo$dir, "f.txt"))

  result <- .git_restore_tracked_file(repo$dir, repo$git, "f.txt")
  expect_true(result$ok)
  expect_equal(result$path, "f.txt")
  expect_equal(readLines(file.path(repo$dir, "f.txt")), "v1")
})

test_that(".git_restore_tracked_file recreates an unstaged deletion", {
  repo <- local_git_repo()
  writeLines("v1", file.path(repo$dir, "f.txt"))
  repo$run("add", "f.txt")
  repo$run("commit", "-q", "-m", "initial")

  file.remove(file.path(repo$dir, "f.txt"))
  expect_false(file.exists(file.path(repo$dir, "f.txt")))

  result <- .git_restore_tracked_file(repo$dir, repo$git, "f.txt")
  expect_true(result$ok)
  expect_true(file.exists(file.path(repo$dir, "f.txt")))
  expect_equal(readLines(file.path(repo$dir, "f.txt")), "v1")
})

test_that(".git_restore_tracked_file fails with PATH_OUTSIDE_REPOSITORY for an escaping path", {
  repo <- local_git_repo()
  result <- .git_restore_tracked_file(repo$dir, repo$git, "../outside.txt")
  expect_false(result$ok)
  expect_equal(result$code, "PATH_OUTSIDE_REPOSITORY")
})

test_that(".git_restore_tracked_file fails with STATE_CHANGED for a path with no pending change", {
  repo <- local_git_repo()
  writeLines("v1", file.path(repo$dir, "f.txt"))
  repo$run("add", "f.txt")
  repo$run("commit", "-q", "-m", "initial")

  result <- .git_restore_tracked_file(repo$dir, repo$git, "f.txt")
  expect_false(result$ok)
  expect_equal(result$code, "STATE_CHANGED")
})

test_that(".git_restore_tracked_file fails with STATE_CHANGED for an untracked path", {
  repo <- local_git_repo()
  writeLines("brand new", file.path(repo$dir, "untracked.txt"))

  result <- .git_restore_tracked_file(repo$dir, repo$git, "untracked.txt")
  expect_false(result$ok)
  expect_equal(result$code, "STATE_CHANGED")
})

test_that(".git_restore_tracked_file fails with CONFLICTS_PRESENT for a conflicted path", {
  repo <- local_git_repo()
  writeLines("base", file.path(repo$dir, "f.txt"))
  repo$run("add", "f.txt")
  repo$run("commit", "-q", "-m", "base")

  repo$run("checkout", "-q", "-b", "other")
  writeLines("other change", file.path(repo$dir, "f.txt"))
  repo$run("commit", "-q", "-am", "other change")

  repo$run("checkout", "-q", "main")
  writeLines("main change", file.path(repo$dir, "f.txt"))
  repo$run("commit", "-q", "-am", "main change")

  processx::run(repo$git, c("-C", repo$dir, "merge", "other"), error_on_status = FALSE)

  result <- .git_restore_tracked_file(repo$dir, repo$git, "f.txt")
  expect_false(result$ok)
  expect_equal(result$code, "CONFLICTS_PRESENT")
})
