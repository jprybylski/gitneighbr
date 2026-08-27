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

test_that(".gitignore_escape_path anchors a plain path to the repository root", {
  expect_equal(.gitignore_escape_path("notes.txt"), "/notes.txt")
  expect_equal(.gitignore_escape_path("sub/notes.txt"), "/sub/notes.txt")
})

test_that(".gitignore_escape_path escapes gitignore metacharacters", {
  expect_equal(.gitignore_escape_path("#todo.txt"), "/\\#todo.txt")
  expect_equal(.gitignore_escape_path("!important.txt"), "/\\!important.txt")
  expect_equal(.gitignore_escape_path("notes[1].txt"), "/notes\\[1\\].txt")
  expect_equal(.gitignore_escape_path("a*b?.txt"), "/a\\*b\\?.txt")
})

test_that(".gitignore_escape_path escapes trailing spaces", {
  expect_equal(.gitignore_escape_path("trailing  "), "/trailing\\ \\ ")
})

test_that(".git_ignore_path creates .gitignore and appends the escaped rule", {
  repo <- local_git_repo()
  writeLines("secret", file.path(repo$dir, "local.env"))

  result <- .git_ignore_path(repo$dir, repo$git, "local.env")
  expect_true(result$ok)
  expect_true(result$added)
  expect_equal(result$rule, "/local.env")
  expect_equal(readLines(file.path(repo$dir, ".gitignore")), "/local.env")
})

test_that(".git_ignore_path preserves existing .gitignore content and fixes a missing trailing newline", {
  repo <- local_git_repo()
  gitignore <- file.path(repo$dir, ".gitignore")
  writeBin(charToRaw("*.log"), gitignore)
  writeLines("secret", file.path(repo$dir, "local.env"))

  result <- .git_ignore_path(repo$dir, repo$git, "local.env")
  expect_true(result$ok)
  expect_true(result$added)
  expect_equal(readLines(gitignore), c("*.log", "/local.env"))
})

test_that(".git_ignore_path is a no-op when the path is already effectively ignored", {
  # `git status` never lists an already-ignored path as untracked in the
  # first place, so this guards a race (e.g. another gitneighbr tab editing
  # .gitignore between the status read that showed this path and this
  # call) rather than a state reachable by mismatched setup alone -- hence
  # mocking the check directly instead of crafting real ignore rules.
  repo <- local_git_repo()
  writeLines("secret", file.path(repo$dir, "local.env"))
  testthat::local_mocked_bindings(.git_path_effectively_ignored = function(...) TRUE)

  result <- .git_ignore_path(repo$dir, repo$git, "local.env")
  expect_true(result$ok)
  expect_false(result$added)
  expect_false(file.exists(file.path(repo$dir, ".gitignore")))
})

test_that(".git_ignore_path fails with PATH_OUTSIDE_REPOSITORY for an escaping path", {
  repo <- local_git_repo()
  result <- .git_ignore_path(repo$dir, repo$git, "../outside.txt")
  expect_false(result$ok)
  expect_equal(result$code, "PATH_OUTSIDE_REPOSITORY")
})

test_that(".git_ignore_path fails with STATE_CHANGED for a tracked file", {
  repo <- local_git_repo()
  writeLines("v1", file.path(repo$dir, "f.txt"))
  repo$run("add", "f.txt")
  repo$run("commit", "-q", "-m", "initial")

  result <- .git_ignore_path(repo$dir, repo$git, "f.txt")
  expect_false(result$ok)
  expect_equal(result$code, "STATE_CHANGED")
})

test_that(".git_ignore_path fails with STATE_CHANGED for a path that no longer exists", {
  repo <- local_git_repo()
  result <- .git_ignore_path(repo$dir, repo$git, "never-existed.txt")
  expect_false(result$ok)
  expect_equal(result$code, "STATE_CHANGED")
})
