# Extracted from test-git-trash.R:67

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "gitneighbr", path = "..")
attach(test_env, warn.conflicts = FALSE)

# prequel ----------------------------------------------------------------------
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

# test -------------------------------------------------------------------------
repo <- local_git_repo()
dir.create(file.path(repo$dir, "newdir"))
writeLines("x", file.path(repo$dir, "newdir", "inside.txt"))
result <- .git_trash_untracked_file(repo$dir, repo$git, "newdir/")
expect_false(result$ok)
expect_equal(result$code, "PATH_IS_DIRECTORY")
