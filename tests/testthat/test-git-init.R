test_that(".git_init_workspace creates the folder and initializes a repository", {
  git <- unname(Sys.which("git"))
  skip_if(!nzchar(git), "git not available")
  dir <- file.path(withr::local_tempdir(), "nested", "project")

  result <- .git_init_workspace(dir, git)
  expect_true(result$ok)
  expect_true(dir.exists(dir))
  expect_equal(.git_repo_kind(dir, git), "worktree")

  branch <- trimws(processx::run(git, c("-C", dir, "branch", "--show-current"), error_on_status = TRUE)$stdout)
  expect_equal(branch, "main")
})

test_that(".git_init_workspace is fine with a folder that already has ordinary files in it", {
  git <- unname(Sys.which("git"))
  skip_if(!nzchar(git), "git not available")
  dir <- withr::local_tempdir()
  writeLines("hello", file.path(dir, "existing.txt"))

  result <- .git_init_workspace(dir, git)
  expect_true(result$ok)
  expect_equal(.git_repo_kind(dir, git), "worktree")
  expect_true(file.exists(file.path(dir, "existing.txt")))
})

test_that(".git_init_workspace refuses a folder that is already a Git repository", {
  git <- unname(Sys.which("git"))
  skip_if(!nzchar(git), "git not available")
  dir <- withr::local_tempdir()
  processx::run(git, c("-C", dir, "init", "-q", "-b", "main"), error_on_status = TRUE)

  result <- .git_init_workspace(dir, git)
  expect_false(result$ok)
  expect_equal(result$code, "ALREADY_A_REPOSITORY")
})
