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

test_that(".git_trash_untracked_file moves an untracked file out of the working tree", {
  repo <- local_git_repo()
  writeLines("scratch", file.path(repo$dir, "scratch.txt"))
  testthat::local_mocked_bindings(.trash_file = function(full_path) list(ok = TRUE))

  result <- .git_trash_untracked_file(repo$dir, repo$git, "scratch.txt")
  expect_true(result$ok)
  expect_equal(result$path, "scratch.txt")
})

test_that(".git_trash_untracked_file reports TRASH_UNAVAILABLE and leaves the file in place", {
  repo <- local_git_repo()
  writeLines("scratch", file.path(repo$dir, "scratch.txt"))
  testthat::local_mocked_bindings(.trash_file = function(full_path) list(ok = FALSE))

  result <- .git_trash_untracked_file(repo$dir, repo$git, "scratch.txt")
  expect_false(result$ok)
  expect_equal(result$code, "TRASH_UNAVAILABLE")
  expect_true(file.exists(file.path(repo$dir, "scratch.txt")))
})

test_that(".git_trash_untracked_file fails with PATH_OUTSIDE_REPOSITORY for an escaping path", {
  repo <- local_git_repo()
  result <- .git_trash_untracked_file(repo$dir, repo$git, "../outside.txt")
  expect_false(result$ok)
  expect_equal(result$code, "PATH_OUTSIDE_REPOSITORY")
})

test_that(".git_trash_untracked_file fails with STATE_CHANGED for a tracked file", {
  repo <- local_git_repo()
  writeLines("v1", file.path(repo$dir, "f.txt"))
  repo$run("add", "f.txt")
  repo$run("commit", "-q", "-m", "initial")

  result <- .git_trash_untracked_file(repo$dir, repo$git, "f.txt")
  expect_false(result$ok)
  expect_equal(result$code, "STATE_CHANGED")
})

test_that(".git_trash_untracked_file fails with STATE_CHANGED for a path that no longer exists", {
  repo <- local_git_repo()
  result <- .git_trash_untracked_file(repo$dir, repo$git, "never-existed.txt")
  expect_false(result$ok)
  expect_equal(result$code, "STATE_CHANGED")
})

test_that(".git_trash_untracked_file refuses an untracked directory", {
  repo <- local_git_repo()
  dir.create(file.path(repo$dir, "newdir"))
  writeLines("x", file.path(repo$dir, "newdir", "inside.txt"))

  result <- .git_trash_untracked_file(repo$dir, repo$git, "newdir/")
  expect_false(result$ok)
  expect_equal(result$code, "PATH_IS_DIRECTORY")
  expect_true(file.exists(file.path(repo$dir, "newdir", "inside.txt")))
})

test_that(".git_trash_untracked_file never shells out to git clean", {
  repo <- local_git_repo()
  writeLines("scratch", file.path(repo$dir, "scratch.txt"))
  writeLines("keep me too", file.path(repo$dir, "other.txt"))
  testthat::local_mocked_bindings(.trash_file = function(full_path) list(ok = TRUE))

  .git_trash_untracked_file(repo$dir, repo$git, "scratch.txt")
  expect_true(file.exists(file.path(repo$dir, "other.txt")))
})

test_that(".trash_file_linux moves a file into the freedesktop.org home trash and writes a .trashinfo", {
  skip_on_os(c("windows", "mac"))
  data_home <- withr::local_tempdir()
  withr::local_envvar(XDG_DATA_HOME = data_home)

  src_dir <- withr::local_tempdir()
  full_path <- file.path(src_dir, "doomed.txt")
  writeLines("bye", full_path)

  result <- .trash_file_linux(full_path)
  expect_true(result$ok)
  expect_false(file.exists(full_path))

  moved <- file.path(data_home, "Trash", "files", "doomed.txt")
  info <- file.path(data_home, "Trash", "info", "doomed.txt.trashinfo")
  expect_true(file.exists(moved))
  expect_true(file.exists(info))
  info_text <- readLines(info)
  expect_true(any(grepl("^\\[Trash Info\\]$", info_text)))
  expect_true(any(grepl("^Path=", info_text)))
  expect_true(any(grepl("^DeletionDate=", info_text)))
})

test_that(".trash_file_linux disambiguates a name collision in files/", {
  skip_on_os(c("windows", "mac"))
  data_home <- withr::local_tempdir()
  withr::local_envvar(XDG_DATA_HOME = data_home)
  fs::dir_create(file.path(data_home, "Trash", "files"))
  writeLines("earlier", file.path(data_home, "Trash", "files", "doomed.txt"))

  src_dir <- withr::local_tempdir()
  full_path <- file.path(src_dir, "doomed.txt")
  writeLines("bye", full_path)

  result <- .trash_file_linux(full_path)
  expect_true(result$ok)
  expect_true(file.exists(file.path(data_home, "Trash", "files", "doomed 2.txt")))
})
