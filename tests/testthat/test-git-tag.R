local_tag_repo_with_remote <- function(env = parent.frame()) {
  git <- unname(Sys.which("git"))
  skip_if(!nzchar(git), "git not available")

  remote_dir <- withr::local_tempdir(.local_envir = env)
  processx::run(git, c("init", "-q", "--bare", "-b", "main", remote_dir), error_on_status = TRUE)

  dir <- withr::local_tempdir(.local_envir = env)
  run <- function(...) processx::run(git, c("-C", dir, ...), error_on_status = TRUE)
  run("init", "-q", "-b", "main")
  run("config", "user.email", "test@example.com")
  run("config", "user.name", "Test")
  writeLines("seed", file.path(dir, "seed.txt"))
  run("add", "seed.txt")
  run("commit", "-q", "-m", "seed commit")
  run("remote", "add", "origin", remote_dir)
  run("push", "-q", "-u", "origin", "main")

  list(dir = dir, git = git, run = run, remote_dir = remote_dir)
}

test_that(".validate_tag_name accepts well-formed names and rejects malformed ones", {
  git <- unname(Sys.which("git"))
  skip_if(!nzchar(git), "git not available")

  expect_true(.validate_tag_name(git, "v1.0.0"))
  expect_true(.validate_tag_name(git, "release-1"))
  expect_false(.validate_tag_name(git, ""))
  expect_false(.validate_tag_name(git, NULL))
  expect_false(.validate_tag_name(git, "has a space"))
  expect_false(.validate_tag_name(git, "..leading-dots"))
  expect_false(.validate_tag_name(git, "trailing."))
  expect_false(.validate_tag_name(git, "a~b"))
})

test_that(".classify_tag_push_failure maps a remote tag collision to TAG_EXISTS", {
  expect_equal(
    .classify_tag_push_failure("! [rejected] v1.0.0 -> v1.0.0 (already exists)"),
    "TAG_EXISTS"
  )
  expect_equal(
    .classify_tag_push_failure("fatal: Authentication failed"),
    "AUTH_REQUIRED"
  )
})

test_that(".git_create_tag creates an annotated tag on HEAD", {
  repo <- local_tag_repo_with_remote()

  result <- .git_create_tag(repo$dir, repo$git, "v1.0.0", annotation = "First release")
  expect_true(result$ok)
  expect_equal(result$name, "v1.0.0")

  type_result <- processx::run(repo$git, c("-C", repo$dir, "cat-file", "-t", "v1.0.0"), error_on_status = TRUE)
  expect_equal(trimws(type_result$stdout), "tag")

  message_result <- processx::run(repo$git, c("-C", repo$dir, "tag", "-l", "-n1", "v1.0.0"), error_on_status = TRUE)
  expect_match(trimws(message_result$stdout), "First release", fixed = TRUE)
})

test_that(".git_create_tag defaults the annotation to the tag name when none is given", {
  repo <- local_tag_repo_with_remote()

  result <- .git_create_tag(repo$dir, repo$git, "v1.0.0")
  expect_true(result$ok)

  message_result <- processx::run(repo$git, c("-C", repo$dir, "tag", "-l", "-n1", "v1.0.0"), error_on_status = TRUE)
  expect_match(trimws(message_result$stdout), "v1.0.0", fixed = TRUE)
})

test_that(".git_create_tag rejects an invalid tag name", {
  repo <- local_tag_repo_with_remote()

  result <- .git_create_tag(repo$dir, repo$git, "has a space")
  expect_false(result$ok)
  expect_equal(result$code, "INVALID_TAG")
})

test_that(".git_create_tag refuses to recreate an existing local tag", {
  repo <- local_tag_repo_with_remote()

  first <- .git_create_tag(repo$dir, repo$git, "v1.0.0")
  expect_true(first$ok)

  second <- .git_create_tag(repo$dir, repo$git, "v1.0.0")
  expect_false(second$ok)
  expect_equal(second$code, "TAG_EXISTS")
})

test_that(".git_push_tag pushes an existing local tag to the branch's upstream remote", {
  repo <- local_tag_repo_with_remote()
  created <- .git_create_tag(repo$dir, repo$git, "v1.0.0")
  expect_true(created$ok)

  result <- .git_push_tag(repo$dir, repo$git, "v1.0.0")
  expect_true(result$ok)
  expect_equal(result$remote, "origin")
  expect_equal(result$name, "v1.0.0")

  remote_tag <- processx::run(repo$git, c("-C", repo$remote_dir, "tag", "-l", "v1.0.0"), error_on_status = TRUE)
  expect_equal(trimws(remote_tag$stdout), "v1.0.0")
})

test_that(".git_push_tag fails with INVALID_TAG for a malformed name", {
  repo <- local_tag_repo_with_remote()

  result <- .git_push_tag(repo$dir, repo$git, "has a space")
  expect_false(result$ok)
  expect_equal(result$code, "INVALID_TAG")
})

test_that(".git_push_tag fails when the tag does not exist locally", {
  repo <- local_tag_repo_with_remote()

  result <- .git_push_tag(repo$dir, repo$git, "v9.9.9")
  expect_false(result$ok)
  expect_equal(result$code, "COMMAND_FAILED")
})

test_that(".git_push_tag refuses to move a tag the remote already has", {
  repo <- local_tag_repo_with_remote()
  created <- .git_create_tag(repo$dir, repo$git, "v1.0.0")
  expect_true(created$ok)
  pushed <- .git_push_tag(repo$dir, repo$git, "v1.0.0")
  expect_true(pushed$ok)

  # Delete and recreate locally so it points at a different object, then
  # try to push again: the remote already has "v1.0.0" pointing elsewhere.
  processx::run(repo$git, c("-C", repo$dir, "tag", "-d", "v1.0.0"), error_on_status = TRUE)
  writeLines("more", file.path(repo$dir, "more.txt"))
  repo$run("add", "more.txt")
  repo$run("commit", "-q", "-m", "second commit")
  recreated <- .git_create_tag(repo$dir, repo$git, "v1.0.0")
  expect_true(recreated$ok)

  result <- .git_push_tag(repo$dir, repo$git, "v1.0.0")
  expect_false(result$ok)
  expect_equal(result$code, "TAG_EXISTS")
})

test_that(".git_push_tag fails with NO_UPSTREAM when the branch has none", {
  git <- unname(Sys.which("git"))
  skip_if(!nzchar(git), "git not available")
  dir <- withr::local_tempdir()
  run <- function(...) processx::run(git, c("-C", dir, ...), error_on_status = TRUE)
  run("init", "-q", "-b", "main")
  run("config", "user.email", "test@example.com")
  run("config", "user.name", "Test")
  writeLines("x", file.path(dir, "x.txt"))
  run("add", "x.txt")
  run("commit", "-q", "-m", "initial")

  created <- .git_create_tag(dir, git, "v1.0.0")
  expect_true(created$ok)

  result <- .git_push_tag(dir, git, "v1.0.0")
  expect_false(result$ok)
  expect_equal(result$code, "NO_UPSTREAM")
})
