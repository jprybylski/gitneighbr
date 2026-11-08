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
  expect_equal(status$staged_count, 1L) # a.txt stays staged exactly as the user left it
  expect_equal(status$unstaged_count, 0L)
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

test_that(".git_commit_selected reports HOOK_FAILED with a sanitized advanced block when a pre-commit hook rejects", {
  repo <- local_git_repo()
  hooks_dir <- file.path(repo$dir, ".git", "hooks")
  hook_path <- file.path(hooks_dir, "pre-commit")
  home <- path.expand("~")
  writeLines(
    c("#!/bin/sh", paste0("echo 'fatal: rejected by hook at ", home, "/secrets' >&2"), "exit 1"),
    hook_path
  )
  Sys.chmod(hook_path, "0755")
  writeLines("hello", file.path(repo$dir, "a.txt"))

  result <- .git_commit_selected(repo$dir, repo$git, "a.txt", summary = "Add a.txt")
  expect_false(result$ok)
  expect_equal(result$code, "HOOK_FAILED")
  expect_match(result$advanced$command, "^git -C .* commit -q -F .*$")
  expect_equal(result$advanced$exit_status, 1L)
  expect_false(grepl(home, result$advanced$stderr, fixed = TRUE))
  expect_match(result$advanced$stderr, "~/secrets", fixed = TRUE)

  status <- .git_status(repo$dir, repo$git)
  expect_equal(status$staged_count, 0L) # index restored, nothing left staged
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

test_that(".classify_commit_failure maps stderr text to stable error codes", {
  expect_equal(.classify_commit_failure("Please tell me who you are."), "IDENTITY_MISSING")
  expect_equal(.classify_commit_failure("unable to auto-detect email address"), "IDENTITY_MISSING")
  expect_equal(.classify_commit_failure("*** empty ident name not allowed"), "IDENTITY_MISSING")
  expect_equal(.classify_commit_failure("hook declined"), "HOOK_FAILED")
  expect_equal(.classify_commit_failure("gpg failed to sign the data"), "SIGNING_FAILED")
  expect_equal(.classify_commit_failure("error: could not sign the commit"), "SIGNING_FAILED")
  expect_equal(.classify_commit_failure("something else entirely"), "COMMAND_FAILED")
  expect_equal(.classify_commit_failure(NULL), "COMMAND_FAILED")
})

test_that(".git_commit_selected reports COMMAND_FAILED when `git write-tree` fails", {
  skip_on_os("windows")
  repo <- local_git_repo()
  writeLines("hello", file.path(repo$dir, "a.txt"))
  failing_git <- .make_failing_git(repo$git, "write-tree")

  result <- .git_commit_selected(repo$dir, failing_git, "a.txt", summary = "Add a.txt")
  expect_false(result$ok)
  expect_equal(result$code, "COMMAND_FAILED")
  expect_match(result$message, "record the selected files")
})

test_that(".git_commit_selected keeps a partially staged, unselected file's staging", {
  repo <- local_git_repo()
  writeLines(c("one", "two"), file.path(repo$dir, "c.txt"))
  repo$run("add", "c.txt")
  repo$run("commit", "-q", "-m", "initial commit")
  writeLines(c("one staged", "two"), file.path(repo$dir, "c.txt"))
  repo$run("add", "c.txt")
  writeLines(c("one staged", "two unstaged"), file.path(repo$dir, "c.txt"))
  writeLines("new", file.path(repo$dir, "d.txt"))
  staged_before <- processx::run(repo$git, c("-C", repo$dir, "diff", "--cached"))$stdout

  result <- .git_commit_selected(repo$dir, repo$git, "d.txt", summary = "Add d.txt")
  expect_true(result$ok)
  expect_identical(processx::run(repo$git, c("-C", repo$dir, "diff", "--cached"))$stdout, staged_before)
  expect_identical(readLines(file.path(repo$dir, "c.txt")), c("one staged", "two unstaged"))
})

test_that(".git_commit_selected refuses when HEAD moved since the list was shown", {
  repo <- local_git_repo()
  writeLines("a", file.path(repo$dir, "a.txt"))
  repo$run("add", "a.txt")
  repo$run("commit", "-q", "-m", "initial commit")
  seen <- trimws(processx::run(repo$git, c("-C", repo$dir, "rev-parse", "HEAD"))$stdout)
  writeLines("b", file.path(repo$dir, "b.txt"))
  repo$run("add", "b.txt")
  repo$run("commit", "-q", "-m", "someone else")
  writeLines("e", file.path(repo$dir, "e.txt"))

  result <- .git_commit_selected(repo$dir, repo$git, "e.txt", summary = "Add e.txt", expected_head = seen)
  expect_false(result$ok)
  expect_equal(result$code, "STATE_CHANGED")
  expect_equal(trimws(processx::run(repo$git, c("-C", repo$dir, "log", "-1", "--format=%s"))$stdout), "someone else")
})

test_that(".git_commit_selected refuses Git's own data and paths through a linked folder", {
  skip_on_os("windows")
  repo <- local_git_repo()
  outside <- withr::local_tempdir()
  writeLines("secret", file.path(outside, "s.txt"))
  file.symlink(outside, file.path(repo$dir, "linked"))
  expect_false(.commit_path_is_safe(repo$dir, "linked/s.txt"))
  expect_false(.commit_path_is_safe(repo$dir, ".git/config"))
  expect_false(.commit_path_is_safe(repo$dir, ".GIT/config"))
  expect_false(.commit_path_is_safe(repo$dir, "../x"))
  expect_true(.commit_path_is_safe(repo$dir, "linked"))
  expect_true(.commit_path_is_safe(repo$dir, "sub/new.txt"))
})

test_that(".git_commit_selected commits on an unborn branch with a managed Git, without hooks", {
  repo <- local_git_repo()
  marker <- file.path(withr::local_tempdir(), "marker")
  hook <- file.path(repo$dir, ".git", "hooks", "pre-commit")
  writeLines(c("#!/bin/sh", paste0("touch '", marker, "'")), hook)
  Sys.chmod(hook, "0755")
  git <- .managed_git(repo$git, withr::local_tempfile(fileext = ".gitconfig"))
  writeLines("a", file.path(repo$dir, "a.txt"))
  writeLines("b", file.path(repo$dir, "b.txt"))

  result <- .git_commit_selected(repo$dir, git, "a.txt", summary = "Add a.txt", expected_head = "")
  expect_true(result$ok)
  expect_false(file.exists(marker))
  files <- processx::run(repo$git, c("-C", repo$dir, "show", "--name-only", "--format="))$stdout
  expect_identical(trimws(files), "a.txt")
  reflog <- processx::run(repo$git, c("-C", repo$dir, "reflog", "-1", "--format=%gs"))$stdout
  expect_match(reflog, "commit: Add a.txt", fixed = TRUE)

  # A second commit moves HEAD only from the commit it was shown.
  first <- trimws(processx::run(repo$git, c("-C", repo$dir, "rev-parse", "HEAD"))$stdout)
  again <- .git_commit_selected(repo$dir, git, "b.txt", summary = "Add b.txt", expected_head = first)
  expect_true(again$ok)
  parent <- trimws(processx::run(repo$git, c("-C", repo$dir, "rev-parse", "HEAD^"))$stdout)
  expect_identical(parent, first)
  status <- .git_status(repo$dir, repo$git)
  expect_false(status$has_changes)
})

test_that(".git_commit_selected reports COMMAND_FAILED and restores the index when `git add` fails", {
  skip_on_os("windows")
  repo <- local_git_repo()
  writeLines("hello", file.path(repo$dir, "a.txt"))
  failing_git <- .make_failing_git(repo$git, "add")

  result <- .git_commit_selected(repo$dir, failing_git, "a.txt", summary = "Add a.txt")
  expect_false(result$ok)
  expect_equal(result$code, "COMMAND_FAILED")
  expect_match(result$message, "stage the selected files")

  status <- .git_status(repo$dir, repo$git)
  expect_equal(status$staged_count, 0L)
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

test_that(".git_commit_selected saves nothing for a path it can't trust, or when HEAD moves mid-save", {
  repo <- .new_test_repo()
  writeLines("a", file.path(repo$dir, "a.txt"))
  repo$run("add", "a.txt")
  repo$run("commit", "-q", "-m", "initial commit")
  head <- function() trimws(processx::run(repo$git, c("-C", repo$dir, "rev-parse", "HEAD"))$stdout)
  before <- head()
  writeLines("x", file.path(repo$dir, "x.txt"))

  local({
    local_mocked_bindings(.commit_path_is_safe = function(...) FALSE)
    result <- .git_commit_selected(repo$dir, repo$git, "x.txt", summary = "Add x")
    expect_identical(result$code, "PATH_REJECTED")
  })
  expect_identical(head(), before)

  git <- .managed_git(repo$git, config_file = withr::local_tempfile(fileext = ".gitconfig"))
  real_exec <- gitneighbr:::.git_exec
  local({
    # Another writer wins the race: update-ref's compare-and-swap fails.
    local_mocked_bindings(.git_exec = function(git_bin, args, ...) {
      if ("update-ref" %in% args) {
        return(list(status = 1L, stdout = "", stderr = "cannot lock ref 'HEAD'"))
      }
      real_exec(git_bin, args, ...)
    })
    result <- .git_commit_selected(repo$dir, git, "x.txt", summary = "Add x")
    expect_identical(result$code, "STATE_CHANGED")
  })
  expect_identical(head(), before)
})
