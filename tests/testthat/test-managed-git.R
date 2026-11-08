# Managed Git execution: a hostile repository or a hostile ambient Git setup
# must not make gitneighbr run another program, or act outside its folder.

.managed_test_git <- function(env = parent.frame(), remote = FALSE, protocols = "https") {
  git <- unname(Sys.which("git"))
  testthat::skip_if(!nzchar(git), "git not available")
  config <- withr::local_tempfile(.local_envir = env, fileext = ".gitconfig")
  .managed_git(git, config_file = config, remote = remote, protocols = protocols)
}

# A script that, if Git ever ran it, would leave `marker` behind.
.marker_script <- function(dir, marker) {
  path <- file.path(dir, "evil.sh")
  writeLines(c("#!/bin/sh", paste0("echo ran > '", marker, "'"), "cat"), path)
  Sys.chmod(path, "755")
  path
}

test_that("every Git call in R/ goes through .git_exec()", {
  root <- testthat::test_path("..", "..", "R")
  skip_if(!dir.exists(root), "package source not available")
  for (file in list.files(root, pattern = "\\.R$", full.names = TRUE)) {
    if (basename(file) == "git-exec.R") next
    text <- paste(readLines(file), collapse = "\n")
    expect_false(grepl("processx::run\\(\\s*git(_bin)?\\s*,", text, perl = TRUE), info = basename(file))
  }
})

test_that("a managed Git gets fixed settings before the subcommand and no external diff tools", {
  git <- .managed_test_git()
  managed <- attr(git, "gitneighbr_managed")
  args <- .managed_git_args(managed, c("-C", "/repo", "diff", "--", "a"))
  sub <- which(args == "diff")
  expect_identical(args[sub + 1:2], c("--no-ext-diff", "--no-textconv"))
  expect_true(all(which(args == "-c") < sub))
  expect_true("core.fsmonitor=false" %in% args)
  expect_true("protocol.allow=never" %in% args)
  expect_true("protocol.https.allow=always" %in% args)
  expect_true("credential.helper=" %in% args)
  status <- .managed_git_args(managed, c("-C", "/repo", "status"))
  expect_false("--no-ext-diff" %in% status)
})

test_that("a managed Git ignores inherited GIT_* variables and the user's own configuration", {
  repo <- .new_test_repo()
  other <- .new_test_repo()
  home <- withr::local_tempdir()
  writeLines(c("[user]", "  name = Ambient User", "[alias]", "  status = !touch pwned"), file.path(home, ".gitconfig"))
  withr::local_envvar(
    HOME = home, GIT_DIR = file.path(other$dir, ".git"), GIT_WORK_TREE = other$dir,
    GIT_CONFIG_PARAMETERS = "'core.fsmonitor=touch pwned'", GIT_CONFIG_COUNT = "1",
    GIT_CONFIG_KEY_0 = "user.name", GIT_CONFIG_VALUE_0 = "Injected"
  )
  git <- .managed_test_git()
  managed <- attr(git, "gitneighbr_managed")
  expect_false(any(grepl("^(GIT_DIR|GIT_WORK_TREE|GIT_CONFIG_PARAMETERS|GIT_CONFIG_COUNT|GIT_CONFIG_KEY_0)$", names(managed$env))))

  top <- .git_exec(git, c("-C", repo$dir, "rev-parse", "--show-toplevel"))
  expect_identical(.real_path(trimws(top$stdout)), .real_path(repo$dir))
  # The repo's own user.name (set by .new_test_repo) is all Git sees.
  name <- .git_exec(git, c("-C", repo$dir, "config", "--get", "user.name"))
  expect_identical(trimws(name$stdout), "Test")
  global <- .git_exec(git, c("config", "--global", "--get", "user.name"), error_on_status = FALSE)
  expect_false(identical(trimws(global$stdout), "Ambient User"))
  expect_false(file.exists(file.path(repo$dir, "pwned")))
})

test_that("hooks, fsmonitor, pager and textconv in the repository never run under a managed Git", {
  repo <- .new_test_repo()
  marker <- file.path(withr::local_tempdir(), "marker")
  evil <- .marker_script(withr::local_tempdir(), marker)

  dir.create(file.path(repo$dir, ".git", "hooks"), showWarnings = FALSE)
  file.copy(evil, file.path(repo$dir, ".git", "hooks", "pre-commit"))
  Sys.chmod(file.path(repo$dir, ".git", "hooks", "pre-commit"), "755")
  hooks <- file.path(repo$dir, "myhooks")
  dir.create(hooks)
  file.copy(evil, file.path(hooks, "pre-commit"))
  file.copy(evil, file.path(hooks, "post-commit"))
  Sys.chmod(file.path(hooks, c("pre-commit", "post-commit")), "755")
  repo$run("config", "core.hooksPath", hooks)
  repo$run("config", "core.fsmonitor", evil)
  repo$run("config", "core.pager", evil)
  repo$run("config", "diff.evil.textconv", evil)
  writeLines("*.txt diff=evil", file.path(repo$dir, ".git", "info", "attributes"))

  git <- .managed_test_git()
  writeLines("one", file.path(repo$dir, "a.txt"))
  .git_exec(git, c("-C", repo$dir, "add", "--", "a.txt"))
  .git_exec(git, c("-C", repo$dir, "commit", "-q", "-m", "one"))
  writeLines("two", file.path(repo$dir, "a.txt"))
  .git_exec(git, c("-C", repo$dir, "status", "--porcelain=v2"))
  .git_exec(git, c("-C", repo$dir, "diff", "--", "a.txt"))
  .git_exec(git, c("-C", repo$dir, "log", "-p", "-1"))
  expect_false(file.exists(marker))

  # The textconv driver is unknown configuration, so a managed session also
  # refuses the repository outright.
  check <- .managed_repo_check(repo$dir, git)
  expect_identical(check$state, "unsupported")
  expect_identical(check$code, "UNSUPPORTED_CONFIGURATION")
  expect_identical(check$detail, "diff.<name>.textconv")
})

test_that("a commit via .git_commit_selected() runs no hook under a managed Git", {
  repo <- .new_test_repo()
  marker <- file.path(withr::local_tempdir(), "marker")
  evil <- .marker_script(withr::local_tempdir(), marker)
  file.copy(evil, file.path(repo$dir, ".git", "hooks", "pre-commit"))
  file.copy(evil, file.path(repo$dir, ".git", "hooks", "commit-msg"))
  Sys.chmod(file.path(repo$dir, ".git", "hooks", c("pre-commit", "commit-msg")), "755")
  git <- .managed_test_git()

  writeLines("x", file.path(repo$dir, "x.txt"))
  result <- .git_commit_selected(repo$dir, git, "x.txt", "Add x")
  expect_true(isTRUE(result$ok))
  expect_false(file.exists(marker))
})

test_that("ext:: and file transports are refused under a managed Git", {
  repo <- .new_test_repo()
  marker <- file.path(withr::local_tempdir(), "marker")
  git <- .managed_test_git()
  res <- .git_exec(git, c("-C", repo$dir, "ls-remote", paste0("ext::sh -c touch% ", marker)), error_on_status = FALSE)
  expect_false(identical(res$status, 0L))
  expect_false(file.exists(marker))
  other <- .new_test_repo()
  res <- .git_exec(git, c("-C", repo$dir, "ls-remote", other$dir), error_on_status = FALSE)
  expect_false(identical(res$status, 0L))
})

test_that(".managed_repo_check() accepts a plain repository and an empty folder", {
  repo <- .new_test_repo()
  git <- .managed_test_git()
  repo$run("config", "core.hooksPath", ".husky")
  repo$run("remote", "add", "origin", "https://example.com/r.git")
  expect_identical(.managed_repo_check(repo$dir, git)$state, "worktree")

  empty <- withr::local_tempdir()
  expect_identical(.managed_repo_check(empty, git)$state, "not_a_repository")
})

test_that(".managed_repo_check() refuses a folder inside another repository", {
  repo <- .new_test_repo()
  git <- .managed_test_git()
  sub <- file.path(repo$dir, "study")
  dir.create(sub)
  check <- .managed_repo_check(sub, git)
  expect_identical(check$state, "enclosing_repository")
  expect_false(grepl(repo$dir, check$message, fixed = TRUE))
})

test_that(".managed_repo_check() refuses repositories that reach outside their folder", {
  git <- .managed_test_git()

  # .git as a file (a linked worktree)
  main <- .new_test_repo()
  writeLines("x", file.path(main$dir, "x"))
  main$run("add", "x")
  main$run("commit", "-q", "-m", "x")
  linked <- file.path(withr::local_tempdir(), "linked")
  main$run("worktree", "add", "-q", linked)
  expect_identical(.managed_repo_check(linked, git)$code, "GIT_FILE")

  # .git as a symlink
  link_dir <- withr::local_tempdir()
  file.symlink(file.path(main$dir, ".git"), file.path(link_dir, ".git"))
  expect_identical(.managed_repo_check(link_dir, git)$code, "GIT_DIR_LINK")

  # core.worktree pointing elsewhere
  wt <- .new_test_repo()
  elsewhere <- withr::local_tempdir()
  wt$run("config", "core.worktree", elsewhere)
  expect_identical(.managed_repo_check(wt$dir, git)$state, "unsupported")

  # alternates
  alt <- .new_test_repo()
  writeLines(file.path(main$dir, ".git", "objects"), file.path(alt$dir, ".git", "objects", "info", "alternates"))
  check <- .managed_repo_check(alt$dir, git)
  expect_identical(check$code, "GIT_DIR_REFERENCE")
  expect_identical(check$detail, ".git/objects/info/alternates")

  # submodules
  sm <- .new_test_repo()
  writeLines('[submodule "x"]', file.path(sm$dir, ".gitmodules"))
  expect_identical(.managed_repo_check(sm$dir, git)$code, "SUBMODULES")
})

test_that(".managed_repo_check() refuses unreviewed settings and filters, naming only the setting", {
  git <- .managed_test_git()
  cases <- list(
    list(key = "include.path", value = "/tmp/evil", detail = "include.path"),
    list(key = "includeIf.gitdir:/x/.path", value = "/tmp/evil", detail = "includeif.<name>.path"),
    list(key = "remote.origin.uploadpack", value = "touch pwned", detail = "remote.<name>.uploadpack"),
    list(key = "core.sshCommand", value = "touch pwned", detail = "core.sshcommand"),
    list(key = "credential.https://example.com.helper", value = "!touch pwned", detail = "credential.<name>.helper"),
    list(key = "filter.lfs.clean", value = "git-lfs clean -- %f", detail = "filter.<name>.clean"),
    list(key = "remote.origin.promisor", value = "true", detail = "remote.<name>.promisor")
  )
  for (case in cases) {
    repo <- .new_test_repo()
    repo$run("config", case$key, case$value)
    check <- .managed_repo_check(repo$dir, git)
    expect_identical(check$code, "UNSUPPORTED_CONFIGURATION", info = case$key)
    expect_identical(check$detail, case$detail, info = case$key)
    expect_false(grepl(case$value, paste(unlist(check), collapse = " "), fixed = TRUE), info = case$key)
  }

  for (attrs in list(c(".gitattributes", "*.bin filter=lfs diff=lfs merge=lfs -text"), c("sub/.gitattributes", "secret filter=git-crypt"))) {
    repo <- .new_test_repo()
    dir.create(file.path(repo$dir, dirname(attrs[[1]])), showWarnings = FALSE)
    writeLines(attrs[[2]], file.path(repo$dir, attrs[[1]]))
    check <- .managed_repo_check(repo$dir, git)
    expect_identical(check$code, "FILTER_ATTRIBUTE", info = attrs[[1]])
    expect_identical(check$detail, attrs[[1]])
  }

  repo <- .new_test_repo()
  writeLines("# filter=lfs is only mentioned in a comment\n*.txt text", file.path(repo$dir, ".gitattributes"))
  expect_identical(.managed_repo_check(repo$dir, git)$state, "worktree")
})

test_that("a named Git helper folder is passed before every subcommand", {
  git <- unname(Sys.which("git"))
  skip_if(!nzchar(git), "git not available")
  exec_path <- trimws(processx::run(git, "--exec-path")$stdout)
  managed <- .managed_git(git, withr::local_tempfile(fileext = ".gitconfig"), exec_path = exec_path)
  args <- .managed_git_args(attr(managed, "gitneighbr_managed"), c("-C", "/r", "status"))
  expect_identical(args[[1]], paste0("--exec-path=", normalizePath(exec_path, winslash = "/")))
  expect_identical(trimws(.git_exec(managed, "--exec-path")$stdout), normalizePath(exec_path, winslash = "/"))
  expect_error(.managed_git(git, tempfile(), exec_path = file.path(tempdir(), "no-such-dir")))
})

test_that(".managed_repo_check() refuses bare repositories, unreadable .git folders and info/attributes filters", {
  git <- .managed_test_git()
  bare <- withr::local_tempdir()
  processx::run(unname(Sys.which("git")), c("init", "-q", "--bare", bare))
  expect_identical(.managed_repo_check(bare, git)$code, "BARE_REPOSITORY")

  repo <- .new_test_repo()
  repo$run("config", "core.bare", "true")
  expect_identical(.managed_repo_check(repo$dir, git)$state, "unsupported")

  broken <- .new_test_repo()
  writeLines("[core\n  this is not a config", file.path(broken$dir, ".git", "config"))
  expect_identical(.managed_repo_check(broken$dir, git)$state, "unsupported")

  info <- .new_test_repo()
  dir.create(file.path(info$dir, ".git", "info"), showWarnings = FALSE)
  writeLines("* filter=lfs", file.path(info$dir, ".git", "info", "attributes"))
  check <- .managed_repo_check(info$dir, git)
  expect_identical(check$code, "FILTER_ATTRIBUTE")
  expect_identical(check$detail, ".git/info/attributes")
})

test_that("a managed Git uses its own system configuration: Git for Windows' defaults on Windows only", {
  git <- unname(Sys.which("git"))
  skip_if(!nzchar(git), "git not available")
  repo <- .new_test_repo()
  get <- function(managed, key) {
    trimws(.git_exec(managed, c("-C", repo$dir, "config", "--get", key), error_on_status = FALSE)$stdout)
  }
  config <- withr::local_tempfile(fileext = ".gitconfig")
  windows <- .managed_git(git, config, windows = TRUE)
  other <- .managed_git(git, config, windows = FALSE)
  expect_identical(get(windows, "core.autocrlf"), "true")
  expect_identical(get(windows, "http.sslBackend"), "schannel")
  expect_identical(get(other, "core.autocrlf"), "")
  # Only those settings: no LFS filter, credential helper or text conversion.
  listed <- .git_exec(windows, c("config", "--system", "--list"))$stdout
  expect_false(grepl("filter|credential|textconv", listed))
  # The repository's own setting wins.
  repo$run("config", "core.autocrlf", "false")
  expect_identical(get(windows, "core.autocrlf"), "false")
})

test_that("files another Windows Git checked out with CRLF line endings aren't shown as changed", {
  repo <- .new_test_repo()
  repo$run("config", "core.autocrlf", "false")
  writeBin(charToRaw("one\ntwo\n"), file.path(repo$dir, "notes.txt"))
  repo$run("add", "notes.txt")
  repo$run("commit", "-q", "-m", "LF in the repository")
  repo$run("config", "--unset", "core.autocrlf")
  # Check it out the way Git for Windows does by default (CRLF on disk), then
  # touch it (a sync tool, a copy): Git has to read it again to compare.
  unlink(file.path(repo$dir, "notes.txt"))
  repo$run("-c", "core.autocrlf=true", "checkout", "--", "notes.txt")
  expect_identical(readBin(file.path(repo$dir, "notes.txt"), "raw", 20), charToRaw("one\r\ntwo\r\n"))
  Sys.setFileTime(file.path(repo$dir, "notes.txt"), Sys.time() + 5)
  config <- withr::local_tempfile(fileext = ".gitconfig")
  changed <- function(managed) {
    trimws(.git_exec(managed, c("-C", repo$dir, "status", "--porcelain"))$stdout)
  }
  expect_identical(changed(.managed_git(repo$git, config, windows = TRUE)), "")
  expect_match(changed(.managed_git(repo$git, config, windows = FALSE)), "notes.txt", fixed = TRUE)
})
