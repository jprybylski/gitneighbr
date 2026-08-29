# Scratch git repos for Playwright e2e fixtures.
#
# Deliberately standalone rather than shared with tests/testthat's own
# inline repo-builder helpers (local_git_repo() etc. in
# test-git-status.R/test-git-remote.R/test-git-clone.R): those already
# assume they're running inside a testthat session (skip_if(), a
# withr::local_tempdir() cleanup frame tied to a test's environment).
# This file runs inside a plain Rscript launched by Playwright, so it
# manages its own tempdirs and never calls testthat.
#
# Same hard rule as the rest of the package: git is always invoked as an
# explicit argv vector via processx, never a shell string.

.e2e_git <- unname(Sys.which("git"))
if (!nzchar(.e2e_git)) stop("gitneighbr e2e fixtures: no `git` executable found on PATH.", call. = FALSE)

.e2e_tempdir <- function() {
  dir <- tempfile(pattern = "gitneighbr-e2e-")
  dir.create(dir, recursive = TRUE)
  dir
}

.e2e_run <- function(dir, ...) {
  processx::run(.e2e_git, c("-C", dir, ...), error_on_status = TRUE)
}

# Explicit -c overrides so a commit can be made without ever writing local
# (or picking up global) identity config -- used by fixture_missing_identity()
# so the served repo genuinely has no identity configured.
.e2e_run_as <- function(dir, name, email, ...) {
  processx::run(.e2e_git, c("-C", dir, "-c", paste0("user.name=", name), "-c", paste0("user.email=", email), ...),
    error_on_status = TRUE
  )
}

.e2e_init_repo <- function(dir) {
  .e2e_run(dir, "init", "-q", "-b", "main")
  .e2e_run(dir, "config", "user.email", "seed@example.com")
  .e2e_run(dir, "config", "user.name", "Seed")
}

# A folder that is not a Git working tree at all (NOT_REPOSITORY state).
fixture_not_a_repo <- function() {
  list(dir = .e2e_tempdir())
}

# Same, plus a separate bare dir with one commit to use as a clone source
# (the "Or clone an existing GitHub repository" onboarding path).
fixture_not_a_repo_with_clone_source <- function() {
  source <- fixture_clean_with_remote()
  list(dir = .e2e_tempdir(), remote_dir = source$remote_dir)
}

# git init + one commit, no remote (NO_UPSTREAM state).
fixture_no_upstream <- function() {
  dir <- .e2e_tempdir()
  .e2e_init_repo(dir)
  writeLines("hello", file.path(dir, "README.md"))
  .e2e_run(dir, "add", "README.md")
  .e2e_run(dir, "commit", "-q", "-m", "initial commit")
  list(dir = dir)
}

# Same as fixture_no_upstream(), plus a separate empty bare dir to use as
# the "Connect to GitHub" publish target.
fixture_no_upstream_with_publish_target <- function() {
  fx <- fixture_no_upstream()
  target_dir <- .e2e_tempdir()
  .e2e_run(target_dir, "init", "-q", "--bare", "-b", "main")
  list(dir = fx$dir, remote_dir = target_dir)
}

# A bare dir standing in for "GitHub", seeded with one commit, plus a
# working-tree clone of it with `origin` already configured (READY).
fixture_clean_with_remote <- function() {
  remote_dir <- .e2e_tempdir()
  .e2e_run(remote_dir, "init", "-q", "--bare", "-b", "main")

  seed_dir <- .e2e_tempdir()
  .e2e_init_repo(seed_dir)
  writeLines("hello", file.path(seed_dir, "README.md"))
  .e2e_run(seed_dir, "add", "README.md")
  .e2e_run(seed_dir, "commit", "-q", "-m", "seed commit")
  .e2e_run(seed_dir, "remote", "add", "origin", remote_dir)
  .e2e_run(seed_dir, "push", "-q", "-u", "origin", "main")

  dir <- .e2e_tempdir()
  processx::run(.e2e_git, c("clone", "-q", remote_dir, dir), error_on_status = TRUE)
  .e2e_run(dir, "config", "user.email", "test@example.com")
  .e2e_run(dir, "config", "user.name", "Test")
  list(dir = dir, remote_dir = remote_dir)
}

# Same as above, plus an uncommitted modified tracked file, an untracked
# file, and a second untracked file meant to be added to .gitignore
# (CHANGES_ONLY; feeds commit/restore/trash/ignore flows).
fixture_dirty_with_remote <- function() {
  fx <- fixture_clean_with_remote()
  writeLines(c("hello", "and more"), file.path(fx$dir, "README.md"))
  writeLines("scratch notes", file.path(fx$dir, "notes.txt"))
  writeLines("build output", file.path(fx$dir, "scratch.log"))
  fx
}

# A local commit that hasn't been pushed yet, clean working tree
# (LOCAL_ONLY; standalone "Send to GitHub").
fixture_local_only_with_remote <- function() {
  fx <- fixture_clean_with_remote()
  writeLines("second file", file.path(fx$dir, "second.txt"))
  .e2e_run(fx$dir, "add", "second.txt")
  .e2e_run(fx$dir, "commit", "-q", "-m", "second commit")
  fx
}

# The remote gains a commit after the clone; local working tree stays
# clean (REMOTE_ONLY_CLEAN; fast-forward "Get updates").
fixture_remote_ahead <- function() {
  fx <- fixture_clean_with_remote()
  advance_dir <- .e2e_tempdir()
  processx::run(.e2e_git, c("clone", "-q", fx$remote_dir, advance_dir), error_on_status = TRUE)
  .e2e_run(advance_dir, "config", "user.email", "seed@example.com")
  .e2e_run(advance_dir, "config", "user.name", "Seed")
  writeLines("advanced", file.path(advance_dir, "advanced.txt"))
  .e2e_run(advance_dir, "add", "advanced.txt")
  .e2e_run(advance_dir, "commit", "-q", "-m", "advance the remote")
  .e2e_run(advance_dir, "push", "-q")
  # `ahead`/`behind` come from the clone's own remote-tracking ref, which a
  # plain `git push` from a *different* clone never updates -- fetch here
  # so the served repo's `origin/main` already reflects the new commit
  # without the app itself having to fetch first.
  .e2e_run(fx$dir, "fetch", "-q")
  fx
}

# Unique commits on both sides (DIVERGED; diagnostic-report/handoff card).
fixture_diverged <- function() {
  fx <- fixture_clean_with_remote()

  advance_dir <- .e2e_tempdir()
  processx::run(.e2e_git, c("clone", "-q", fx$remote_dir, advance_dir), error_on_status = TRUE)
  .e2e_run(advance_dir, "config", "user.email", "seed@example.com")
  .e2e_run(advance_dir, "config", "user.name", "Seed")
  writeLines("remote-side change", file.path(advance_dir, "remote-only.txt"))
  .e2e_run(advance_dir, "add", "remote-only.txt")
  .e2e_run(advance_dir, "commit", "-q", "-m", "remote-only commit")
  .e2e_run(advance_dir, "push", "-q")
  # See fixture_remote_ahead()'s comment: fetch so the served clone's own
  # origin/main already reflects the remote-only commit.
  .e2e_run(fx$dir, "fetch", "-q")

  writeLines("local-side change", file.path(fx$dir, "local-only.txt"))
  .e2e_run(fx$dir, "add", "local-only.txt")
  .e2e_run(fx$dir, "commit", "-q", "-m", "local-only commit")
  fx
}

# git init + a commit made without ever configuring local/global identity
# (IDENTITY_INCOMPLETE notice) -- serve.R additionally isolates HOME for
# this fixture so the machine's real global identity isn't visible to the
# server's git subprocess.
fixture_missing_identity <- function() {
  dir <- .e2e_tempdir()
  .e2e_run(dir, "init", "-q", "-b", "main")
  writeLines("hello", file.path(dir, "README.md"))
  .e2e_run(dir, "add", "README.md")
  .e2e_run_as(dir, "Seed", "seed@example.com", "commit", "-q", "-m", "initial commit")
  list(dir = dir)
}
