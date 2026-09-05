#!/usr/bin/env Rscript
# Entry point for one Playwright e2e spec file's server.
#
# Invoked as `Rscript e2e/fixtures/serve.R <fixture-name> <info-path>`
# from `frontend/e2e/helpers.ts`. Builds the named scratch repo (see
# fixtures/repo.R), starts a real gitneighbr session against it with
# `gitneighbr::open_repo()` (never opening a browser), writes the
# session's authenticated URL to <info-path> once the server is
# confirmed ready, then blocks for the life of the process so Playwright
# can manage this script's lifetime the same way it manages any other
# `webServer`-style command.

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 2) stop("usage: serve.R <fixture-name> <info-path>", call. = FALSE)
fixture_name <- args[[1]]
info_path <- args[[2]]

# Resolve this script's own directory regardless of the working directory
# Playwright launches it from, so `source("repo.R")` and the load_all()
# fallback below both work.
.script_path <- sub("^--file=", "", grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE))
.script_dir <- dirname(normalizePath(.script_path))
.repo_root <- normalizePath(file.path(.script_dir, "..", "..", ".."))

source(file.path(.script_dir, "repo.R"))

# IDENTITY_INCOMPLETE only reproduces if the server's git subprocess can't
# see a real global identity -- point HOME/XDG/GIT_CONFIG_* at an empty
# scratch dir for this one fixture before the server (and the git
# processes it spawns) inherit the environment.
if (identical(fixture_name, "missing-identity")) {
  isolated_home <- .e2e_tempdir()
  Sys.setenv(
    HOME = isolated_home,
    XDG_CONFIG_HOME = file.path(isolated_home, ".config"),
    GIT_CONFIG_GLOBAL = file.path(isolated_home, ".gitconfig"),
    GIT_CONFIG_SYSTEM = file.path(isolated_home, ".no-such-gitconfig")
  )
}

repo <- switch(fixture_name,
  "not-a-repo" = fixture_not_a_repo(),
  "not-a-repo-with-clone-source" = fixture_not_a_repo_with_clone_source(),
  "no-upstream" = fixture_no_upstream(),
  "no-upstream-with-publish-target" = fixture_no_upstream_with_publish_target(),
  "clean-with-remote" = fixture_clean_with_remote(),
  "dirty-with-remote" = fixture_dirty_with_remote(),
  "local-only-with-remote" = fixture_local_only_with_remote(),
  "remote-ahead" = fixture_remote_ahead(),
  "diverged" = fixture_diverged(),
  "conflicted" = fixture_conflicted(),
  "missing-identity" = fixture_missing_identity(),
  "with-policy" = fixture_with_policy(),
  stop("unknown fixture: ", fixture_name, call. = FALSE)
)

pkgload::load_all(.repo_root, quiet = TRUE)

session <- gitneighbr::open_repo(path = repo$dir, browse = FALSE, port = 0L)

# Some fixtures (e.g. a clone source) hand back extra paths the spec file
# needs but that aren't part of the served repo itself.
extra_fields <- ""
if (!is.null(repo$remote_dir)) {
  extra_fields <- sprintf(',"remoteDir":"%s"', repo$remote_dir)
}
writeLines(sprintf('{"url":"%s"%s}', session$url(redact = FALSE), extra_fields), info_path)

while (session$is_alive()) Sys.sleep(0.5)
