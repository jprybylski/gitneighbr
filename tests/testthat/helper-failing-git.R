# A "git" stand-in that fails on one chosen subcommand and otherwise
# delegates to the real `git`. Several functions have a `COMMAND_FAILED`-ish
# branch for a specific step (e.g. `write-tree`, `reset`, `add`, `push`)
# deep inside a sequence of otherwise-successful Git calls; a globally
# broken git_bin fails at the first call instead of the one under test, so
# this lets a test target exactly one step. Shell-script based, so it only
# runs on platforms with a real POSIX shell -- callers should
# `skip_on_os("windows")`.
#' @noRd
.make_failing_git <- function(real_git, fail_on, env = parent.frame()) {
  script <- withr::local_tempfile(.local_envir = env, fileext = ".sh")
  writeLines(c(
    "#!/bin/sh",
    "for arg in \"$@\"; do",
    sprintf("  if [ \"$arg\" = \"%s\" ]; then", fail_on),
    sprintf("    echo 'intentional failure for testing: %s' >&2", fail_on),
    "    exit 7",
    "  fi",
    "done",
    sprintf("exec %s \"$@\"", shQuote(real_git))
  ), script)
  Sys.chmod(script, "0755")
  script
}
