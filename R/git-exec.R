#' Run one Git command
#'
#' Every Git call in the package goes through here (a test checks that no
#' other code calls `processx::run(git_bin, ...)`).
#'
#' A standalone session runs the user's own Git as-is, with their
#' environment, hooks, signing and credential helpers (by design; see
#' CLAUDE.md). A managed session's `git_bin` carries a `gitneighbr_managed`
#' attribute (from `.managed_git()`): Git then runs with a minimal
#' environment, gitneighbr's own system configuration instead of the
#' machine's, no user configuration, and fixed `-c` settings
#' that stop repository configuration from starting other programs.
#' `.managed_repo_check()` refuses repositories whose own configuration
#' would still need them.
#'
#' @param git_bin The Git executable, as passed around the package.
#' @param args Git arguments (an argv vector; never a shell string).
#' @param ... Passed on to `processx::run()`.
#' @param env Extra environment variables for this one call (named
#'   character), e.g. `GIT_INDEX_FILE`. Only honoured in managed sessions and
#'   by callers that need them in both modes.
#' @noRd
.git_exec <- function(git_bin, args, ..., env = NULL) {
  managed <- attr(git_bin, "gitneighbr_managed", exact = TRUE)
  command <- as.character(git_bin)
  if (is.null(managed)) {
    if (is.null(env)) {
      return(processx::run(command, args, ...))
    }
    return(processx::run(command, args, ..., env = c("current", env)))
  }
  processx::run(
    command,
    .managed_git_args(managed, args),
    ...,
    env = c(managed$env, env)
  )
}

#' Environment variables a managed Git keeps from the server's environment
#'
#' The launcher controls the server's environment; of that, Git sees only
#' these plus whatever the launcher names in `pass_env`. Every `GIT_*` and
#' `SSH_*` variable is left out.
#' @noRd
.MANAGED_ENV_KEEP <- c(
  "HOME", "USER", "LOGNAME", "USERNAME", "USERPROFILE", "HOMEDRIVE", "HOMEPATH",
  "LANG", "LANGUAGE", "LC_ALL", "LC_CTYPE", "LC_MESSAGES", "TZ",
  "TMPDIR", "TMP", "TEMP", "PATH",
  "SystemRoot", "SYSTEMROOT", "SystemDrive", "SYSTEMDRIVE", "windir", "WINDIR",
  "ComSpec", "COMSPEC", "PATHEXT", "APPDATA", "LOCALAPPDATA", "ProgramData", "PROGRAMDATA"
)

#' Proxy settings a managed Git keeps when remote actions are enabled
#' @noRd
.MANAGED_PROXY_ENV <- c("HTTPS_PROXY", "https_proxy", "HTTP_PROXY", "http_proxy", "NO_PROXY", "no_proxy", "ALL_PROXY", "all_proxy")

#' The system configuration a managed Git reads instead of the machine's
#'
#' Git for Windows installs with defaults that decide how files look on disk
#' and how HTTPS certificates are checked: line endings converted on checkout
#' (`core.autocrlf`) and the Windows certificate store (`schannel`), which
#' also trusts the certificates a company network adds. Without them a
#' folder checked out by another Windows Git tool would show every text file
#' as changed, and Send/Update could fail behind such a network. The rest of
#' that file (Git LFS, credential helpers, text conversion) stays off.
#' Repository and launcher settings still take precedence; on other
#' platforms the file is empty.
#' @noRd
.MANAGED_SYSTEM_CONFIG_WINDOWS <- c(
  "[core]",
  "\tautocrlf = true",
  "\tsymlinks = false",
  "\tfscache = true",
  "[http]",
  "\tsslBackend = schannel",
  "\tschannelUseSSLCAInfo = false"
)

#' Write the managed system configuration file and return its path
#' @noRd
.managed_system_config <- function(windows = .Platform$OS.type == "windows") {
  path <- file.path(tempdir(), if (windows) "gitneighbr-system-windows.gitconfig" else "gitneighbr-system.gitconfig")
  writeLines(if (windows) .MANAGED_SYSTEM_CONFIG_WINDOWS else character(), path)
  normalizePath(path, winslash = "/", mustWork = TRUE)
}

#' Describe a trusted Git executable for a managed session
#'
#' @param git Absolute path of the Git executable the launcher trusts.
#' @param config_file The only configuration file Git reads besides the
#'   repository's own (`GIT_CONFIG_GLOBAL`); identity is written here. A
#'   launcher passes a file it owns, outside the repository.
#' @param pass_env Names of further environment variables to pass to Git
#'   unchanged (for a launcher's own Git setup).
#' @param remote Whether remote actions are enabled (keeps proxy settings).
#' @param protocols Transport protocols Git may use.
#' @param exec_path Git's own helper folder (`git --exec-path`), for a Git
#'   build that isn't relocatable on its own. Passed as `--exec-path=` on
#'   every call, since `GIT_EXEC_PATH` is never inherited.
#' @param windows Use Git for Windows' defaults as the system configuration
#'   (see `.MANAGED_SYSTEM_CONFIG_WINDOWS`). Defaults to the platform.
#' @return `git` with a `gitneighbr_managed` attribute.
#' @noRd
.managed_git <- function(git, config_file, pass_env = character(), remote = FALSE, protocols = "https",
                         exec_path = NULL, windows = .Platform$OS.type == "windows") {
  empty <- file.path(tempdir(), "gitneighbr-empty")
  dir.create(empty, showWarnings = FALSE, recursive = TRUE)
  if (length(list.files(empty, all.files = TRUE, no.. = TRUE)) > 0L) {
    stop("gitneighbr: the managed empty directory is not empty: ", empty, call. = FALSE)
  }
  if (!file.exists(config_file)) {
    file.create(config_file)
  }

  keep <- c(.MANAGED_ENV_KEEP, if (remote) .MANAGED_PROXY_ENV, pass_env)
  inherited <- Sys.getenv()
  inherited <- inherited[names(inherited) %in% keep & !grepl("^(GIT|SSH)_", names(inherited))]
  env <- c(
    stats::setNames(as.character(inherited), names(inherited)),
    GIT_CONFIG_SYSTEM = .managed_system_config(windows),
    GIT_CONFIG_GLOBAL = normalizePath(config_file, winslash = "/", mustWork = TRUE),
    GIT_ATTR_NOSYSTEM = "1",
    GIT_TERMINAL_PROMPT = "0",
    GIT_OPTIONAL_LOCKS = "0",
    GCM_INTERACTIVE = "never",
    GIT_NO_REPLACE_OBJECTS = "1"
  )

  empty <- normalizePath(empty, winslash = "/", mustWork = TRUE)
  config <- c(
    "core.hooksPath" = empty,
    "init.templateDir" = empty,
    "core.fsmonitor" = "false",
    "core.pager" = "cat",
    "core.editor" = "false",
    "sequence.editor" = "false",
    "core.askPass" = "",
    "core.sshCommand" = "false",
    "color.ui" = "never",
    "commit.gpgSign" = "false",
    "tag.gpgSign" = "false",
    "tag.forceSignAnnotated" = "false",
    "push.gpgSign" = "false",
    "credential.helper" = "",
    "submodule.recurse" = "false",
    "fetch.recurseSubmodules" = "false",
    "push.recurseSubmodules" = "no",
    "gc.auto" = "0",
    "maintenance.auto" = "false",
    "protocol.allow" = "never",
    stats::setNames(rep("always", length(protocols)), paste0("protocol.", protocols, ".allow"))
  )
  config_args <- as.vector(rbind("-c", paste0(names(config), "=", config)))
  if (!is.null(exec_path)) {
    config_args <- c(paste0("--exec-path=", normalizePath(exec_path, winslash = "/", mustWork = TRUE)), config_args)
  }

  structure(
    normalizePath(git, winslash = "/", mustWork = TRUE),
    gitneighbr_managed = list(env = env, config_args = config_args, config_file = config_file)
  )
}

#' Git subcommands that must not run external diff or text-conversion programs
#' @noRd
.NO_EXTERNAL_DIFF <- c("diff", "log", "show", "diff-tree")

#' The full argv for a managed Git call
#'
#' Fixed `-c` settings go first (they must precede the subcommand). Diff-
#' producing subcommands also get `--no-ext-diff --no-textconv`.
#' @noRd
.managed_git_args <- function(managed, args) {
  sub_at <- if (length(args) >= 3L && identical(args[[1]], "-C")) 3L else 1L
  if (length(args) >= sub_at && args[[sub_at]] %in% .NO_EXTERNAL_DIFF) {
    args <- append(args, c("--no-ext-diff", "--no-textconv"), after = sub_at)
  }
  c(managed$config_args, args)
}

#' Is this a managed session's Git?
#' @noRd
.is_managed_git <- function(git_bin) {
  !is.null(attr(git_bin, "gitneighbr_managed", exact = TRUE))
}
