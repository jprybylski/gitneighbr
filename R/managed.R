#' Features a managed session can offer
#'
#' `local`: status, changes, diffs, selected-file commits, `.gitignore`
#' help, explicit init and identity. `remote`: check destination, send and
#' update. The rest are the standalone app's extra actions.
#' @noRd
.MANAGED_FEATURES <- c("local", "remote", "tags", "restore", "trash", "clone", "github")

#' A standalone session's features: everything, plus its own send/update
#' routes (a managed session uses the preview-bound ones instead)
#' @noRd
.STANDALONE_FEATURES <- c(.MANAGED_FEATURES, "standalone")

#' The header a launcher's proxy uses to pass the session token
#' @noRd
.MANAGED_TOKEN_HEADER <- "X-Gitneighbr-Token"

#' The header that carries the shutdown token on `POST api/v1/shutdown`
#' @noRd
.MANAGED_SHUTDOWN_HEADER <- "X-Gitneighbr-Shutdown-Token"

#' Parse a `--features=` value
#' @noRd
.parse_features <- function(features) {
  if (is.null(features)) {
    return("local")
  }
  values <- unique(trimws(unlist(strsplit(as.character(features), ",", fixed = TRUE))))
  values <- values[nzchar(values)]
  unknown <- setdiff(values, .MANAGED_FEATURES)
  if (length(unknown) > 0L) {
    stop("Unknown feature(s): ", paste(unknown, collapse = ", "),
      ". Known: ", paste(.MANAGED_FEATURES, collapse = ", "), ".",
      call. = FALSE
    )
  }
  if (!"local" %in% values) {
    stop("`features` must include \"local\".", call. = FALSE)
  }
  values
}

#' Build the configuration a managed session runs with
#'
#' `NULL` everywhere a managed value is absent means standalone behaviour.
#' @noRd
.managed_config <- function(shutdown_token = NULL, features = "local", policy = NULL) {
  config <- list(
    shutdown_token = shutdown_token,
    features = .parse_features(features),
    policy = policy,
    control = new.env(parent = emptyenv())
  )
  config$control$stop_requested <- FALSE
  config
}

#' Run gitneighbr in the foreground for a launcher
#'
#' Serves one folder until a launcher stops it with `POST api/v1/shutdown`
#' (or the process is killed). Unlike [open_repo()] it blocks, never opens a
#' browser, needs an explicit port, and prints one line to standard output
#' once the socket is bound:
#'
#' `GITNEIGHBR_READY {"url":"http://127.0.0.1:<port><base_path>",...}`
#'
#' The session token is never placed in a URL. A launcher's reverse proxy
#' passes it on every request in the `X-Gitneighbr-Token` header; the page
#' itself never sees it.
#'
#' `inst/scripts/serve.R` is the command-line form of this function; see
#' `vignette("launcher")`.
#'
#' @param repo The folder to serve.
#' @param port Loopback port to listen on.
#' @param git Absolute path to the `git` executable to run.
#' @param api_token Token required on every request except health.
#' @param host Loopback host: `"127.0.0.1"` (default) or `"localhost"`.
#' @param base_path The URL path a reverse proxy serves the app under.
#' @param shutdown_token Token that `POST api/v1/shutdown` must carry in the
#'   `X-Gitneighbr-Shutdown-Token` header. Without it there is no shutdown
#'   route.
#' @param features Comma-separated features to offer (see Details). Always
#'   includes `"local"`.
#' @param git_config The only Git configuration file read besides the
#'   repository's own, and where the identity form saves a name and email. A
#'   launcher passes a file it owns, outside the folder. Without it a
#'   temporary file is used and identity lasts for this session only.
#' @param policy_file A launcher policy file (JSON, outside the folder): how
#'   to group files, which paths no action may change, which need extra
#'   review before sending, and suggested `.gitignore` rules to offer when
#'   the folder is first set up. See `vignette("launcher")`.
#' @param git_exec_path Git's helper folder, for a Git build that needs it
#'   named (one that isn't relocatable on its own). Must be an absolute folder.
#' @param pass_env Names of environment variables (besides a small fixed set
#'   such as `HOME`, `PATH` and the locale) to pass to Git unchanged.
#'
#' @section Git in a managed session:
#' Git runs with none of the computer's or the user's own configuration (on
#' Windows, gitneighbr supplies Git for Windows' defaults for line endings
#' and certificate checks instead), no inherited `GIT_*` or `SSH_*`
#' variables, hooks, fsmonitor, pagers, signing and credential helpers
#' switched off, external diff and text-conversion programs off,
#' and HTTPS as the only transport. A repository whose own settings or
#' attributes need anything else (a filter such as Git LFS, an include, an
#' unknown setting), that lives inside another repository, or that reads
#' from outside its folder is shown as unsupported and can't be changed.
#' @return Invisibly, `0L` once the server has stopped.
#' @export
gitneighbr_serve <- function(repo, port, git, api_token, host = "127.0.0.1", base_path = "/",
                             shutdown_token = NULL, features = "local", git_config = NULL,
                             policy_file = NULL, pass_env = character(), git_exec_path = NULL) {
  .assert_loopback_host(host)
  port <- suppressWarnings(as.integer(port))
  if (length(port) != 1L || is.na(port) || port < 1L || port > 65535L) {
    stop("`port` must be a TCP port number.", call. = FALSE)
  }
  if (!is.character(repo) || length(repo) != 1L || !dir.exists(repo)) {
    stop("`repo` must be an existing folder.", call. = FALSE)
  }
  if (!is.character(git) || length(git) != 1L || !.is_absolute_path(git) || !file.exists(git)) {
    stop("`git` must be the absolute path of an existing Git executable.", call. = FALSE)
  }
  for (secret in list(api_token, shutdown_token)) {
    if (!is.null(secret) && (!is.character(secret) || length(secret) != 1L || nchar(secret) < 16L)) {
      stop("Tokens must be strings of at least 16 characters.", call. = FALSE)
    }
  }
  if (is.null(api_token)) {
    stop("`api_token` is required.", call. = FALSE)
  }

  pass_env <- as.character(pass_env)
  if (any(!grepl("^[A-Za-z_][A-Za-z0-9_]*$", pass_env)) || any(grepl("^(GIT|SSH)_", pass_env))) {
    stop("`pass_env` must name environment variables other than GIT_* and SSH_*.", call. = FALSE)
  }
  if (is.null(git_config)) {
    git_config <- tempfile("gitneighbr-", fileext = ".gitconfig")
  } else if (!.is_absolute_path(git_config) || dir.exists(git_config) || !dir.exists(dirname(git_config))) {
    stop("`git_config` must be an absolute file path in an existing folder.", call. = FALSE)
  }

  repo_root <- as.character(fs::path_real(repo))
  if (startsWith(paste0(.real_path(dirname(git_config)), "/"), paste0(repo_root, "/"))) {
    stop("`git_config` must be outside the folder being served.", call. = FALSE)
  }
  base_path <- .normalize_base_path(base_path)
  policy <- NULL
  if (!is.null(policy_file)) {
    if (startsWith(paste0(.real_path(policy_file)), paste0(repo_root, "/"))) {
      stop("`policy_file` must be outside the folder being served.", call. = FALSE)
    }
    policy <- .read_launcher_policy(policy_file)
  }
  managed <- .managed_config(shutdown_token = shutdown_token, features = features, policy = policy)
  if (!is.null(git_exec_path) && (!.is_absolute_path(git_exec_path) || !dir.exists(git_exec_path))) {
    stop("`git_exec_path` must be an existing absolute folder.", call. = FALSE)
  }
  git_bin <- .managed_git(git, git_config,
    pass_env = pass_env, remote = "remote" %in% managed$features,
    exec_path = git_exec_path
  )
  api <- .build_api(repo_root, git_bin, api_token, port, base_path = base_path, managed = managed)
  .serve_run(api, host, port, base_path, managed$control)
}

#' Is `path` absolute on this platform?
#' @noRd
.is_absolute_path <- function(path) {
  grepl("^(/|[A-Za-z]:[/\\\\]|\\\\\\\\)", path)
}

#' The line a launcher waits for, written once the socket is bound
#' @noRd
.serve_ready_line <- function(host, port, base_path) {
  info <- list(
    url = paste0("http://", host, ":", port, base_path),
    host = host,
    port = port,
    pid = Sys.getpid(),
    version = as.character(utils::packageVersion("gitneighbr"))
  )
  paste("GITNEIGHBR_READY", jsonlite::toJSON(info, auto_unbox = TRUE))
}

#' Run an API until the shutdown route asks it to stop
#'
#' fiery checks for a stop at the end of every service cycle, so a running
#' request (a Git command included) always finishes and its reply is sent
#' before the socket closes.
#' @noRd
.serve_run <- function(api, host, port, base_path, control) {
  ready <- FALSE
  api <- plumber2::api_on(api, "cycle-start", function(...) {
    if (!ready) {
      ready <<- TRUE
      cat(.serve_ready_line(host, port, base_path), "\n", sep = "")
      flush(stdout())
    }
  })
  api <- plumber2::api_on(api, "cycle-end", function(...) {
    if (isTRUE(control$stop_requested)) {
      plumber2::api_stop(api)
    }
  })
  tryCatch(
    plumber2::api_run(api, host = host, port = port, block = TRUE, showcase = FALSE, silent = TRUE),
    interrupt = function(e) invisible(NULL)
  )
  invisible(0L)
}

#' The features each route needs (routes not listed need `local`)
#' @noRd
.ROUTE_FEATURES <- list(
  "api/v1/refresh-remote" = c("remote", "standalone"),
  "api/v1/push" = c("remote", "standalone"),
  "api/v1/update" = c("remote", "standalone"),
  "api/v1/publish" = c("remote", "standalone"),
  "api/v1/remote/check" = "remote",
  "api/v1/remote/connect" = "remote",
  "api/v1/remote/token" = "remote",
  "api/v1/send/preview" = "remote",
  "api/v1/send" = "remote",
  "api/v1/receive/preview" = "remote",
  "api/v1/receive" = "remote",
  "api/v1/credential-diagnosis" = "remote",
  "api/v1/tag" = "tags",
  "api/v1/push-tag" = c("tags", "remote", "standalone"),
  "api/v1/restore" = "restore",
  "api/v1/trash" = "trash",
  "api/v1/clone" = c("clone", "remote"),
  "api/v1/github/status" = "github",
  "api/v1/github/connect" = "github",
  "api/v1/github/disconnect" = "github",
  "api/v1/github/release" = c("github", "remote"),
  "api/v1/github/releases" = "github",
  "api/v1/pull-request" = c("github", "remote")
)

#' The features a route needs
#' @noRd
.route_features <- function(path) {
  .ROUTE_FEATURES[[path]] %||% "local"
}
