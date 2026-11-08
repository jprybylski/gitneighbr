# Run gitneighbr in the foreground for a launcher.
#
#   Rscript --vanilla <library>/gitneighbr/scripts/serve.R --repo=<folder> \
#     --port=<port> --git=<absolute path> --api-token-env=<NAME> [options]
#
# Secrets are read from the environment variables named by --*-token-env, so
# they never appear in argv. See ?gitneighbr::gitneighbr_serve and
# vignette("launcher", package = "gitneighbr").

usage <- paste0(
  "Usage: Rscript --vanilla serve.R --repo=PATH --port=N --git=PATH --api-token-env=NAME [options]\n",
  "\n",
  "  --repo=PATH                the folder to serve\n",
  "  --port=N                   loopback port to listen on\n",
  "  --git=PATH                 absolute path of the git executable\n",
  "  --api-token-env=NAME       environment variable holding the session token\n",
  "  --host=HOST                127.0.0.1 (default) or localhost\n",
  "  --base-path=PATH           path a reverse proxy serves the app under\n",
  "  --shutdown-token-env=NAME  environment variable holding the shutdown token\n",
  "  --features=LIST            comma list: local (default), remote, tags, restore,\n",
  "                             trash, clone, github\n",
  "  --git-config=PATH          Git configuration file to use (outside the folder)\n",
  "  --pass-env=LIST            comma list of extra environment variables for Git\n"
)

`%||%` <- function(x, y) if (is.null(x)) y else x

KNOWN <- c(
  "repo", "port", "git", "api-token-env", "host", "base-path", "shutdown-token-env", "features",
  "git-config", "pass-env"
)
REQUIRED <- c("repo", "port", "git", "api-token-env")

fail <- function(...) {
  cat("gitneighbr serve: ", ..., "\n\n", usage, file = stderr(), sep = "")
  quit(save = "no", status = 2L)
}

parse_args <- function(argv) {
  args <- list()
  for (arg in argv) {
    if (arg %in% c("-h", "--help")) {
      cat(usage)
      quit(save = "no", status = 0L)
    }
    match <- regmatches(arg, regexec("^--([a-z-]+)=(.*)$", arg))[[1]]
    if (length(match) != 3L) {
      fail("expected --name=value, got '", arg, "'.")
    }
    name <- match[[2]]
    if (!name %in% KNOWN) {
      fail("unknown option --", name, ".")
    }
    if (!is.null(args[[name]])) {
      fail("--", name, " given twice.")
    }
    args[[name]] <- match[[3]]
  }
  missing <- setdiff(REQUIRED, names(args))
  if (length(missing) > 0L) {
    fail("missing ", paste0("--", missing, collapse = ", "), ".")
  }
  args
}

secret_from_env <- function(name) {
  if (is.null(name)) {
    return(NULL)
  }
  if (!grepl("^[A-Za-z_][A-Za-z0-9_]*$", name)) {
    fail("'", name, "' is not an environment variable name.")
  }
  value <- Sys.getenv(name, unset = "")
  if (!nzchar(value)) {
    fail("environment variable ", name, " is not set.")
  }
  value
}

args <- parse_args(commandArgs(trailingOnly = TRUE))
api_token <- secret_from_env(args[["api-token-env"]])
shutdown_token <- secret_from_env(args[["shutdown-token-env"]])

status <- tryCatch(
  gitneighbr::gitneighbr_serve(
    repo = args[["repo"]],
    port = args[["port"]],
    git = args[["git"]],
    api_token = api_token,
    host = args[["host"]] %||% "127.0.0.1",
    base_path = args[["base-path"]] %||% "/",
    shutdown_token = shutdown_token,
    features = args[["features"]] %||% "local",
    git_config = args[["git-config"]],
    pass_env = if (is.null(args[["pass-env"]])) character() else strsplit(args[["pass-env"]], ",", fixed = TRUE)[[1]]
  ),
  error = function(e) fail(conditionMessage(e))
)
quit(save = "no", status = status)
