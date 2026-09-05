#' Check whether something is listening on host:port
#' @noRd
.port_is_open <- function(host, port) {
  con <- tryCatch(
    suppressWarnings(socketConnection(host = host, port = port, timeout = 1, open = "r+")),
    error = function(e) NULL
  )
  if (is.null(con)) {
    return(FALSE)
  }
  close(con)
  TRUE
}

#' Locate the gitneighbr package source/install root usable by a child Rscript
#' @noRd
.package_root <- function() {
  path <- tryCatch(find.package("gitneighbr"), error = function(e) NULL)
  if (is.null(path)) {
    stop(
      "gitneighbr: package not found on the search path. Install it, or run ",
      "devtools::load_all() first.",
      call. = FALSE
    )
  }
  path
}

#' Spawn the server as a background OS process
#'
#' Isolated into its own function so it is easy to mock in tests. Uses
#' `processx` (not `callr`/`later`/`httpuv`) so the server is a real,
#' independent OS process: killing the parent R session does not orphan it
#' (thanks to `cleanup = TRUE`), and it can be found and killed later purely
#' from its PID/port, with no live R object required.
#' @noRd
.launch_server_process <- function(pkg_root, env_vars) {
  child_expr <- paste0(
    "if (requireNamespace('gitneighbr', quietly = TRUE)) { library(gitneighbr) } ",
    "else { pkgload::load_all(", shQuote(pkg_root), ", quiet = TRUE) }; ",
    "gitneighbr:::.run_server()"
  )
  processx::process$new(
    command = file.path(R.home("bin"), "Rscript"),
    args = c("--vanilla", "-e", child_expr),
    env = env_vars,
    stdout = "|",
    stderr = "|",
    cleanup = TRUE,
    cleanup_tree = TRUE
  )
}

#' Poll until the server is accepting connections, or fail fast
#' @noRd
.wait_for_server <- function(host, port, process, timeout = 10) {
  deadline <- Sys.time() + timeout
  while (Sys.time() < deadline) {
    if (!process$is_alive()) {
      process$wait()
      stderr <- tryCatch(process$read_all_error(), error = function(e) "")
      stop(
        "gitneighbr: server process exited during startup.\n",
        stderr,
        call. = FALSE
      )
    }
    if (.port_is_open(host, port)) {
      return(invisible(TRUE))
    }
    Sys.sleep(0.2)
  }
  process$kill_tree()
  stop("gitneighbr: server did not become ready within ", timeout, " seconds.", call. = FALSE)
}

#' Launch the gitneighbr app for a repository, without blocking the console
#'
#' @param repo_root Canonical repository root.
#' @param host Loopback host to bind to.
#' @param port Port to bind to, or `0L` to pick a free ephemeral port.
#' @param git_bin Path to the `git` executable.
#' @return A `gitneighbr_session` object (see session.R).
#' @noRd
.gitneighbr_serve <- function(repo_root, host = "127.0.0.1", port = 0L, git_bin = Sys.which("git")) {
  .assert_loopback_host(host)

  if (identical(port, 0L)) {
    port <- .find_free_port()
  }
  if (.port_is_open(host, port)) {
    stop("gitneighbr: port ", port, " on ", host, " is already in use.", call. = FALSE)
  }

  token <- .new_session_token()
  pkg_root <- .package_root()

  # "current" merges these on top of the child's inherited environment
  # (processx replaces the environment outright otherwise) - the Git
  # subprocess needs PATH, HOME, SSH_AUTH_SOCK, etc. for credential
  # helpers and SSH agents to work. Every value is unnamed defensively:
  # a named scalar (e.g. from Sys.which()) silently mangles the env var
  # name when combined with c(name = value).
  process <- .launch_server_process(
    pkg_root,
    env_vars = c(
      "current",
      GITNEIGHBR_REPO = unname(repo_root),
      GITNEIGHBR_HOST = unname(host),
      GITNEIGHBR_PORT = as.character(port),
      GITNEIGHBR_TOKEN = unname(token),
      GITNEIGHBR_GIT = unname(git_bin)
    )
  )

  .wait_for_server(host, port, process)

  GitneighbrSession$new(
    process = process,
    host = host,
    port = port,
    token = token,
    repo_root = repo_root
  )
}

#' Run a PowerShell command and return its processx result, or NULL on error
#' @noRd
.run_powershell <- function(command) {
  tryCatch(
    processx::run("powershell", c("-NoProfile", "-NonInteractive", "-Command", command),
      error_on_status = FALSE, timeout = 5
    ),
    error = function(e) NULL
  )
}

#' Find PIDs listening on a TCP port
#'
#' `lsof` doesn't exist on Windows, so that branch parses `netstat -ano`
#' instead -- unlike PowerShell's `Get-NetTCPConnection` (a NetTCPIP module
#' cmdlet, subject to module-autoload/version quirks), `netstat` is a plain
#' built-in binary present and stable on every Windows version.
#' @noRd
.pids_listening_on_port <- function(port) {
  if (identical(.Platform$OS.type, "windows")) {
    result <- tryCatch(
      processx::run("netstat", c("-ano", "-p", "TCP"), error_on_status = FALSE, timeout = 5),
      error = function(e) NULL
    )
    if (is.null(result)) {
      return(integer())
    }
    lines <- strsplit(result$stdout, "\r?\n")[[1]]
    listening <- lines[grepl("LISTENING", lines, fixed = TRUE)]
    # Local address column is `host:port` (host may be 0.0.0.0, 127.0.0.1,
    # or [::]) -- match ":<port>" followed by whitespace so e.g. port 9520
    # can't match a local address ending in ...:19520.
    matching <- listening[grepl(paste0(":", port, "(?=\\s)"), listening, perl = TRUE)]
    pids <- vapply(matching, function(l) {
      fields <- strsplit(trimws(l), "\\s+")[[1]]
      suppressWarnings(as.integer(fields[length(fields)]))
    }, integer(1), USE.NAMES = FALSE)
    return(unique(pids[!is.na(pids)]))
  }
  result <- tryCatch(
    processx::run("lsof", c("-ti", paste0("TCP:", port), "-sTCP:LISTEN"), error_on_status = FALSE, timeout = 5),
    error = function(e) NULL
  )
  if (is.null(result) || !nzchar(trimws(result$stdout))) {
    return(integer())
  }
  as.integer(strsplit(trimws(result$stdout), "\n")[[1]])
}

#' Does this PID's command line look like a gitneighbr server?
#' @noRd
.pid_looks_like_gitneighbr_server <- function(pid) {
  result <- if (identical(.Platform$OS.type, "windows")) {
    .run_powershell(sprintf(
      "(Get-CimInstance Win32_Process -Filter 'ProcessId=%d').CommandLine",
      as.integer(pid)
    ))
  } else {
    tryCatch(
      processx::run("ps", c("-o", "command=", "-p", as.character(pid)), error_on_status = FALSE, timeout = 5),
      error = function(e) NULL
    )
  }
  if (is.null(result)) {
    return(FALSE)
  }
  cmd <- result$stdout
  grepl("gitneighbr", cmd, fixed = TRUE) && grepl(".run_server", cmd, fixed = TRUE)
}

#' Get a PID's parent PID
#' @noRd
.parent_pid <- function(pid) {
  result <- if (identical(.Platform$OS.type, "windows")) {
    .run_powershell(sprintf(
      "(Get-CimInstance Win32_Process -Filter 'ProcessId=%d').ParentProcessId",
      as.integer(pid)
    ))
  } else {
    tryCatch(
      processx::run("ps", c("-o", "ppid=", "-p", as.character(pid)), error_on_status = FALSE, timeout = 5),
      error = function(e) NULL
    )
  }
  if (is.null(result) || !nzchar(trimws(result$stdout))) {
    return(NA_integer_)
  }
  as.integer(trimws(result$stdout))
}

#' Kill a single PID, verified to be a gitneighbr server, and its matching parent
#' @noRd
.kill_gitneighbr_server_pid <- function(pid) {
  if (!.pid_looks_like_gitneighbr_server(pid)) {
    stop(
      "gitneighbr: refusing to kill PID ", pid, " because it does not look like ",
      "a gitneighbr server process.",
      call. = FALSE
    )
  }
  tools::pskill(pid, signal = tools::SIGKILL)

  parent <- .parent_pid(pid)
  if (!is.na(parent) && .pid_looks_like_gitneighbr_server(parent)) {
    tools::pskill(parent, signal = tools::SIGKILL)
  }
  invisible(TRUE)
}

#' Stop whatever gitneighbr server is listening on a port
#'
#' Recovery path for when the `gitneighbr_session` handle has been lost
#' (e.g. the R session that started it was restarted). Never silently
#' no-ops on an unrelated process occupying the port.
#' @noRd
.stop_server_on_port <- function(host, port) {
  if (!.port_is_open(host, port)) {
    return(invisible(FALSE))
  }
  pids <- .pids_listening_on_port(port)
  if (length(pids) == 0L) {
    stop(
      "gitneighbr: port ", port, " is open but no listening PID could be found ",
      "(unsupported platform for stop-by-port, or a permissions issue).",
      call. = FALSE
    )
  }
  for (pid in pids) {
    .kill_gitneighbr_server_pid(pid)
  }
  invisible(TRUE)
}
