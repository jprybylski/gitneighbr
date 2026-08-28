#' Classify a remote URL's transport
#'
#' @return One of `"https"`, `"ssh"`, `"git"`, `"other"`, or `NULL` if `url`
#'   is `NULL`/empty. The scp-like SSH shorthand (`git@host:path`) has no
#'   scheme to match on, so it is recognized by the `user@host:` shape
#'   instead -- deliberately checked after the `://`-based schemes so a
#'   URL like `ssh://git@host/path` (which also contains an `@`) is not
#'   misclassified.
#' @noRd
.git_remote_transport <- function(url) {
  if (is.null(url) || !nzchar(url)) {
    return(NULL)
  }
  if (grepl("^https?://", url, ignore.case = TRUE)) {
    return("https")
  }
  if (grepl("^ssh://", url, ignore.case = TRUE)) {
    return("ssh")
  }
  if (grepl("^git://", url, ignore.case = TRUE)) {
    return("git")
  }
  if (grepl("^[^/@[:space:]]+@[^/@[:space:]]+:", url)) {
    return("ssh")
  }
  "other"
}

#' The current OS family, for platform-specific credential guidance
#' @return One of `"windows"`, `"macos"`, `"linux"`, `"other"`.
#' @noRd
.platform_name <- function() {
  sysname <- Sys.info()[["sysname"]]
  if (identical(sysname, "Windows")) {
    return("windows")
  }
  if (identical(sysname, "Darwin")) {
    return("macos")
  }
  if (identical(sysname, "Linux")) {
    return("linux")
  }
  "other"
}

#' Whether an SSH agent is reachable and what it holds, via `ssh-add -l`
#'
#' Read-only: this never adds, removes, or unlocks a key, and never prompts
#' for a passphrase -- `ssh-add -l` only lists fingerprints already loaded
#' (spec Sec 16: gitneighbr never collects, transmits, or stores
#' credentials). Exit status distinguishes the three states `ssh-add`
#' defines: `0` (agent running, keys loaded), `1` (agent running, no keys),
#' and anything else (no agent reachable at all, e.g. `SSH_AUTH_SOCK` unset).
#' @param ssh_add Path to the `ssh-add` executable to run; overridable so
#'   tests can point this at a fake executable with a controlled exit code
#'   instead of depending on the real local SSH agent's state.
#' @return A list with `available` (whether an `ssh-add` executable was
#'   found at all), `running`, `has_keys`, `key_count`, and `detail` (a
#'   human sentence, `NULL` when keys are present).
#' @noRd
.ssh_agent_status <- function(ssh_add = unname(Sys.which("ssh-add"))) {
  if (!nzchar(ssh_add)) {
    return(list(
      available = FALSE, running = FALSE, has_keys = FALSE, key_count = 0L,
      detail = "No `ssh-add` executable was found alongside Git."
    ))
  }
  result <- tryCatch(
    processx::run(ssh_add, "-l", error_on_status = FALSE, timeout = 5),
    error = function(e) NULL
  )
  if (is.null(result)) {
    return(list(
      available = TRUE, running = FALSE, has_keys = FALSE, key_count = 0L,
      detail = "Could not run `ssh-add` to check for loaded keys."
    ))
  }
  if (identical(result$status, 0L)) {
    lines <- strsplit(trimws(result$stdout), "\n", fixed = TRUE)[[1]]
    lines <- lines[nzchar(lines)]
    return(list(available = TRUE, running = TRUE, has_keys = TRUE, key_count = length(lines), detail = NULL))
  }
  if (identical(result$status, 1L)) {
    return(list(
      available = TRUE, running = TRUE, has_keys = FALSE, key_count = 0L,
      detail = "An SSH agent is running, but it has no keys loaded."
    ))
  }
  list(
    available = TRUE, running = FALSE, has_keys = FALSE, key_count = 0L,
    detail = "No SSH agent is reachable."
  )
}

#' Platform-specific guidance for a missing/unconfigured HTTPS credential helper
#' @noRd
.https_helper_guidance <- function(platform) {
  switch(platform,
    windows = paste(
      "Install Git Credential Manager (bundled with Git for Windows), or run",
      "`git config --global credential.helper manager`, then retry."
    ),
    macos = paste(
      "Enable the built-in helper with",
      "`git config --global credential.helper osxkeychain`, or install Git Credential Manager, then retry."
    ),
    linux = paste(
      "Install Git Credential Manager, or enable a helper such as",
      "`git config --global credential.helper libsecret` (or your distribution's keyring integration), then retry."
    ),
    "Configure a Git credential helper for HTTPS authentication (see Git Credential Manager), then retry."
  )
}

#' Platform-specific guidance for a missing/empty SSH agent
#' @noRd
.ssh_agent_guidance <- function(platform) {
  switch(platform,
    windows = paste(
      "Start the \"OpenSSH Authentication Agent\" Windows service, or run",
      "`eval $(ssh-agent -s)` in Git Bash, then load your key with `ssh-add ~/.ssh/id_ed25519`, then retry."
    ),
    macos = paste(
      "Load your key into the agent with",
      "`ssh-add --apple-use-keychain ~/.ssh/id_ed25519` (use your key's actual path), then retry."
    ),
    linux = paste(
      "Run `eval $(ssh-agent -s)`, then load your key with",
      "`ssh-add ~/.ssh/id_ed25519` (use your key's actual path), then retry."
    ),
    "Start your SSH agent and load your GitHub key with `ssh-add`, then retry."
  )
}

#' Guidance when a credential appears expired or revoked rather than absent
#' @noRd
.expired_credential_guidance <- function() {
  paste(
    "Your saved GitHub credential may be expired or revoked. Generate a new",
    "personal access token (or re-authenticate through Git Credential Manager",
    "or your OS keychain) using your normal GitHub sign-in, then retry -- gitneighbr",
    "never collects or stores this credential itself."
  )
}

#' Diagnose why GitHub authentication might be failing for a repository
#'
#' Implements spec Sec 16 step 3 ("provide platform-appropriate setup
#' guidance") and issue-18 credential diagnostics: read-only introspection
#' of the configured remote's transport, whether an HTTPS credential helper
#' or SSH agent with loaded keys is in place, and -- when the sanitized
#' stderr of a just-failed fetch/push is supplied -- whether GitHub's own
#' message suggests an expired or revoked credential specifically. Never
#' invokes a credential helper or unlocks/prompts an SSH agent (e.g. never
#' runs `git credential fill`), so it can never itself trigger the kind of
#' external prompt spec Sec 16 reserves for the user to complete outside
#' gitneighbr.
#'
#' @param stderr_text Optional sanitized stderr from the most recently
#'   failed fetch/push against this repository's remote.
#' @return A list: `transport` (`"https"`, `"ssh"`, `"git"`, `"other"`, or
#'   `NULL` if no remote is configured), `platform` (`"windows"`,
#'   `"macos"`, `"linux"`, or `"other"`), `checks` (a named list of
#'   `gitneighbr_doctor_check` objects, reusing the same shape [doctor()]
#'   uses), and `guidance` (a character vector of platform-appropriate next
#'   steps, empty when nothing looks wrong).
#' @noRd
.git_credential_diagnosis <- function(repo_root, git_bin, stderr_text = NULL) {
  remote_url <- .git_remote_url(repo_root, git_bin)
  transport <- .git_remote_transport(remote_url)
  platform <- .platform_name()
  checks <- list()
  guidance <- character()

  checks$remote <- if (is.null(remote_url)) {
    .doctor_check("cred_remote", "fail", "No 'origin' remote is configured, so there is nothing to authenticate to.")
  } else {
    .doctor_check("cred_remote", "ok", paste0("Remote transport: ", transport %||% "unknown", "."))
  }

  if (identical(transport, "https")) {
    helpers <- .git_credential_helpers(repo_root, git_bin)
    if (length(helpers) > 0L) {
      checks$credential_helper <- .doctor_check(
        "credential_helper", "ok",
        paste0("Credential helper configured: ", paste(helpers, collapse = ", "), "."),
        list(helpers = helpers)
      )
    } else {
      checks$credential_helper <- .doctor_check(
        "credential_helper", "fail",
        "No Git credential helper is configured for HTTPS authentication."
      )
      guidance <- c(guidance, .https_helper_guidance(platform))
    }
  } else if (identical(transport, "ssh")) {
    agent <- .ssh_agent_status()
    checks$ssh_agent <- if (isTRUE(agent$has_keys)) {
      .doctor_check(
        "ssh_agent", "ok",
        paste0("SSH agent is running with ", agent$key_count, " key(s) loaded."),
        list(key_count = agent$key_count)
      )
    } else {
      .doctor_check("ssh_agent", "fail", agent$detail %||% "No SSH agent with loaded keys was found.")
    }
    if (!isTRUE(agent$has_keys)) {
      guidance <- c(guidance, .ssh_agent_guidance(platform))
    }
  }

  if (!is.null(stderr_text) && grepl("expired|revoked|bad credentials", tolower(stderr_text))) {
    checks$token <- .doctor_check(
      "token_expired", "fail",
      "GitHub reported the credential as invalid, expired, or revoked."
    )
    guidance <- c(guidance, .expired_credential_guidance())
  }

  # `.doctor_check()` objects carry class `gitneighbr_doctor_check` for
  # `print.gitneighbr_doctor_report()`'s benefit; jsonlite refuses to
  # serialize a classed object with no registered `asJSON` method, and
  # unlike `doctor()`'s report this diagnosis crosses the HTTP boundary
  # (spec Sec 14's JSON envelope), so the class must be stripped here.
  list(
    transport = transport,
    platform = platform,
    checks = lapply(checks, unclass),
    guidance = unique(guidance)
  )
}
