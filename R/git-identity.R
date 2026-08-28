#' Read one Git config key's effective value and the scope it came from
#'
#' Uses `--show-scope`, available since Git 2.26 (older than
#' `.min_git_version`, so always safe here), to report which config file the
#' effective value came from (`"local"`, `"global"`, `"system"`,
#' `"worktree"`, or `"command"`) alongside the value itself -- without this,
#' a value inherited from `~/.gitconfig` would be indistinguishable from one
#' set on this repository specifically.
#' @return A list with `value` and `scope`, both `NULL` if the key is unset
#'   anywhere.
#' @noRd
.git_config_scoped <- function(repo_root, git_bin, key) {
  result <- tryCatch(
    processx::run(
      git_bin, c("-C", repo_root, "config", "--show-scope", "--get", key),
      error_on_status = FALSE, timeout = 5
    ),
    error = function(e) NULL
  )
  if (is.null(result) || !identical(result$status, 0L)) {
    return(list(value = NULL, scope = NULL))
  }
  line <- sub("\n$", "", result$stdout)
  parts <- strsplit(line, "\t", fixed = TRUE)[[1]]
  if (length(parts) < 2L) {
    return(list(value = NULL, scope = NULL))
  }
  list(scope = parts[[1]], value = paste(parts[-1], collapse = "\t"))
}

#' The effective Git identity (`user.name`/`user.email`) for a repository
#'
#' @return A list: `name`, `name_scope`, `email`, `email_scope` (each `NULL`
#'   when unset), and `complete` (`TRUE` only when both are set to a
#'   non-empty value).
#' @noRd
.git_identity <- function(repo_root, git_bin) {
  name <- .git_config_scoped(repo_root, git_bin, "user.name")
  email <- .git_config_scoped(repo_root, git_bin, "user.email")
  list(
    name = name$value,
    name_scope = name$scope,
    email = email$value,
    email_scope = email$scope,
    complete = !is.null(name$value) && nzchar(name$value) && !is.null(email$value) && nzchar(email$value)
  )
}

#' Validate a proposed `user.name` value
#'
#' Git itself imposes no format rules on this field; the only thing that
#' would actually break is a newline (Git config values are stored one per
#' line), so that -- plus a sane length cap -- is all this checks.
#' @noRd
.validate_identity_name <- function(name) {
  if (is.null(name) || length(name) != 1L || !is.character(name) || is.na(name)) {
    return(FALSE)
  }
  trimmed <- trimws(name)
  nzchar(trimmed) && !grepl("[\r\n]", trimmed) && nchar(trimmed, type = "chars") <= 200L
}

#' Validate a proposed `user.email` value
#'
#' A light shape check (`local@domain.tld`), not full RFC 5322 validation --
#' Git accepts anything here, so this exists only to catch an obvious typo
#' before writing it to config, not to police what counts as a real address.
#' @noRd
.validate_identity_email <- function(email) {
  if (is.null(email) || length(email) != 1L || !is.character(email) || is.na(email)) {
    return(FALSE)
  }
  trimmed <- trimws(email)
  nchar(trimmed, type = "chars") <= 320L &&
    grepl("^[^\\s@]+@[^\\s@]+\\.[^\\s@]+$", trimmed, perl = TRUE)
}

#' Set `user.name`/`user.email`, guiding a new user through `git config`
#' without them needing to know it exists
#'
#' Writes both keys with two separate `git config` invocations so a failure
#' partway through (e.g. an unwritable global config file) is reported
#' precisely rather than silently leaving only one of the pair set.
#'
#' @param scope Either `"global"` (the person's identity, reused across
#'   every repository -- the default, since Git identity is fundamentally
#'   about who someone is, not which project they're in) or `"local"`
#'   (written only to this repository's `.git/config`, for someone who
#'   deliberately wants a different identity here).
#' @return A list. On success: `ok = TRUE`, `name`, `email`, `scope`. On
#'   failure: `ok = FALSE`, `code`, `message`, `recoverable`.
#' @noRd
.git_set_identity <- function(repo_root, git_bin, name, email, scope = "global") {
  scope <- if (identical(scope, "local")) "local" else "global"
  name <- if (is.null(name)) "" else trimws(as.character(name)[[1]])
  email <- if (is.null(email)) "" else trimws(as.character(email)[[1]])

  if (!.validate_identity_name(name)) {
    return(list(
      ok = FALSE, code = "INVALID_NAME",
      message = "Enter a name.", recoverable = TRUE
    ))
  }
  if (!.validate_identity_email(email)) {
    return(list(
      ok = FALSE, code = "INVALID_EMAIL",
      message = "Enter a valid email address.", recoverable = TRUE
    ))
  }

  scope_flag <- paste0("--", scope)
  name_args <- c("-C", repo_root, "config", scope_flag, "user.name", name)
  name_result <- processx::run(git_bin, name_args, error_on_status = FALSE, timeout = 5)
  if (!identical(name_result$status, 0L)) {
    return(list(
      ok = FALSE, code = "COMMAND_FAILED",
      message = "Could not save your name.", recoverable = TRUE,
      advanced = .advanced_block(name_args, name_result)
    ))
  }

  email_args <- c("-C", repo_root, "config", scope_flag, "user.email", email)
  email_result <- processx::run(git_bin, email_args, error_on_status = FALSE, timeout = 5)
  if (!identical(email_result$status, 0L)) {
    return(list(
      ok = FALSE, code = "COMMAND_FAILED",
      message = "Could not save your email.", recoverable = TRUE,
      advanced = .advanced_block(email_args, email_result)
    ))
  }

  list(ok = TRUE, name = name, email = email, scope = scope)
}
