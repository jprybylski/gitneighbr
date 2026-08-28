#' The application's ~25-code error taxonomy (spec Sec 15)
#'
#' Maps every stable application error code to a short, user-facing title.
#' The per-call-site `message` (passed to `.error_envelope()`) carries the
#' situation-specific sentence; `title` is the stable, code-derived heading
#' spec Sec 14 pairs with it in the API envelope. A handful of codes beyond
#' spec Sec 15's table are included because the codebase already emits them
#' for cases the spec's table doesn't separately enumerate (e.g.
#' `PATH_IS_DIRECTORY`, `TRASH_UNAVAILABLE`).
#' @noRd
.error_titles <- c(
  GIT_UNAVAILABLE = "Git isn't available",
  GIT_TOO_OLD = "Git needs updating",
  NOT_REPOSITORY = "Not a Git project",
  BARE_REPOSITORY = "No working files here",
  IDENTITY_MISSING = "Git needs your name and email",
  INVALID_NAME = "Name isn't valid",
  INVALID_EMAIL = "Email isn't valid",
  DETACHED_HEAD = "Not on a branch",
  NO_UPSTREAM = "Not connected to GitHub",
  REMOTE_NOT_GITHUB = "Remote isn't GitHub",
  AUTH_REQUIRED = "GitHub access needed",
  REMOTE_UNREACHABLE = "Can't reach GitHub",
  REMOTE_AHEAD = "GitHub has newer work",
  DIVERGED = "Histories need to be combined",
  DIRTY_BLOCKS_UPDATE = "Unsaved changes in the way",
  CONFLICTS_PRESENT = "Conflicts need attention",
  PROTECTED_BRANCH = "GitHub rejected this",
  HOOK_FAILED = "A repository hook rejected this",
  SIGNING_FAILED = "Signing failed",
  LARGE_FILE_REJECTED = "File too large",
  TAG_EXISTS = "That version label already exists",
  INVALID_TAG = "Not a valid version label",
  INVALID_SUMMARY = "Summary isn't valid",
  EMPTY_SELECTION = "Nothing selected",
  EMPTY_COMMIT = "No changes to save",
  STATE_CHANGED = "Repository changed",
  OPERATION_IN_PROGRESS = "Another action is already running",
  PATH_OUTSIDE_REPOSITORY = "Path is outside this repository",
  PATH_IS_DIRECTORY = "That's a folder, not a file",
  TRASH_UNAVAILABLE = "Couldn't move file to trash",
  COMMAND_FAILED = "Git command failed"
)

#' Look up the stable title for an application error code
#' @noRd
.error_title <- function(code) {
  title <- unname(.error_titles[code])
  if (is.na(title)) "Something went wrong" else title
}

#' Build the `advanced` block for an error: exact command, exit status, and
#' sanitized stderr (spec Sec 15 / Sec 9.1's advanced-details disclosure)
#'
#' @param args The argv (without the leading `git` binary path) passed to
#'   `processx::run()`.
#' @param result The list `processx::run()` returned (`status`, `stderr`).
#' @noRd
.advanced_block <- function(args, result) {
  command <- .sanitize_git_output(paste(c("git", as.character(args)), collapse = " "))
  list(
    command = command,
    exit_status = result$status %||% NA_integer_,
    stderr = .sanitize_git_output(result$stderr)
  )
}

#' Build a JSON error envelope
#'
#' Mirrors the envelope shape described in the project spec: every response
#' carries `ok`, `data`, `error`, and `status_version`, and every error has a
#' stable `code`, a stable `title`, a human `message`, and a `recoverable`
#' flag. `data` is `NULL` for ordinary errors, but a `409 STATE_CHANGED`
#' rejection embeds fresh status data here so the frontend can update its
#' display without an extra round trip (spec Sec 7.3). `advanced`, when
#' supplied, carries the exact Git command, its exit status, and sanitized
#' stderr behind the frontend's "Advanced details" disclosure (spec Sec 9.1);
#' it is omitted (`NULL`) for errors with no underlying failed Git command.
#' `diagnosis`, when supplied (only ever alongside `code = "AUTH_REQUIRED"`),
#' carries `.git_credential_diagnosis()`'s platform-appropriate guidance
#' (spec Sec 16 step 3).
#' @noRd
.error_envelope <- function(code, message, recoverable = TRUE, status_version = NULL, data = NULL, advanced = NULL, diagnosis = NULL) {
  list(
    ok = FALSE,
    data = data,
    error = list(
      code = code,
      title = .error_title(code),
      message = message,
      recoverable = recoverable,
      advanced = advanced,
      diagnosis = diagnosis
    ),
    status_version = status_version
  )
}

#' Build a JSON success envelope
#' @noRd
.ok_envelope <- function(data, status_version = NULL) {
  list(
    ok = TRUE,
    data = data,
    error = NULL,
    status_version = status_version
  )
}

#' Signal a condition classed for a given gitneighbr error code
#'
#' Lets callers use `tryCatch(..., gitneighbr_error = function(e) ...)` while
#' still carrying the stable API error code on the condition object.
#' @noRd
.gitneighbr_error <- function(code, message, recoverable = TRUE) {
  structure(
    class = c(paste0("gitneighbr_error_", tolower(code)), "gitneighbr_error", "error", "condition"),
    list(message = message, call = sys.call(-1), code = code, recoverable = recoverable)
  )
}

#' Raise a `.gitneighbr_error()` condition as an R error
#'
#' A thin `stop()` wrapper so call sites outside the HTTP layer (e.g.
#' [open_repo()]) can signal a stable application error code (spec Sec 15)
#' the same way `tryCatch(..., gitneighbr_error = function(e) ...)` callers
#' expect, without constructing the condition object inline.
#' @noRd
.stop_gitneighbr_error <- function(code, message, recoverable = FALSE) {
  stop(.gitneighbr_error(code, message, recoverable = recoverable))
}
