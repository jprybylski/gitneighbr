#' Build a JSON error envelope
#'
#' Mirrors the envelope shape described in the project spec: every response
#' carries `ok`, `data`, `error`, and `status_version`, and every error has a
#' stable `code`, a human `message`, and a `recoverable` flag.
#' @noRd
.error_envelope <- function(code, message, recoverable = TRUE, status_version = NULL) {
  list(
    ok = FALSE,
    data = NULL,
    error = list(
      code = code,
      message = message,
      recoverable = recoverable
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
