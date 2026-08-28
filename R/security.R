#' Generate a random session token
#'
#' 256 bits of randomness from a CSPRNG, hex-encoded so it is safe to embed
#' in a URL fragment and an HTTP `Authorization: Bearer` header.
#' @noRd
.new_session_token <- function() {
  paste0(as.character(openssl::rand_bytes(32)), collapse = "")
}

#' Validate that a host is loopback-only
#'
#' `gitneighbr` never binds to a non-loopback address in this phase of the
#' project: the app has no authentication beyond the session token and no
#' business being reachable from other machines on the network.
#' @noRd
.assert_loopback_host <- function(host) {
  if (!identical(host, "127.0.0.1") && !identical(host, "localhost")) {
    stop(
      "gitneighbr only supports binding to '127.0.0.1' or 'localhost', not '",
      host, "'.",
      call. = FALSE
    )
  }
  invisible(host)
}

#' Constant-time-ish token comparison
#'
#' Avoids a naive `==` short-circuit timing side channel for token checks.
#' @noRd
.tokens_match <- function(supplied, expected) {
  if (is.null(supplied) || !is.character(supplied) || length(supplied) != 1L) {
    return(FALSE)
  }
  if (nchar(supplied) != nchar(expected)) {
    return(FALSE)
  }
  identical(charToRaw(supplied), charToRaw(expected))
}

#' The `Host`/`Origin` values a loopback-bound server on `port` may legitimately see
#'
#' Both spellings of loopback (`127.0.0.1` and `localhost`) are always
#' accepted regardless of which one the session was actually opened with:
#' either is safe (both stay on-machine), and the browser tab itself picks
#' whichever spelling the initial URL used. A DNS-rebinding attacker's
#' hostname can never equal either literal string, no matter what it
#' resolves to.
#' @noRd
.loopback_authorities <- function(port) {
  c(paste0("127.0.0.1:", port), paste0("localhost:", port))
}

#' Validate a request's `Host` header against the bound loopback port (spec
#' Sec 12.1's DNS-rebinding mitigation)
#'
#' A missing header (not possible for a conformant HTTP/1.1 client, but
#' cheap to check) is rejected along with everything else that isn't an
#' exact `host:port` match.
#' @noRd
.valid_host_header <- function(host_header, port) {
  !is.null(host_header) && host_header %in% .loopback_authorities(port)
}

#' Validate a request's `Origin` header against the bound loopback port
#' (spec Sec 12.2)
#'
#' `Origin` is absent for same-origin navigations in some older clients and
#' for non-browser API callers (curl, an R script) entirely -- neither of
#' which this rejects, since the bearer token is the actual authentication
#' boundary here. When the header *is* present, it must name this exact
#' server over plain HTTP (the local server never serves HTTPS).
#' @noRd
.valid_origin_header <- function(origin_header, port) {
  if (is.null(origin_header) || !nzchar(origin_header)) {
    return(TRUE)
  }
  origin_header %in% paste0("http://", .loopback_authorities(port))
}

#' Generate a short opaque ID for one mutating operation (spec Sec 12.5)
#'
#' Used only for correlating a single mutation across logs and its own
#' response (`X-Operation-Id` header) -- not a secret, so a short ID (8
#' random bytes, hex-encoded) is plenty.
#' @noRd
.new_operation_id <- function() {
  paste0(as.character(openssl::rand_bytes(8)), collapse = "")
}

#' Redact secrets and unnecessary home-directory disclosure from raw Git output
#'
#' Implements spec Sec 15's sanitization requirement for anything from raw
#' Git stderr (or an assembled command line) that might reach the browser or
#' logs via an error's `advanced` block: embedded URL credentials
#' (`https://user:pass@host`), `Authorization: Bearer` tokens, GitHub's
#' PAT-shaped tokens (`ghp_...`, `gho_...`, etc.), common `key=value`/`key:
#' value` credential patterns, and the user's home directory (collapsed to
#' `~`, matching spec Sec 14.2's "prefer a home-relative display path").
#' Best-effort by nature -- this is a diagnostic aid, not a guarantee that no
#' novel credential shape ever leaks -- so callers must still avoid needless
#' disclosure upstream rather than relying on this alone.
#' @noRd
.sanitize_git_output <- function(text) {
  if (is.null(text) || length(text) != 1L || is.na(text) || !nzchar(text)) {
    return("")
  }
  x <- text

  # Embedded URL credentials: https://user:pass@host -> https://***@host
  x <- gsub("(https?://)[^/@\\s]+:[^/@\\s]+@", "\\1***@", x, perl = TRUE)
  # Authorization: Bearer <token>
  x <- gsub("(?i)(bearer\\s+)\\S+", "\\1***", x, perl = TRUE)
  # GitHub PAT-shaped tokens anywhere in the text
  x <- gsub("\\bgh[pousr]_[A-Za-z0-9]{20,}\\b", "***", x, perl = TRUE)
  # Common `key=value` / `key: value` credential-looking fields
  x <- gsub("(?i)\\b(password|passwd|token|secret|api[_-]?key)\\s*[:=]\\s*\\S+", "\\1=***", x, perl = TRUE)

  # Home-directory disclosure: collapse the user's home directory to '~'.
  # Longer of the two candidate spellings first, so e.g. a trailing slash
  # variant doesn't leave a stray fragment behind.
  home_candidates <- unique(c(path.expand("~"), Sys.getenv("HOME", "")))
  home_candidates <- home_candidates[nzchar(home_candidates)]
  home_candidates <- home_candidates[order(-nchar(home_candidates))]
  for (home in home_candidates) {
    x <- gsub(home, "~", x, fixed = TRUE)
  }

  x
}
