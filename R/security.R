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
