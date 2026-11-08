#' Normalise the URL path a reverse proxy serves the app under
#'
#' `NULL`, `""` and `"/"` all mean "served at the root". Anything else must be
#' a plain URL path (letters, digits, `-`, `_`, `.`, `~` and `/`) and comes
#' back with exactly one leading and one trailing slash, e.g.
#' `"apps/gitneighbr"` -> `"/apps/gitneighbr/"`.
#' @noRd
.normalize_base_path <- function(base_path) {
  if (is.null(base_path)) {
    return("/")
  }
  if (!is.character(base_path) || length(base_path) != 1L || is.na(base_path)) {
    stop("`base_path` must be a single string such as \"/apps/gitneighbr/\".", call. = FALSE)
  }
  path <- trimws(base_path)
  if (!nzchar(path) || identical(path, "/")) {
    return("/")
  }
  segments <- strsplit(path, "/", fixed = TRUE)[[1]]
  segments <- segments[nzchar(segments)]
  valid <- length(segments) > 0L &&
    all(grepl("^[A-Za-z0-9._~-]+$", segments)) &&
    !any(segments %in% c(".", ".."))
  if (!valid) {
    stop("`base_path` must be a URL path such as \"/apps/gitneighbr/\", not '", base_path, "'.", call. = FALSE)
  }
  paste0("/", paste(segments, collapse = "/"), "/")
}

#' The frontend directory to serve for a base path
#'
#' At the root the installed `www/` is served unchanged. Under a base path a
#' per-session copy is made whose `index.html` carries `<base href>`, so the
#' relative asset and API URLs resolve under the prefix even when the page
#' was opened without its trailing slash.
#' @noRd
.prepare_www <- function(www_dir, base_path = "/") {
  if (identical(base_path, "/")) {
    return(www_dir)
  }
  target <- tempfile("gitneighbr-www-")
  dir.create(target)
  file.copy(list.files(www_dir, full.names = TRUE), target, recursive = TRUE)
  index_path <- file.path(target, "index.html")
  index <- readLines(index_path, warn = FALSE, encoding = "UTF-8")
  head_at <- grep("<head>", index, fixed = TRUE)[1]
  if (is.na(head_at)) {
    stop("The app's index.html has no <head> element.", call. = FALSE)
  }
  index <- append(index, paste0('    <base href="', base_path, '" />'), after = head_at)
  writeLines(index, index_path, useBytes = TRUE)
  target
}
