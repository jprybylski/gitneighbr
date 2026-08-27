#' Escape a literal path for safe inclusion as a `.gitignore` rule
#'
#' `.gitignore` pattern syntax treats `\`, `!`, `#`, `*`, `?`, `[`, and `]`
#' specially, and silently trims unescaped trailing whitespace -- so a raw
#' repository-relative path can't just be written as-is if it happens to
#' contain any of those. Each is escaped with a leading backslash, and the
#' whole rule is anchored with a leading `/` so it matches only this one
#' path relative to the repository root, never a same-named file elsewhere
#' in the tree.
#' @noRd
.gitignore_escape_path <- function(path) {
  special <- c("\\", "!", "#", "*", "?", "[", "]")
  for (ch in special) {
    path <- gsub(ch, paste0("\\", ch), path, fixed = TRUE)
  }
  trailing <- regexpr(" +$", path)
  if (trailing != -1L) {
    trailing_len <- attr(trailing, "match.length")
    head <- substr(path, 1, nchar(path) - trailing_len)
    path <- paste0(head, strrep("\\ ", trailing_len))
  }
  paste0("/", path)
}

#' Whether Git already effectively ignores a path
#'
#' Delegates to `git check-ignore`, which accounts for the repository
#' `.gitignore` at every directory level plus `.git/info/exclude` and the
#' user's global excludes file -- broader than just "is this exact rule
#' already in the repo-root `.gitignore`", which is what spec Sec 8.9 point
#' 5 ("duplicate effective rules are not added") asks for.
#' @noRd
.git_path_effectively_ignored <- function(repo_root, git_bin, path) {
  result <- processx::run(
    git_bin, c("-C", repo_root, "check-ignore", "-q", "--", path),
    error_on_status = FALSE, timeout = 15
  )
  identical(result$status, 0L)
}

#' Append one confirmed rule to the repository-root `.gitignore`
#'
#' Implements spec Sec 8.9: the caller proposes the escaped literal path as
#' the default rule, the user confirms the exact text, and this appends it
#' -- creating `.gitignore` if it doesn't exist yet, and otherwise
#' preserving every byte already there. A missing trailing newline on the
#' existing content is fixed up before appending rather than left to
#' corrupt the prior last line.
#'
#' Only ever targets a currently *untracked* path (a fresh
#' `.git_status_entries()` read, mirroring the freshness checks
#' `.git_restore_tracked_file()` / `.git_trash_untracked_file()` use):
#' "stop showing this file" has no meaning for a path Git already tracks,
#' so anything else is refused as `STATE_CHANGED`. If the path is already
#' effectively ignored (`.git_path_effectively_ignored()`), this is a
#' no-op success rather than an appended duplicate line.
#'
#' @return A list. On success: `ok = TRUE`, `path`, `rule`, `added`
#'   (whether a new line was actually written, vs. an already-covered
#'   no-op). On failure: `ok = FALSE`, `code`, `message`, `recoverable`.
#' @noRd
.git_ignore_path <- function(repo_root, git_bin, path) {
  path <- if (is.null(path)) "" else as.character(path)[[1]]

  if (!.validate_repo_relative_path(repo_root, path)) {
    return(list(
      ok = FALSE, code = "PATH_OUTSIDE_REPOSITORY",
      message = "That path is not inside this repository.", recoverable = FALSE
    ))
  }

  status_entries <- .git_status_entries(repo_root, git_bin)
  entry <- Find(function(e) identical(e$path, path), status_entries)
  if (is.null(entry) || entry$kind != "untracked") {
    return(list(
      ok = FALSE, code = "STATE_CHANGED",
      message = "The repository changed since this was shown. Refresh and try again.",
      recoverable = TRUE
    ))
  }

  rule <- .gitignore_escape_path(path)

  if (.git_path_effectively_ignored(repo_root, git_bin, path)) {
    return(list(ok = TRUE, path = path, rule = rule, added = FALSE))
  }

  gitignore_path <- fs::path(repo_root, ".gitignore")
  exists <- fs::file_exists(gitignore_path)
  existing_bytes <- if (exists) {
    readBin(gitignore_path, "raw", n = file.info(gitignore_path)$size)
  } else {
    raw()
  }
  needs_newline <- length(existing_bytes) > 0L && existing_bytes[[length(existing_bytes)]] != as.raw(0x0a)
  new_bytes <- charToRaw(paste0(if (needs_newline) "\n" else "", rule, "\n"))

  appended <- tryCatch(
    {
      con <- file(gitignore_path, open = if (exists) "ab" else "wb")
      on.exit(close(con))
      writeBin(new_bytes, con)
      TRUE
    },
    error = function(e) FALSE
  )
  if (!appended) {
    return(list(
      ok = FALSE, code = "COMMAND_FAILED",
      message = "gitneighbr couldn't write to .gitignore.", recoverable = TRUE
    ))
  }

  list(ok = TRUE, path = path, rule = rule, added = TRUE)
}
