#' The well-known empty-tree object ID
#'
#' Always valid in any Git repository (it needs no object to exist), so
#' diffing against it gives a uniform "everything is new" base for an unborn
#' branch instead of a special-cased code path.
#' @noRd
.git_empty_tree_sha <- "4b825dc642cb6eb9a060e54bf8d69288fbee4904"

#' Run a Git command and read its raw stdout bytes
#'
#' `processx::run()`'s pipe-based stdout capture does not tolerate embedded
#' NUL bytes reliably, so `-z` output is redirected to a file and read back
#' with `readBin()` instead. Returns the raw vector, not a string, so the
#' caller can split on NUL before ever converting to R character data.
#' @noRd
.run_git_raw <- function(git_bin, args, timeout = 15) {
  outfile <- tempfile("gitneighbr-git-")
  on.exit(unlink(outfile), add = TRUE)
  processx::run(git_bin, args, error_on_status = TRUE, timeout = timeout, stdout = outfile)
  size <- file.info(outfile)$size
  if (is.na(size) || size == 0L) {
    return(raw())
  }
  readBin(outfile, "raw", n = size)
}

#' Split a raw byte vector on NUL (0x00) into strings
#'
#' The standard workaround for R's ban on embedded NULs in character
#' strings: cut the raw vector into segments *between* NUL bytes first, then
#' convert each segment (which by construction contains no NUL) with
#' `rawToChar()`. A trailing empty segment after a final terminating NUL is
#' dropped, since Git's `-z` output always NUL-terminates (not just
#' separates) records.
#' @noRd
.split_nul <- function(bytes) {
  if (length(bytes) == 0L) {
    return(character())
  }
  nul_idx <- which(bytes == as.raw(0L))
  starts <- c(1L, nul_idx + 1L)
  ends <- c(nul_idx - 1L, length(bytes))
  if (length(starts) > 0L && starts[[length(starts)]] > length(bytes)) {
    starts <- starts[-length(starts)]
    ends <- ends[-length(ends)]
  }
  vapply(seq_along(starts), function(i) {
    if (starts[[i]] > ends[[i]]) "" else rawToChar(bytes[starts[[i]]:ends[[i]]])
  }, character(1))
}

#' Parse `git status --porcelain=v2 -z --branch` into per-file entries
#'
#' Unlike `.parse_git_status_v2()` (used for the summary counts on
#' `/api/v1/status`), this keeps one record per changed path, including
#' rename pairs, with full path fidelity via NUL-delimited parsing.
#'
#' @return A list of entries, each a list with `kind` (`"ordinary"`,
#'   `"rename"`, `"untracked"`, or `"conflicted"`), `path`, `old_path`
#'   (`NULL` unless `kind == "rename"`), `deleted_flag`, `new_flag`, and
#'   `staged` (whether the index entry (`X` column) already differs from
#'   `HEAD`; always `FALSE` for `"untracked"` and `"conflicted"` kinds,
#'   which have no single well-defined staged/unstaged split).
#' @noRd
.git_status_entries <- function(repo_root, git_bin) {
  raw <- .run_git_raw(git_bin, c("-C", repo_root, "status", "--porcelain=v2", "-z", "--branch"))
  fields <- .split_nul(raw)

  entries <- list()
  i <- 1L
  n <- length(fields)
  while (i <= n) {
    field <- fields[[i]]
    kind <- if (nzchar(field)) substr(field, 1, 1) else ""

    if (kind == "1") {
      m <- regmatches(field, regexec("^1 (\\S+) (\\S+) (\\S+) (\\S+) (\\S+) (\\S+) (\\S+) (.*)$", field))[[1]]
      xy <- m[[2]]
      path <- m[[9]]
      x <- substr(xy, 1, 1)
      y <- substr(xy, 2, 2)
      entries[[length(entries) + 1L]] <- list(
        kind = "ordinary", path = path, old_path = NULL,
        deleted_flag = (x == "D" || y == "D"),
        new_flag = (x == "A" || y == "A"),
        staged = (x != ".")
      )
      i <- i + 1L
    } else if (kind == "2") {
      m <- regmatches(field, regexec("^2 (\\S+) (\\S+) (\\S+) (\\S+) (\\S+) (\\S+) (\\S+) (\\S+) (.*)$", field))[[1]]
      xy <- m[[2]]
      path <- m[[10]]
      old_path <- fields[[i + 1L]]
      entries[[length(entries) + 1L]] <- list(
        kind = "rename", path = path, old_path = old_path,
        deleted_flag = FALSE, new_flag = FALSE,
        staged = (substr(xy, 1, 1) != ".")
      )
      i <- i + 2L
    } else if (kind == "u") {
      m <- regmatches(field, regexec("^u (\\S+) (\\S+) (\\S+) (\\S+) (\\S+) (\\S+) (\\S+) (\\S+) (\\S+) (.*)$", field))[[1]]
      path <- m[[11]]
      entries[[length(entries) + 1L]] <- list(
        kind = "conflicted", path = path, old_path = NULL,
        deleted_flag = FALSE, new_flag = FALSE, staged = FALSE
      )
      i <- i + 1L
    } else if (kind == "?") {
      entries[[length(entries) + 1L]] <- list(
        kind = "untracked", path = substr(field, 3, nchar(field)), old_path = NULL,
        deleted_flag = FALSE, new_flag = TRUE, staged = FALSE
      )
      i <- i + 1L
    } else {
      # Branch headers ("# ...") and ignored entries ("!") are not part of
      # the changed-file list.
      i <- i + 1L
    }
  }
  entries
}

#' Parse NUL-delimited `git diff --numstat -z` fields into entries
#'
#' A plain (non-rename) record is one field shaped `<added>\t<deleted>\t<path>`.
#' A rename record has an *empty* trailing path in that first field, followed
#' by two more NUL-delimited fields: the old path, then the new path. Binary
#' files report `-` for both counts.
#' @noRd
.parse_numstat_entries <- function(fields) {
  entries <- list()
  i <- 1L
  n <- length(fields)
  while (i <= n) {
    field <- fields[[i]]
    if (!nzchar(field)) {
      i <- i + 1L
      next
    }
    m <- regmatches(field, regexec("^(-|[0-9]+)\t(-|[0-9]+)\t(.*)$", field))[[1]]
    if (length(m) == 0L) {
      i <- i + 1L
      next
    }
    binary <- identical(m[[2]], "-")
    added <- if (binary) NA_integer_ else as.integer(m[[2]])
    deleted <- if (binary) NA_integer_ else as.integer(m[[3]])
    trailing <- m[[4]]

    if (nzchar(trailing)) {
      entries[[length(entries) + 1L]] <- list(path = trailing, added = added, deleted = deleted, binary = binary)
      i <- i + 1L
    } else {
      new_path <- fields[[i + 2L]]
      entries[[length(entries) + 1L]] <- list(path = new_path, added = added, deleted = deleted, binary = binary)
      i <- i + 3L
    }
  }
  entries
}

#' `git diff [--cached] --numstat -z -M`, parsed
#' @noRd
.git_numstat <- function(repo_root, git_bin, cached) {
  args <- c("-C", repo_root, "diff", "--numstat", "-z", "-M")
  if (cached) {
    args <- c(args, "--cached")
  }
  raw <- .run_git_raw(git_bin, args)
  .parse_numstat_entries(.split_nul(raw))
}

#' Fold numstat entries into a path-keyed lookup, summing duplicate paths
#' @noRd
.numstat_lookup <- function(entries) {
  map <- list()
  for (e in entries) {
    key <- e$path
    if (is.null(map[[key]])) {
      map[[key]] <- list(added = 0L, deleted = 0L, binary = FALSE)
    }
    if (isTRUE(e$binary)) {
      map[[key]]$binary <- TRUE
    } else {
      map[[key]]$added <- map[[key]]$added + e$added
      map[[key]]$deleted <- map[[key]]$deleted + e$deleted
    }
  }
  map
}

#' Sum stats for a path (and, for renames, its old path) out of a numstat lookup
#' @noRd
.lookup_stats <- function(map, path, old_path) {
  keys <- c(path, old_path)
  keys <- keys[!vapply(keys, is.null, logical(1))]
  added <- 0L
  deleted <- 0L
  binary <- FALSE
  found <- FALSE
  for (key in keys) {
    entry <- map[[key]]
    if (!is.null(entry)) {
      found <- TRUE
      if (entry$binary) {
        binary <- TRUE
      } else {
        added <- added + entry$added
        deleted <- deleted + entry$deleted
      }
    }
  }
  list(added = added, deleted = deleted, binary = binary, found = found)
}

#' Inspect an untracked file directly (no Git object exists yet to diff)
#'
#' Binary detection mirrors Git's own heuristic: a NUL byte anywhere in a
#' leading sample of the content. Line count is only computed for files at
#' or under `max_bytes`, to avoid reading arbitrarily large new files into
#' memory just to describe them in a list row.
#' @noRd
.untracked_file_stats <- function(repo_root, path, max_bytes) {
  full <- fs::path(repo_root, path)
  size <- tryCatch(as.numeric(fs::file_size(full)), error = function(e) NA_real_)
  if (is.na(size)) {
    return(list(added = NA_integer_, binary = FALSE, large = FALSE))
  }

  con <- file(full, open = "rb")
  on.exit(close(con))
  sample <- readBin(con, "raw", n = min(size, 8000))
  binary <- any(sample == as.raw(0L))
  large <- size > max_bytes

  if (binary || large) {
    return(list(added = NA_integer_, binary = binary, large = large))
  }

  seek(con, where = 0)
  content <- readBin(con, "raw", n = size)
  newline_count <- sum(content == as.raw(0x0A))
  has_trailing_partial_line <- length(content) > 0L && content[[length(content)]] != as.raw(0x0A)
  list(added = newline_count + as.integer(has_trailing_partial_line), binary = FALSE, large = FALSE)
}

#' Build the changed-file list backing `GET /api/v1/changes`
#'
#' Combines the porcelain v2 status entries (source of truth for which
#' paths changed, their rename pairs, and New/Changed/Renamed/Deleted
#' classification) with separately-queried staged and unstaged numstat line
#' counts, since Git's own rename-vs-numstat heuristics do not always agree
#' on whether a renamed-and-edited file is one rename record or a
#' delete/add pair — summing both possible keys handles either shape.
#'
#' @return A list of entries: `path`, `old_path` (`NULL` unless renamed),
#'   `state` (`"NEW"`, `"CHANGED"`, `"RENAMED"`, `"DELETED"`, or
#'   `"CONFLICTED"`), `untracked`, `added`, `deleted`, `binary`, `large`.
#' @noRd
.git_changes <- function(repo_root, git_bin, max_bytes = getOption("gitneighbr.diff_max_bytes", 200000L)) {
  status_entries <- .git_status_entries(repo_root, git_bin)
  staged_map <- .numstat_lookup(.git_numstat(repo_root, git_bin, cached = TRUE))
  unstaged_map <- .numstat_lookup(.git_numstat(repo_root, git_bin, cached = FALSE))

  lapply(status_entries, function(e) {
    if (e$kind == "untracked") {
      stats <- .untracked_file_stats(repo_root, e$path, max_bytes)
      return(list(
        path = e$path, old_path = NULL, state = "NEW", untracked = TRUE,
        added = stats$added, deleted = 0L, binary = stats$binary, large = stats$large
      ))
    }
    if (e$kind == "conflicted") {
      return(list(
        path = e$path, old_path = NULL, state = "CONFLICTED", untracked = FALSE,
        added = NA_integer_, deleted = NA_integer_, binary = FALSE, large = FALSE
      ))
    }

    staged <- .lookup_stats(staged_map, e$path, e$old_path)
    unstaged <- .lookup_stats(unstaged_map, e$path, e$old_path)
    binary <- staged$binary || unstaged$binary
    added <- if (binary) NA_integer_ else staged$added + unstaged$added
    deleted <- if (binary) NA_integer_ else staged$deleted + unstaged$deleted

    state <- if (e$kind == "rename") {
      "RENAMED"
    } else if (e$deleted_flag) {
      "DELETED"
    } else if (e$new_flag) {
      "NEW"
    } else {
      "CHANGED"
    }

    size <- tryCatch(as.numeric(fs::file_size(fs::path(repo_root, e$path))), error = function(err) NA_real_)
    large <- !binary && !is.na(size) && size > max_bytes

    list(
      path = e$path, old_path = e$old_path, state = state, untracked = FALSE,
      added = added, deleted = deleted, binary = binary, large = large
    )
  })
}

#' Validate that a repository-relative path stays inside the repository
#'
#' Purely lexical (no filesystem access), so it works for paths that no
#' longer exist on disk (a deleted file's diff, say). Rejects absolute
#' paths and any path that normalizes to somewhere outside `repo_root`.
#' @noRd
.validate_repo_relative_path <- function(repo_root, path) {
  if (is.null(path) || length(path) != 1L || !nzchar(path) || fs::is_absolute_path(path)) {
    return(FALSE)
  }
  normalized <- as.character(fs::path_norm(fs::path(repo_root, path)))
  root <- as.character(fs::path_norm(repo_root))
  startsWith(normalized, paste0(root, "/"))
}

#' Unified diff text comparing an untracked file against nothing
#' @noRd
.diff_untracked_text <- function(repo_root, git_bin, path) {
  # Deliberately the literal string "/dev/null", not base::nullfile(): Git's
  # diff machinery pattern-matches this exact path (on every OS, including
  # Windows builds of Git) to render a "diff against nothing" header without
  # ever opening it. base::nullfile() returns "NUL" on Windows, which Git
  # does not special-case the same way, producing a malformed diff.
  result <- processx::run(
    git_bin,
    c("-C", repo_root, "diff", "--no-index", "--", "/dev/null", path),
    error_on_status = FALSE, timeout = 15
  )
  result$stdout
}

#' Unified diff text for one tracked (possibly renamed) path
#' @noRd
.diff_tracked_text <- function(repo_root, git_bin, path, old_path, unborn) {
  base_ref <- if (isTRUE(unborn)) .git_empty_tree_sha else "HEAD"
  pathspecs <- unique(c(old_path, path))
  result <- processx::run(
    git_bin,
    c("-C", repo_root, "diff", base_ref, "-M", "--", pathspecs),
    error_on_status = FALSE, timeout = 15
  )
  result$stdout
}

#' Build the response for `GET /api/v1/diff?path=`
#'
#' Reuses `.git_changes()` to classify the requested path (binary? renamed?
#' untracked?) rather than duplicating that logic, then fetches unified diff
#' text only for non-binary files, byte- and line-limited with an
#' `offset_lines`/`truncated` pair that lets the frontend request further
#' chunks of a large diff.
#'
#' @return A list with `found` (`FALSE` if `path` has no pending changes),
#'   and when found: `path`, `old_path`, `state`, `binary`, `lines`,
#'   `offset_lines`, `total_lines`, `truncated`.
#' @noRd
.git_diff <- function(repo_root, git_bin, path, offset_lines = 0L,
                       max_bytes = getOption("gitneighbr.diff_max_bytes", 200000L)) {
  changes <- .git_changes(repo_root, git_bin, max_bytes = max_bytes)
  entry <- Find(function(c) identical(c$path, path), changes)
  if (is.null(entry)) {
    return(list(found = FALSE))
  }
  if (isTRUE(entry$binary)) {
    return(list(
      found = TRUE, path = entry$path, old_path = entry$old_path, state = entry$state,
      binary = TRUE, lines = list(), offset_lines = 0L, total_lines = 0L, truncated = FALSE
    ))
  }

  text <- if (isTRUE(entry$untracked)) {
    .diff_untracked_text(repo_root, git_bin, path)
  } else {
    status <- .git_status(repo_root, git_bin)
    .diff_tracked_text(repo_root, git_bin, entry$path, entry$old_path, unborn = status$unborn)
  }

  all_lines <- strsplit(text, "\n", fixed = TRUE)[[1]]
  total_lines <- length(all_lines)
  start <- min(offset_lines + 1L, total_lines + 1L)

  chunk <- character()
  bytes_used <- 0L
  end <- start - 1L
  if (start <= total_lines) {
    for (idx in start:total_lines) {
      line_bytes <- nchar(all_lines[[idx]], type = "bytes") + 1L
      if (length(chunk) > 0L && (bytes_used + line_bytes) > max_bytes) {
        break
      }
      chunk <- c(chunk, all_lines[[idx]])
      bytes_used <- bytes_used + line_bytes
      end <- idx
    }
  }

  list(
    found = TRUE, path = entry$path, old_path = entry$old_path, state = entry$state,
    binary = FALSE, lines = as.list(chunk),
    offset_lines = if (total_lines == 0L) 0L else start - 1L,
    total_lines = total_lines, truncated = end < total_lines
  )
}
