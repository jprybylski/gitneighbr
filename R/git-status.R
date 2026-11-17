#' Confirm a Git binary is present and usable
#' @noRd
.git_available <- function(git_bin) {
  if (is.null(git_bin) || !nzchar(git_bin)) {
    return(FALSE)
  }
  result <- tryCatch(
    .git_exec(git_bin, c("--version"), error_on_status = FALSE, timeout = 5),
    error = function(e) NULL
  )
  !is.null(result) && identical(result$status, 0L)
}

#' Resolve the working-tree root for a path, if it is inside a Git repository
#'
#' @return The canonical repo root as a string, or `NULL` if `path` is not
#'   inside a (non-bare) Git working tree.
#' @noRd
.git_root <- function(path, git_bin) {
  result <- tryCatch(
    .git_exec(git_bin, c("-C", path, "rev-parse", "--show-toplevel"),
      error_on_status = FALSE,
      timeout = 5
    ),
    error = function(e) NULL
  )
  if (is.null(result) || !identical(result$status, 0L)) {
    return(NULL)
  }
  fs::path_real(trimws(result$stdout))
}

#' Run `git status --porcelain=v2 -z --branch` once, as NUL-split fields
#'
#' Uses an argv array (never a shell string). The NUL-delimited form keeps
#' every path byte-exact; it is read through a file (`.run_git_raw()`) since
#' R strings can't hold NUL. One run feeds both the summary counts
#' (`.parse_git_status_fields()`) and the per-file list
#' (`.status_entries_from_fields()`), so a poll needs a single Git process.
#' @noRd
.git_status_fields <- function(repo_root, git_bin) {
  .split_nul(.run_git_raw(
    git_bin,
    c("-C", repo_root, "status", "--porcelain=v2", "-z", "--branch", "--untracked-files=all")
  ))
}

#' Run `git status` and summarise it
#'
#' @return A list with `branch`, `upstream`, `ahead`, `behind`,
#'   `staged_count`, `unstaged_count`, `untracked_count`, `conflicted_count`,
#'   `has_changes`, `detached`, `unborn` and `oid` (the `HEAD` commit, `""`
#'   when unborn).
#' @noRd
.git_status <- function(repo_root, git_bin) {
  .parse_git_status_fields(.git_status_fields(repo_root, git_bin))
}

#' Summarise `git status --porcelain=v2 -z --branch` fields
#'
#' Same counts as `.parse_git_status_v2()`; a rename record's second field
#' (the old path) is skipped rather than read as a record of its own.
#' @noRd
.parse_git_status_fields <- function(fields) {
  # Records keep only their kind and XY columns (all the summary reads), so
  # a path containing a newline can't split one into two lines.
  lines <- character()
  i <- 1L
  n <- length(fields)
  while (i <= n) {
    field <- fields[[i]]
    if (startsWith(field, "# ")) {
      lines <- c(lines, gsub("\n", " ", field, fixed = TRUE))
    } else if (nzchar(field)) {
      lines <- c(lines, substr(field, 1L, 4L))
    }
    i <- i + if (startsWith(field, "2 ")) 2L else 1L
  }
  headers <- lines[startsWith(lines, "# ")]
  status <- .parse_git_status_v2(paste(lines, collapse = "\n"))
  oid <- sub("^# branch\\.oid ", "", headers[startsWith(headers, "# branch.oid ")])
  status$oid <- if (length(oid) == 1L && !identical(oid, "(initial)")) oid else ""
  status
}

#' @noRd
.parse_git_status_v2 <- function(stdout) {
  tokens <- strsplit(stdout, "\n", fixed = TRUE)[[1]]
  tokens <- tokens[nzchar(tokens)]

  branch <- NULL
  upstream <- NULL
  ahead <- 0L
  behind <- 0L
  unborn <- FALSE
  detached <- FALSE
  staged_count <- 0L
  unstaged_count <- 0L
  untracked_count <- 0L
  conflicted_count <- 0L

  i <- 1L
  n <- length(tokens)
  while (i <= n) {
    token <- tokens[[i]]
    kind <- substr(token, 1, 1)

    if (kind == "#") {
      if (startsWith(token, "# branch.head ")) {
        branch <- sub("^# branch\\.head ", "", token)
        detached <- identical(branch, "(detached)")
      } else if (startsWith(token, "# branch.oid ")) {
        oid <- sub("^# branch\\.oid ", "", token)
        unborn <- identical(oid, "(initial)")
      } else if (startsWith(token, "# branch.upstream ")) {
        upstream <- sub("^# branch\\.upstream ", "", token)
      } else if (startsWith(token, "# branch.ab ")) {
        ab <- sub("^# branch\\.ab ", "", token)
        parts <- strsplit(ab, " ")[[1]]
        ahead <- as.integer(sub("^\\+", "", parts[[1]]))
        behind <- as.integer(sub("^-", "", parts[[2]]))
      }
    } else if (kind %in% c("1", "2")) {
      xy <- substr(token, 3, 4)
      x <- substr(xy, 1, 1)
      y <- substr(xy, 2, 2)
      if (x != ".") staged_count <- staged_count + 1L
      if (y != ".") unstaged_count <- unstaged_count + 1L
      # Rename/copy ("2") entries carry both paths on this same line,
      # tab-separated; no extra line to consume.
    } else if (kind == "u") {
      # Unmerged entries share the "1"/"2" line shape but their XY column
      # encodes each side's conflict state (e.g. "UU"), not a staged vs.
      # unstaged split, so they are counted separately rather than folded
      # into staged/unstaged counts.
      conflicted_count <- conflicted_count + 1L
    } else if (kind == "?") {
      untracked_count <- untracked_count + 1L
    }
    # kind == "!" (ignored) is intentionally not counted.

    i <- i + 1L
  }

  list(
    branch = branch,
    upstream = upstream,
    ahead = ahead,
    behind = behind,
    unborn = unborn,
    detached = detached,
    staged_count = staged_count,
    unstaged_count = unstaged_count,
    untracked_count = untracked_count,
    conflicted_count = conflicted_count,
    has_changes = (staged_count + unstaged_count + untracked_count + conflicted_count) > 0L
  )
}
