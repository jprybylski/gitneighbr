#' Confirm a Git binary is present and usable
#' @noRd
.git_available <- function(git_bin) {
  if (is.null(git_bin) || !nzchar(git_bin)) {
    return(FALSE)
  }
  result <- tryCatch(
    processx::run(git_bin, c("--version"), error_on_status = FALSE, timeout = 5),
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
    processx::run(
      git_bin,
      c("-C", path, "rev-parse", "--show-toplevel"),
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

#' Run `git status --porcelain=v2 --branch` and parse it
#'
#' Uses an argv array (never a shell string) so paths and branch names can
#' never be interpreted as shell syntax. Deliberately omits `-z`: R string
#' values cannot contain embedded NUL bytes, so the NUL-delimited form
#' cannot be parsed as ordinary R character data. This means paths with
#' unusual characters get C-style quoted by Git rather than being exposed
#' as literal bytes; full raw-path fidelity is tracked as a Phase 1 issue.
#'
#' @return A list with `branch`, `upstream`, `ahead`, `behind`,
#'   `staged_count`, `unstaged_count`, `untracked_count`, `conflicted_count`,
#'   `has_changes`, `detached`, `unborn`.
#' @noRd
.git_status <- function(repo_root, git_bin) {
  result <- processx::run(
    git_bin,
    c("-C", repo_root, "status", "--porcelain=v2", "--branch"),
    error_on_status = TRUE,
    timeout = 15
  )
  .parse_git_status_v2(result$stdout)
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
