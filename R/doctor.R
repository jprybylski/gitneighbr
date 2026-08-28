#' Minimum Git version gitneighbr supports (spec §10.2)
#' @noRd
.min_git_version <- "2.34.0"

#' Parse the version number out of `git --version` output
#'
#' Handles vendor suffixes such as `"git version 2.42.0 (Apple Git-140)"`.
#' @return A version string, or `NULL` if it could not be parsed.
#' @noRd
.parse_git_version <- function(version_output) {
  m <- regmatches(version_output, regexpr("[0-9]+\\.[0-9]+(\\.[0-9]+)?", version_output))
  if (length(m) == 0L || !nzchar(m)) {
    return(NULL)
  }
  m
}

#' Resolve `git --version`, parsed, without erroring
#' @return A version string, or `NULL` if Git could not be run or the
#'   output could not be parsed.
#' @noRd
.git_version <- function(git_bin) {
  result <- tryCatch(
    processx::run(git_bin, "--version", error_on_status = FALSE, timeout = 5),
    error = function(e) NULL
  )
  if (is.null(result) || !identical(result$status, 0L)) {
    return(NULL)
  }
  .parse_git_version(result$stdout)
}

#' Classify a path as inside a Git working tree, a bare repository, or neither
#'
#' Distinguishes `NOT_REPOSITORY` from `BARE_REPOSITORY` (spec §15), which
#' `.git_root()` alone cannot do: `rev-parse --show-toplevel` fails for both.
#' @return One of `"worktree"`, `"bare"`, `"none"`.
#' @noRd
.git_repo_kind <- function(path, git_bin) {
  in_tree <- tryCatch(
    processx::run(git_bin, c("-C", path, "rev-parse", "--is-inside-work-tree"), error_on_status = FALSE, timeout = 5),
    error = function(e) NULL
  )
  if (!is.null(in_tree) && identical(in_tree$status, 0L) && identical(trimws(in_tree$stdout), "true")) {
    return("worktree")
  }
  is_bare <- tryCatch(
    processx::run(git_bin, c("-C", path, "rev-parse", "--is-bare-repository"), error_on_status = FALSE, timeout = 5),
    error = function(e) NULL
  )
  if (!is.null(is_bare) && identical(is_bare$status, 0L) && identical(trimws(is_bare$stdout), "true")) {
    return("bare")
  }
  "none"
}

#' The URL configured for a remote, if any
#' @noRd
.git_remote_url <- function(repo_root, git_bin, remote_name = "origin") {
  result <- tryCatch(
    processx::run(git_bin, c("-C", repo_root, "remote", "get-url", remote_name), error_on_status = FALSE, timeout = 5),
    error = function(e) NULL
  )
  if (is.null(result) || !identical(result$status, 0L)) {
    return(NULL)
  }
  url <- trimws(result$stdout)
  if (!nzchar(url)) NULL else url
}

#' Does a remote URL point at github.com?
#' @noRd
.is_github_remote <- function(url) {
  !is.null(url) && grepl("^git@github\\.com:|(^|//)github\\.com[:/]", url, ignore.case = TRUE)
}

#' Effective `credential.helper` value(s) for a repository (all config scopes)
#' @noRd
.git_credential_helpers <- function(repo_root, git_bin) {
  result <- tryCatch(
    processx::run(
      git_bin,
      c("-C", repo_root, "config", "--get-all", "credential.helper"),
      error_on_status = FALSE,
      timeout = 5
    ),
    error = function(e) NULL
  )
  if (is.null(result) || !identical(result$status, 0L)) {
    return(character())
  }
  helpers <- strsplit(trimws(result$stdout), "\n", fixed = TRUE)[[1]]
  helpers[nzchar(helpers)]
}

#' The Git-resolved hooks directory for a repository (respects `core.hooksPath`)
#' @noRd
.git_hooks_dir <- function(repo_root, git_bin) {
  result <- tryCatch(
    processx::run(git_bin, c("-C", repo_root, "rev-parse", "--git-path", "hooks"), error_on_status = FALSE, timeout = 5),
    error = function(e) NULL
  )
  if (is.null(result) || !identical(result$status, 0L)) {
    return(NULL)
  }
  path <- trimws(result$stdout)
  if (!fs::is_absolute_path(path)) {
    path <- fs::path(repo_root, path)
  }
  path
}

#' Names of active (non-`.sample`, executable) hooks in a repository
#' @noRd
.active_hooks <- function(repo_root, git_bin) {
  dir <- .git_hooks_dir(repo_root, git_bin)
  if (is.null(dir) || !fs::dir_exists(dir)) {
    return(character())
  }
  files <- fs::dir_ls(dir, type = "file")
  files <- files[!grepl("\\.sample$", files)]
  files <- files[file.access(files, mode = 1L) == 0L]
  unname(fs::path_file(files))
}

#' Build one diagnostic check result
#' @noRd
.doctor_check <- function(id, status = c("ok", "fail", "advisory", "skipped"), message, detail = NULL) {
  status <- match.arg(status)
  structure(
    list(id = id, status = status, message = message, detail = detail),
    class = "gitneighbr_doctor_check"
  )
}

#' Diagnose whether gitneighbr can run against a repository
#'
#' Runs the same read-only checks [open_repo()] relies on, without starting a
#' server or mutating the repository: Git presence and version, repository
#' detection (including distinguishing a bare repository from no repository
#' at all), upstream configuration, whether the remote looks like GitHub,
#' credential-helper presence, and active commit hooks.
#'
#' @inheritParams open_repo
#' @return An invisible `gitneighbr_doctor_report`: a list with `ok` (`TRUE`
#'   unless any non-advisory check failed) and `checks`, a list of
#'   `gitneighbr_doctor_check` objects (`id`, `status` — one of `"ok"`,
#'   `"fail"`, `"advisory"`, `"skipped"` — `message`, and an optional
#'   `detail` list). Printed automatically; print again with `print()`.
#' @export
doctor <- function(path = ".", git = getOption("gitneighbr.git", unname(Sys.which("git")))) {
  git <- unname(git)
  checks <- list()

  git_found <- nzchar(git)
  checks$git_found <- if (git_found) {
    .doctor_check("git_found", "ok", paste0("Git executable found at '", git, "'."))
  } else {
    .doctor_check("git_found", "fail", "No `git` executable found. Install Git and ensure it is on PATH.")
  }

  git_runs <- git_found && .git_available(git)
  checks$git_runs <- if (!git_found) {
    .doctor_check("git_runs", "skipped", "Skipped: no Git executable to run.")
  } else if (git_runs) {
    .doctor_check("git_runs", "ok", "Git executable runs.")
  } else {
    .doctor_check("git_runs", "fail", paste0("The configured `git` executable at '", git, "' could not be run."))
  }

  version <- if (git_runs) .git_version(git) else NULL
  checks$git_version <- if (!git_runs) {
    .doctor_check("git_version", "skipped", "Skipped: Git does not run.")
  } else if (is.null(version)) {
    .doctor_check("git_version", "fail", "Could not determine the installed Git version.")
  } else if (utils::compareVersion(version, .min_git_version) >= 0) {
    .doctor_check("git_version", "ok", paste0("Git ", version, " (>= ", .min_git_version, " required)."), list(version = version))
  } else {
    .doctor_check(
      "git_version", "fail",
      paste0("Git ", version, " is too old; gitneighbr requires ", .min_git_version, " or later."),
      list(version = version)
    )
  }

  repo_kind <- if (git_runs) .git_repo_kind(path, git) else "none"
  repo_root <- if (identical(repo_kind, "worktree")) .git_root(path, git) else NULL
  checks$repository <- if (!git_runs) {
    .doctor_check("repository", "skipped", "Skipped: Git does not run.")
  } else if (identical(repo_kind, "worktree")) {
    .doctor_check("repository", "ok", paste0("Inside a Git working tree at '", repo_root, "'."), list(repo_root = repo_root))
  } else if (identical(repo_kind, "bare")) {
    .doctor_check("repository", "fail", paste0("'", path, "' is a bare repository (no working tree); gitneighbr needs a working tree."))
  } else {
    .doctor_check("repository", "fail", paste0("'", path, "' is not inside a Git working tree."))
  }

  if (!is.null(repo_root)) {
    status <- .git_status(repo_root, git)

    checks$upstream <- if (status$detached) {
      .doctor_check("upstream", "advisory", "HEAD is detached; not on a branch with an upstream.")
    } else if (is.null(status$upstream)) {
      .doctor_check("upstream", "advisory", paste0("Branch '", status$branch, "' has no upstream configured yet."))
    } else {
      .doctor_check("upstream", "ok", paste0("Branch '", status$branch, "' tracks '", status$upstream, "'."))
    }

    remote_url <- .git_remote_url(repo_root, git)
    checks$remote <- if (is.null(remote_url)) {
      .doctor_check("remote", "advisory", "No 'origin' remote is configured.")
    } else if (.is_github_remote(remote_url)) {
      .doctor_check("remote", "ok", "The 'origin' remote points to GitHub.")
    } else {
      .doctor_check("remote", "advisory", "The 'origin' remote does not look like GitHub.")
    }

    helpers <- .git_credential_helpers(repo_root, git)
    checks$credential_helper <- if (length(helpers) > 0L) {
      .doctor_check(
        "credential_helper", "ok",
        paste0("Credential helper configured: ", paste(helpers, collapse = ", "), "."),
        list(helpers = helpers)
      )
    } else {
      .doctor_check(
        "credential_helper", "advisory",
        "No `credential.helper` configured; authenticating to GitHub may prompt outside gitneighbr."
      )
    }

    hooks <- .active_hooks(repo_root, git)
    checks$hooks <- if (length(hooks) > 0L) {
      .doctor_check(
        "hooks", "advisory",
        paste0("Active hooks present: ", paste(hooks, collapse = ", "), "."),
        list(hooks = hooks)
      )
    } else {
      .doctor_check("hooks", "ok", "No active commit/push hooks.")
    }

    identity <- .git_identity(repo_root, git)
    checks$identity <- if (isTRUE(identity$complete)) {
      .doctor_check(
        "identity", "ok",
        paste0("Git identity: ", identity$name, " <", identity$email, ">."),
        list(name = identity$name, email = identity$email)
      )
    } else if (is.null(identity$name) && is.null(identity$email)) {
      .doctor_check(
        "identity", "advisory",
        "No Git identity (name/email) configured yet; gitneighbr will help you set it before your first snapshot."
      )
    } else {
      .doctor_check(
        "identity", "advisory",
        "Git identity is only partially configured; gitneighbr will help you finish it before your first snapshot."
      )
    }
  }

  report <- structure(
    list(
      ok = !any(vapply(checks, function(check) identical(check$status, "fail"), logical(1))),
      checks = checks
    ),
    class = "gitneighbr_doctor_report"
  )

  print(report)
  invisible(report)
}

#' @export
print.gitneighbr_doctor_report <- function(x, ...) {
  symbol <- c(ok = "OK", fail = "FAIL", advisory = "!!", skipped = "--")
  cat("gitneighbr doctor\n")
  for (check in x$checks) {
    cat("  [", symbol[[check$status]], "] ", check$message, "\n", sep = "")
  }
  cat(if (x$ok) "Overall: ready.\n" else "Overall: not ready; see FAIL items above.\n")
  invisible(x)
}
