#' Derive the primary repository state
#'
#' Implements the state machine in spec Sec 7.1, minus `UNSUPPORTED_REPOSITORY`
#' (no detection rule for it exists yet) and `BARE_REPOSITORY` (already
#' surfaced separately by [doctor()]).
#'
#' Precedence, most urgent first: a repository that cannot be inspected at
#' all; one with human-judgment-required conflicts already present (spec
#' Sec 13.3 forbids ever auto-resolving these) - checked ahead of detached
#' `HEAD` because a conflict stopped mid-rebase or mid-cherry-pick leaves
#' `HEAD` detached too, and the conflict is the actionable fact; one whose
#' `HEAD` cannot be described as a branch for any other reason; one where
#' the last remote operation was rejected for authentication, which makes
#' any ahead/behind count unreliable until it is retried; then the
#' ordinary upstream/ahead/behind combinations.
#'
#' @param status A list as returned by `.git_status()`, or `NULL` if the
#'   path is not inside a Git working tree.
#' @param git_ok Whether a usable Git binary was found at all.
#' @param auth_required Whether the most recent fetch or push against this
#'   repository's upstream was rejected for authentication and has not
#'   since succeeded (tracked per-session; see `.build_api()`'s
#'   `session_state$auth_required`).
#' @return A single string, one of `GIT_UNAVAILABLE`, `NOT_REPOSITORY`,
#'   `DETACHED_HEAD`, `CONFLICTED`, `AUTH_REQUIRED`, `NO_UPSTREAM`, `READY`,
#'   `CHANGES_ONLY`, `LOCAL_ONLY`, `CHANGES_AND_LOCAL`, `REMOTE_ONLY_CLEAN`,
#'   `REMOTE_ONLY_DIRTY`, or `DIVERGED`.
#' @noRd
.primary_state <- function(status, git_ok, auth_required = FALSE) {
  if (!git_ok) {
    return("GIT_UNAVAILABLE")
  }
  if (is.null(status)) {
    return("NOT_REPOSITORY")
  }
  if ((status$conflicted_count %||% 0L) > 0L) {
    return("CONFLICTED")
  }
  if (status$detached) {
    return("DETACHED_HEAD")
  }
  if (isTRUE(auth_required)) {
    return("AUTH_REQUIRED")
  }
  if (is.null(status$upstream)) {
    return("NO_UPSTREAM")
  }

  ahead <- status$ahead
  behind <- status$behind
  has_changes <- status$has_changes

  if (ahead > 0L && behind > 0L) {
    return("DIVERGED")
  }
  if (ahead > 0L) {
    return(if (has_changes) "CHANGES_AND_LOCAL" else "LOCAL_ONLY")
  }
  if (behind > 0L) {
    return(if (has_changes) "REMOTE_ONLY_DIRTY" else "REMOTE_ONLY_CLEAN")
  }
  if (has_changes) "CHANGES_ONLY" else "READY"
}

#' Is a merge, rebase, cherry-pick, or revert currently in progress?
#'
#' Checks for the marker files/directories Git itself leaves behind while
#' one of these operations is stopped on a conflict, via
#' `git rev-parse --git-path` so the check works under worktrees and
#' submodules without hardcoding a `.git/` layout. Purely a filesystem
#' read - no state-changing Git call is made.
#'
#' @return One of `"merge"`, `"rebase"`, `"cherry-pick"`, `"revert"`, or
#'   `NULL` if none is in progress.
#' @noRd
.git_in_progress_operation <- function(repo_root, git_bin) {
  git_path <- function(name) {
    result <- tryCatch(
      processx::run(
        git_bin, c("-C", repo_root, "rev-parse", "--git-path", name),
        error_on_status = FALSE, timeout = 5
      ),
      error = function(e) NULL
    )
    if (is.null(result) || !identical(result$status, 0L)) {
      return(NULL)
    }
    # `--git-path` prints its answer relative to `repo_root`, not to this
    # process's own working directory, even when queried via `-C`.
    trimws(result$stdout)
  }
  exists_at <- function(name) {
    relative <- git_path(name)
    if (is.null(relative)) {
      return(FALSE)
    }
    path <- if (fs::is_absolute_path(relative)) relative else fs::path(repo_root, relative)
    fs::file_exists(path) || fs::dir_exists(path)
  }

  if (exists_at("MERGE_HEAD")) {
    return("merge")
  }
  if (exists_at("rebase-merge") || exists_at("rebase-apply")) {
    return("rebase")
  }
  if (exists_at("CHERRY_PICK_HEAD")) {
    return("cherry-pick")
  }
  if (exists_at("REVERT_HEAD")) {
    return("revert")
  }
  NULL
}

#' Does a repository have any ignored (not just untracked) paths?
#' @noRd
.has_ignored_files <- function(repo_root, git_bin) {
  result <- tryCatch(
    processx::run(
      git_bin, c("-C", repo_root, "status", "--porcelain=v2", "--ignored=matching"),
      error_on_status = FALSE, timeout = 15
    ),
    error = function(e) NULL
  )
  if (is.null(result) || !identical(result$status, 0L)) {
    return(FALSE)
  }
  lines <- strsplit(result$stdout, "\n", fixed = TRUE)[[1]]
  any(startsWith(lines, "!"))
}

#' Is commit signing turned on for this repository?
#' @noRd
.commit_signing_enabled <- function(repo_root, git_bin) {
  result <- tryCatch(
    processx::run(
      git_bin, c("-C", repo_root, "config", "--get", "commit.gpgsign"),
      error_on_status = FALSE, timeout = 5
    ),
    error = function(e) NULL
  )
  !is.null(result) && identical(result$status, 0L) && identical(trimws(result$stdout), "true")
}

#' Is Git LFS active in this repository?
#'
#' Checks for an LFS filter driver configured (the state left behind by
#' `git lfs install`) or an LFS pattern recorded in a committed
#' `.gitattributes`, without shelling out to the (optional, separately
#' installed) `git-lfs` binary itself.
#' @noRd
.git_lfs_active <- function(repo_root, git_bin) {
  attrs_path <- fs::path(repo_root, ".gitattributes")
  if (fs::file_exists(attrs_path)) {
    lines <- tryCatch(readLines(attrs_path, warn = FALSE), error = function(e) character())
    if (any(grepl("filter=lfs", lines, fixed = TRUE))) {
      return(TRUE)
    }
  }
  result <- tryCatch(
    processx::run(
      git_bin, c("-C", repo_root, "config", "--get", "filter.lfs.clean"),
      error_on_status = FALSE, timeout = 5
    ),
    error = function(e) NULL
  )
  !is.null(result) && identical(result$status, 0L) && nzchar(trimws(result$stdout))
}

#' Number of unsaved-changes notices: how many whole days old is `HEAD`?
#'
#' Used only to decide whether to nudge about stale unsaved changes
#' (spec/issue #30) - never for anything that gates a mutating action.
#' @return An integer day count, or `NULL` if it can't be determined
#'   (e.g. an unborn repository with no commits yet).
#' @noRd
.days_since_last_commit <- function(repo_root, git_bin) {
  result <- tryCatch(
    processx::run(
      git_bin, c("-C", repo_root, "log", "-1", "--format=%ct"),
      error_on_status = FALSE, timeout = 5
    ),
    error = function(e) NULL
  )
  if (is.null(result) || !identical(result$status, 0L) || !nzchar(trimws(result$stdout))) {
    return(NULL)
  }
  committed_at <- suppressWarnings(as.numeric(trimws(result$stdout)))
  if (is.na(committed_at)) {
    return(NULL)
  }
  as.integer(floor(as.numeric(Sys.time()) - committed_at) %/% 86400L)
}

#' Threshold (in whole days) before unsaved changes are called "stale"
#' @noRd
.stale_changes_days <- 14L

#' Local tag names (if any) pointing at the current `HEAD`
#' @noRd
.git_tags_at_head <- function(repo_root, git_bin) {
  result <- tryCatch(
    processx::run(
      git_bin, c("-C", repo_root, "tag", "--points-at", "HEAD"),
      error_on_status = FALSE, timeout = 5
    ),
    error = function(e) NULL
  )
  if (is.null(result) || !identical(result$status, 0L)) {
    return(character())
  }
  tags <- strsplit(trimws(result$stdout), "\n", fixed = TRUE)[[1]]
  tags[nzchar(tags)]
}

#' Build the supporting-notices list for the current status (spec Sec 7.2)
#'
#' Each notice is a list with `code` (a stable identifier) and `message` (a
#' user-facing sentence). Two notices - "a tag exists locally but not
#' remotely" and "a pushed tag exists for the current commit" - are tracked
#' from `session_state$pending_tags`/`pushed_tags` (populated by the
#' `/api/v1/tag` and `/api/v1/push-tag` handlers) rather than a live
#' `git ls-remote`: comparing against the actual remote would mean an
#' unsolicited network call on every 2-second status poll, which risks
#' hanging the poll loop or prompting for credentials outside an explicit
#' user action. The tradeoff is that a tag created or pushed before this
#' session started, or from outside gitneighbr entirely, is not reflected.
#'
#' @param show_ignored Whether to check for ignored files at all; skipped by
#'   default since walking ignore rules can be slow in a large repository
#'   and the interface only needs the answer when the user has opted to see
#'   ignored files.
#' @return A list of `list(code, message)` notices.
#' @noRd
.status_notices <- function(repo_root, git_bin, status, session_state, show_ignored = FALSE) {
  notices <- list()
  add <- function(code, message) {
    notices[[length(notices) + 1L]] <<- list(code = code, message = message)
  }

  if (!isTRUE(.git_identity(repo_root, git_bin)$complete)) {
    add("IDENTITY_INCOMPLETE", "Git needs your name and email before you can save a snapshot.")
  }

  if ((status$untracked_count %||% 0L) > 0L) {
    add("UNTRACKED_PRESENT", "There are untracked files in this repository.")
  }
  if (isTRUE(status$has_changes) && !isTRUE(status$unborn)) {
    days_old <- .days_since_last_commit(repo_root, git_bin)
    if (!is.null(days_old) && days_old >= .stale_changes_days) {
      add("STALE_CHANGES", "It's been a while since your last saved snapshot.")
    }
  }
  if (isTRUE(show_ignored) && .has_ignored_files(repo_root, git_bin)) {
    add("IGNORED_PRESENT", "There are ignored files in this repository.")
  }

  hooks <- .active_hooks(repo_root, git_bin)
  if (any(hooks %in% c("pre-commit", "commit-msg", "post-commit"))) {
    add("COMMIT_HOOK_ACTIVE", "A commit hook will run when you save a snapshot.")
  }
  if (.commit_signing_enabled(repo_root, git_bin)) {
    add("SIGNING_ENABLED", "Commit signing is enabled for this repository.")
  }
  if (.git_lfs_active(repo_root, git_bin)) {
    add("LFS_ACTIVE", "Git LFS is active in this repository.")
  }
  if (fs::file_exists(fs::path(repo_root, ".gitmodules"))) {
    add("SUBMODULE_PRESENT", "This repository contains a submodule.")
  }

  remote_url <- .git_remote_url(repo_root, git_bin)
  if (!is.null(remote_url) && !.is_github_remote(remote_url)) {
    add("REMOTE_NOT_GITHUB", "The current remote is not hosted on GitHub.")
  }

  pushed_tags <- session_state$pushed_tags %||% character()
  if (length(pushed_tags) > 0L && length(intersect(.git_tags_at_head(repo_root, git_bin), pushed_tags)) > 0L) {
    add("PUSHED_TAG_AT_HEAD", "A pushed tag exists for the current commit.")
  }
  pending_tags <- session_state$pending_tags %||% character()
  if (length(pending_tags) > 0L) {
    add("LOCAL_ONLY_TAG", "A tag exists locally but has not been sent to GitHub yet.")
  }

  policy <- .read_repo_policy(repo_root)
  if (isTRUE(policy$has_policy_file) && !isTRUE(policy$valid)) {
    add("POLICY_INVALID", policy$error %||% "Repository policy configuration contains errors.")
  } else if (isTRUE(policy$require_pull_request) || .is_branch_protected(status$branch, policy)) {
    add("POLICY_PR_REQUIRED", "Changes to this branch must be sent via Pull Request.")
  }

  notices
}

#' Compute the full `/api/v1/status` data payload and bump `status_version`
#'
#' Session-scoped optimistic-concurrency contract (spec Sec 7.3): compares
#' the freshly computed status data against the last snapshot this server
#' process returned, and only advances `session_state$version` when
#' something actually changed. Both the `GET /api/v1/status` handler and
#' every mutating handler in `.build_api()` call this - the former to
#' answer polls, the latter both to gate stale mutations against the
#' version the client last observed and to report an up-to-date version on
#' its own response.
#'
#' @param session_state An environment private to one running server,
#'   holding `version`, `last_snapshot`, `auth_required`, `pending_tags`,
#'   and `pushed_tags` (see `.build_api()`).
#' @return A list with `data` (the status payload) and `version` (the
#'   current `status_version`, already bumped if `data` changed since the
#'   last call).
#' @noRd
.status_payload <- function(repo_root, git_bin, session_state, show_ignored = FALSE) {
  git_ok <- .git_available(git_bin)
  # `repo_root` may not be a Git repository yet (onboarding: init/clone
  # haven't run). `.git_status()` uses `error_on_status = TRUE` and would
  # throw for a non-repo path, so repo-ness is checked first; `NULL` here
  # is exactly what `.primary_state()` reads as `NOT_REPOSITORY`.
  status <- if (git_ok && identical(.git_repo_kind(repo_root, git_bin), "worktree")) {
    .git_status(repo_root, git_bin)
  } else {
    NULL
  }
  notices <- if (git_ok && !is.null(status)) {
    .status_notices(repo_root, git_bin, status, session_state, show_ignored = show_ignored)
  } else {
    list()
  }
  state <- .primary_state(status, git_ok, auth_required = isTRUE(session_state$auth_required))
  in_progress_operation <- if (git_ok && !is.null(status)) {
    .git_in_progress_operation(repo_root, git_bin)
  } else {
    NULL
  }

  policy <- .read_repo_policy(repo_root)
  github_info <- .github_api_status(repo_root, git_bin, session_state = session_state, policy = policy)
  is_protected <- .is_branch_protected(status$branch, policy = policy, github_protected = github_info$branch_protected)

  capabilities <- list(
    can_commit = (status$unstaged_count %||% 0L) > 0L || (status$staged_count %||% 0L) > 0L || (status$untracked_count %||% 0L) > 0L,
    can_push = (status$ahead %||% 0L) > 0L && !is_protected && !isTRUE(policy$require_pull_request),
    can_update = (status$behind %||% 0L) > 0L && (status$unstaged_count %||% 0L) == 0L && (status$untracked_count %||% 0L) == 0L,
    can_tag = !isTRUE(status$detached) && !isTRUE(status$unborn),
    can_pull_request = isTRUE(github_info$is_github) && (status$ahead %||% 0L) > 0L,
    can_release = isTRUE(github_info$is_github),
    requires_pull_request = is_protected || isTRUE(policy$require_pull_request),
    disallow_untracked_trash = isTRUE(policy$disallow_untracked_trash)
  )

  data <- list(
    repository = list(root_display = fs::path_file(repo_root)),
    primary_state = state,
    upstream = status$upstream,
    branch = status$branch,
    ahead = status$ahead %||% 0L,
    behind = status$behind %||% 0L,
    staged_count = status$staged_count %||% 0L,
    unstaged_count = status$unstaged_count %||% 0L,
    untracked_count = status$untracked_count %||% 0L,
    conflicted_count = status$conflicted_count %||% 0L,
    in_progress_operation = in_progress_operation,
    recovery = .recovery_commands(state, in_progress_operation, status$upstream),
    notices = notices,
    policy = policy,
    github = github_info,
    capabilities = capabilities
  )

  if (!identical(data, session_state$last_snapshot)) {
    session_state$version <- session_state$version + 1L
    session_state$last_snapshot <- data
  }

  list(data = data, version = session_state$version)
}
