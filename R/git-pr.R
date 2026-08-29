#' Validate a branch name against `git check-ref-format`
#' @noRd
.validate_branch_name <- function(git_bin, name) {
  if (is.null(name) || length(name) != 1L || !is.character(name) || is.na(name) || !nzchar(name)) {
    return(FALSE)
  }
  result <- processx::run(
    git_bin, c("check-ref-format", paste0("refs/heads/", name)),
    error_on_status = FALSE, timeout = 5
  )
  identical(result$status, 0L)
}

#' Check whether a local branch already exists
#' @noRd
.git_branch_exists_locally <- function(repo_root, git_bin, name) {
  result <- processx::run(
    git_bin, c("-C", repo_root, "show-ref", "--verify", "--quiet", paste0("refs/heads/", name)),
    error_on_status = FALSE, timeout = 5
  )
  identical(result$status, 0L)
}

#' Create a feature branch, push it to GitHub, and open a Pull Request
#'
#' Implements spec Sec 22 (0.3.0 Protected-branch workflow via generated branch + PR).
#'
#' @param repo_root Canonical path to the Git repository root.
#' @param git_bin Path to the `git` executable.
#' @param target_branch Base branch to merge into (e.g. `"main"`).
#' @param pr_branch Name of feature branch to create and push (optional; auto-generated if NULL).
#' @param title Pull request title (optional; extracted from latest commit summary if NULL).
#' @param body Pull request description body (optional).
#' @param session_state Mutable session state for GitHub token resolution.
#' @param api_base Base URL for GitHub API (overridable for testing).
#' @return A list with `ok = TRUE` and PR details on success, or `ok = FALSE` and error details.
#' @noRd
.git_create_pull_request <- function(repo_root, git_bin, target_branch = NULL, pr_branch = NULL,
                                     title = NULL, body = NULL, session_state = NULL,
                                     policy = NULL, api_base = NULL, gh_bin = Sys.which("gh")) {
  status <- .git_status(repo_root, git_bin)
  if (isTRUE(status$detached)) {
    return(list(
      ok = FALSE, code = "DETACHED_HEAD",
      message = "Check out a branch before creating a pull request.", recoverable = FALSE
    ))
  }

  current_branch <- status$branch %||% "main"
  if (is.null(target_branch) || !nzchar(trimws(target_branch))) {
    target_branch <- current_branch
  }

  if (is.null(policy)) {
    policy <- .read_repo_policy(repo_root)
  }

  # Verify remote is a GitHub repository
  remote_url <- NULL
  rem_res <- tryCatch(
    processx::run(git_bin, c("-C", repo_root, "config", "--get", "remote.origin.url"), error_on_status = FALSE, timeout = 5),
    error = function(e) NULL
  )
  if (!is.null(rem_res) && identical(rem_res$status, 0L)) {
    remote_url <- trimws(rem_res$stdout)
  }

  slug <- .parse_github_slug(remote_url, policy = policy)
  if (is.null(slug)) {
    return(list(
      ok = FALSE, code = "REMOTE_NOT_GITHUB",
      message = "Pull requests can only be opened for GitHub repositories.", recoverable = FALSE
    ))
  }

  if (is.null(api_base) || !nzchar(api_base)) {
    api_base <- .github_api_base(slug, policy = policy)
  }

  host <- slug$host %||% policy$github_host %||% "github.com"
  is_enterprise <- isTRUE(slug$is_enterprise)

  token_info <- .get_github_token(session_state, host = host, is_enterprise = is_enterprise, gh_bin = gh_bin)
  if (is.null(token_info)) {
    return(list(
      ok = FALSE, code = "GITHUB_AUTH_REQUIRED",
      message = "Connecting your GitHub account is required to open a Pull Request.", recoverable = TRUE
    ))
  }

  prefix <- policy$pr_branch_prefix %||% "update/"

  if (is.null(pr_branch) || !nzchar(trimws(pr_branch))) {
    timestamp_str <- format(Sys.time(), "%Y%m%d-%H%M%S")
    pr_branch <- paste0(prefix, timestamp_str)
  } else {
    pr_branch <- trimws(pr_branch)
  }

  if (!.validate_branch_name(git_bin, pr_branch)) {
    return(list(
      ok = FALSE, code = "INVALID_BRANCH_NAME",
      message = paste0("'", pr_branch, "' is not a valid Git branch name."), recoverable = TRUE
    ))
  }

  if (.git_branch_exists_locally(repo_root, git_bin, pr_branch)) {
    return(list(
      ok = FALSE, code = "BRANCH_EXISTS",
      message = paste0("A branch named '", pr_branch, "' already exists locally."), recoverable = TRUE
    ))
  }

  # If title is missing, use commit summary
  if (is.null(title) || !nzchar(trimws(title))) {
    log_res <- tryCatch(
      processx::run(git_bin, c("-C", repo_root, "log", "-1", "--format=%s"), error_on_status = FALSE, timeout = 5),
      error = function(e) NULL
    )
    if (!is.null(log_res) && identical(log_res$status, 0L) && nzchar(trimws(log_res$stdout))) {
      title <- trimws(log_res$stdout)
    } else {
      title <- paste0("Updates for ", target_branch)
    }
  }

  if (is.null(body)) {
    body_res <- tryCatch(
      processx::run(git_bin, c("-C", repo_root, "log", "-1", "--format=%b"), error_on_status = FALSE, timeout = 5),
      error = function(e) NULL
    )
    if (!is.null(body_res) && identical(body_res$status, 0L) && nzchar(trimws(body_res$stdout))) {
      body <- trimws(body_res$stdout)
    } else {
      body <- ""
    }
  }

  # Step 1: Create local branch pointing to HEAD
  create_res <- processx::run(
    git_bin, c("-C", repo_root, "branch", pr_branch, "HEAD"),
    error_on_status = FALSE, timeout = 10
  )
  if (!identical(create_res$status, 0L)) {
    return(list(
      ok = FALSE, code = "COMMAND_FAILED",
      message = "Failed to create feature branch.",
      advanced = list(command = paste("git branch", pr_branch, "HEAD"), exit_status = create_res$status, stderr = create_res$stderr),
      recoverable = TRUE
    ))
  }

  # Clean up local branch on failure if subsequent steps fail
  created_branch <- TRUE
  on.exit({
    if (isTRUE(created_branch)) {
      tryCatch(
        processx::run(git_bin, c("-C", repo_root, "branch", "-D", pr_branch), error_on_status = FALSE, timeout = 5),
        error = function(e) NULL
      )
    }
  }, add = TRUE)

  # Step 2: Push branch to remote origin
  push_args <- c("-C", repo_root, "push", "origin", paste0("refs/heads/", pr_branch, ":refs/heads/", pr_branch))
  push_res <- processx::run(git_bin, push_args, error_on_status = FALSE, timeout = 60)

  if (!identical(push_res$status, 0L)) {
    code <- .classify_push_failure(push_res$stderr)
    return(list(
      ok = FALSE, code = code,
      message = paste0("Failed to push feature branch '", pr_branch, "' to GitHub."),
      advanced = list(
        command = paste("git push origin", pr_branch),
        exit_status = push_res$status,
        stderr = .sanitize_git_output(push_res$stderr)
      ),
      recoverable = TRUE
    ))
  }

  # Step 3: Open Pull Request via GitHub REST API
  api_path <- sprintf("/repos/%s/%s/pulls", slug$owner, slug$repo)
  pr_payload <- list(
    title = title,
    head = pr_branch,
    base = target_branch,
    body = body
  )

  api_res <- .github_api_call(
    api_path, method = "POST", body = pr_payload,
    token = token_info$token, api_base = api_base
  )

  if (!api_res$ok || !is.list(api_res$data)) {
    return(list(
      ok = FALSE, code = "PR_CREATION_FAILED",
      message = api_res$error %||% "GitHub rejected the pull request creation.",
      recoverable = TRUE
    ))
  }

  # Successful PR: do not delete the branch on exit
  created_branch <- FALSE

  list(
    ok = TRUE,
    pr_number = api_res$data$number,
    pr_url = api_res$data$html_url,
    pr_branch = pr_branch,
    base_branch = target_branch,
    title = api_res$data$title,
    state = api_res$data$state
  )
}
