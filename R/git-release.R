#' Extract the annotation body text for an existing Git tag
#' @noRd
.git_tag_annotation <- function(repo_root, git_bin, tag_name) {
  result <- tryCatch(
    processx::run(
      git_bin, c("-C", repo_root, "tag", "-l", "--format=%(contents)", tag_name),
      error_on_status = FALSE, timeout = 5
    ),
    error = function(e) NULL
  )
  if (is.null(result) || !identical(result$status, 0L)) {
    return("")
  }
  trimws(result$stdout)
}

#' Create a GitHub Release from a local or pushed annotated tag
#'
#' Implements spec Sec 22 (0.3.0 Release creation from a pushed tag).
#'
#' @param repo_root Canonical path to the Git repository root.
#' @param git_bin Path to the `git` executable.
#' @param tag_name Name of the tag to release.
#' @param name Title of the release (optional; defaults to `tag_name`).
#' @param body Release description/notes (optional; defaults to tag annotation).
#' @param draft Logical indicating whether the release is a draft (default `FALSE`).
#' @param prerelease Logical indicating whether the release is a pre-release (default `FALSE`).
#' @param session_state Mutable session state for token resolution.
#' @param api_base Base URL for GitHub API (overridable for testing).
#' @return A list with `ok = TRUE` and release details, or `ok = FALSE` and error details.
#' @noRd
.git_create_release <- function(repo_root, git_bin, tag_name, name = NULL, body = NULL,
                                draft = FALSE, prerelease = FALSE, session_state = NULL,
                                policy = NULL, api_base = NULL, gh_bin = Sys.which("gh")) {
  if (is.null(tag_name) || !nzchar(trimws(tag_name))) {
    return(list(
      ok = FALSE, code = "INVALID_TAG",
      message = "A tag name is required to create a release.", recoverable = TRUE
    ))
  }
  tag_name <- trimws(tag_name)

  if (!.git_tag_exists_locally(repo_root, git_bin, tag_name)) {
    return(list(
      ok = FALSE, code = "TAG_NOT_FOUND",
      message = paste0("Tag '", tag_name, "' does not exist locally."), recoverable = TRUE
    ))
  }

  if (is.null(policy) && !is.null(repo_root) && nzchar(repo_root)) {
    policy <- .read_repo_policy(repo_root)
  }

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
      message = "Releases can only be created for GitHub repositories.", recoverable = FALSE
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
      message = "Connecting your GitHub account is required to publish a Release.", recoverable = TRUE
    ))
  }

  # Ensure the tag is pushed to remote origin
  push_res <- .git_push_tag(repo_root, git_bin, tag_name)
  if (!push_res$ok && !identical(push_res$code, "TAG_EXISTS")) {
    return(list(
      ok = FALSE, code = push_res$code,
      message = paste0("Failed to push tag '", tag_name, "' to GitHub before creating release: ", push_res$message),
      advanced = push_res$advanced,
      recoverable = TRUE
    ))
  }

  if (is.null(name) || !nzchar(trimws(name))) {
    name <- tag_name
  } else {
    name <- trimws(name)
  }

  if (is.null(body) || !nzchar(trimws(body))) {
    body <- .git_tag_annotation(repo_root, git_bin, tag_name) %||% ""
  } else {
    body <- trimws(body)
  }

  payload <- list(
    tag_name = tag_name,
    name = name,
    body = body,
    draft = isTRUE(draft),
    prerelease = isTRUE(prerelease)
  )

  api_path <- sprintf("/repos/%s/%s/releases", slug$owner, slug$repo)
  api_res <- .github_api_call(api_path, method = "POST", body = payload, token = token_info$token, api_base = api_base)

  if (!api_res$ok) {
    return(list(
      ok = FALSE, code = "RELEASE_CREATION_FAILED",
      message = paste0("Failed to create GitHub release: ", api_res$error %||% "Unknown API error"),
      recoverable = TRUE
    ))
  }

  rel_data <- api_res$data
  list(
    ok = TRUE,
    code = "RELEASE_CREATED",
    message = paste0("Published release '", name, "' for tag '", tag_name, "'."),
    data = list(
      release_id = rel_data$id %||% NULL,
      tag_name = tag_name,
      name = name,
      html_url = rel_data$html_url %||% NULL,
      draft = isTRUE(draft),
      prerelease = isTRUE(prerelease)
    )
  )
}

#' List recent releases for a repository from GitHub API
#' @noRd
.git_list_releases <- function(repo_root, git_bin, session_state = NULL, policy = NULL,
                               api_base = NULL, gh_bin = Sys.which("gh")) {
  remote_url <- NULL
  rem_res <- tryCatch(
    processx::run(git_bin, c("-C", repo_root, "config", "--get", "remote.origin.url"), error_on_status = FALSE, timeout = 5),
    error = function(e) NULL
  )
  if (!is.null(rem_res) && identical(rem_res$status, 0L)) {
    remote_url <- trimws(rem_res$stdout)
  }

  if (is.null(policy) && !is.null(repo_root) && nzchar(repo_root)) {
    policy <- .read_repo_policy(repo_root)
  }

  slug <- .parse_github_slug(remote_url, policy = policy)
  if (is.null(slug)) {
    return(list(ok = FALSE, code = "REMOTE_NOT_GITHUB", message = "Not a GitHub repository.", releases = list()))
  }

  if (is.null(api_base) || !nzchar(api_base)) {
    api_base <- .github_api_base(slug, policy = policy)
  }

  host <- slug$host %||% policy$github_host %||% "github.com"
  is_enterprise <- isTRUE(slug$is_enterprise)

  token_info <- .get_github_token(session_state, host = host, is_enterprise = is_enterprise, gh_bin = gh_bin)
  token <- if (!is.null(token_info)) token_info$token else NULL

  api_path <- sprintf("/repos/%s/%s/releases?per_page=30", slug$owner, slug$repo)
  api_res <- .github_api_call(api_path, method = "GET", token = token, api_base = api_base)

  if (!api_res$ok || !is.list(api_res$data)) {
    return(list(ok = FALSE, code = "GITHUB_API_ERROR", message = api_res$error, releases = list()))
  }

  releases <- lapply(api_res$data, function(r) {
    list(
      id = r$id,
      tag_name = r$tag_name,
      name = r$name,
      html_url = r$html_url,
      draft = r$draft,
      prerelease = r$prerelease,
      published_at = r$published_at
    )
  })

  list(ok = TRUE, releases = releases)
}
