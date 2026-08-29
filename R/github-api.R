#' Parse GitHub owner and repo slug from a remote URL
#'
#' Supports HTTPS, SSH (scp-like and ssh://), and git:// URLs for github.com
#' as well as GitHub Enterprise Server (GHES) and GitHub Enterprise Cloud (GHEC).
#'
#' @param url Remote URL string.
#' @param policy Optional repository policy list from `.read_repo_policy()`.
#' @return A list with `owner`, `repo`, `host`, `is_enterprise`, or `NULL` if not a recognized GitHub URL.
#' @noRd
.parse_github_slug <- function(url, policy = NULL) {
  if (is.null(url) || !is.character(url) || length(url) == 0L || !nzchar(url[[1]])) {
    return(NULL)
  }
  clean <- trimws(url[[1]])
  clean <- sub("\\.git/?$", "", clean)

  host <- NULL
  path_part <- NULL

  # SSH scp style: git@github.com:owner/repo or git@github.company.com:owner/repo
  if (grepl("^[a-zA-Z0-9._-]+@[a-zA-Z0-9._-]+:", clean)) {
    host <- sub(":.*$", "", clean)
    host <- sub("^[a-zA-Z0-9._-]+@", "", host)
    path_part <- sub("^[^:]+:", "", clean)
  } else if (grepl("^[a-zA-Z0-9+.-]+://", clean)) {
    # URL style: https://github.com/owner/repo, ssh://git@host/owner/repo
    scheme_stripped <- sub("^[a-zA-Z0-9+.-]+://", "", clean)
    host_full <- sub("/.*$", "", scheme_stripped)
    host <- sub("^.*@", "", host_full) # strip user@
    host <- sub(":[0-9]+$", "", host)  # strip port
    path_part <- sub("^[^/]+/", "", scheme_stripped)
  }

  if (is.null(host) || is.null(path_part)) {
    return(NULL)
  }

  host_lower <- tolower(host)
  is_github_com <- identical(host_lower, "github.com") || grepl("\\.github\\.com$", host_lower)
  is_ghe_cloud <- grepl("\\.ghe\\.com$", host_lower)
  is_ghe_heuristic <- grepl("^github\\.", host_lower) || grepl("\\.github\\.", host_lower)
  is_policy_host <- !is.null(policy$github_host) && identical(host_lower, tolower(policy$github_host))
  is_env_host <- nzchar(Sys.getenv("GH_HOST")) && identical(host_lower, tolower(Sys.getenv("GH_HOST")))
  has_policy_api <- !is.null(policy$github_api_url) && nzchar(policy$github_api_url)
  has_env_api <- nzchar(Sys.getenv("GITHUB_API_URL")) || nzchar(Sys.getenv("GITHUB_ENTERPRISE_URL"))

  is_recognized_github <- is_github_com || is_ghe_cloud || is_ghe_heuristic || is_policy_host || is_env_host || has_policy_api || has_env_api

  if (!is_recognized_github) {
    return(NULL)
  }

  parts <- strsplit(path_part, "/")[[1]]
  parts <- parts[nzchar(parts)]
  if (length(parts) < 2L) {
    return(NULL)
  }

  owner <- parts[[length(parts) - 1L]]
  repo <- parts[[length(parts)]]

  list(
    owner = owner,
    repo = repo,
    host = host,
    is_enterprise = !is_github_com
  )
}

#' Resolve GitHub REST API base URL
#'
#' @param slug Optional list from `.parse_github_slug()`.
#' @param policy Optional repository policy list from `.read_repo_policy()`.
#' @return Character string containing the API base URL.
#' @noRd
.github_api_base <- function(slug = NULL, policy = NULL) {
  if (!is.null(policy$github_api_url) && nzchar(policy$github_api_url)) {
    return(sub("/+$", "", policy$github_api_url))
  }

  for (var in c("GITHUB_API_URL", "GITHUB_ENTERPRISE_URL")) {
    val <- Sys.getenv(var, unset = "")
    if (nzchar(val)) {
      return(sub("/+$", "", trimws(val)))
    }
  }

  if (is.null(slug) || is.null(slug$host) || identical(tolower(slug$host), "github.com")) {
    return("https://api.github.com")
  }

  host_lower <- tolower(slug$host)
  if (grepl("\\.ghe\\.com$", host_lower)) {
    return(paste0("https://api.", host_lower))
  }

  # GitHub Enterprise Server default API path is /api/v3
  paste0("https://", slug$host, "/api/v3")
}

#' Resolve GitHub API token from session state, environment, or gh CLI
#'
#' Follows precedence:
#' 1. In-session token configured by user
#' 2. Environment variables (GITHUB_ENTERPRISE_TOKEN / GH_ENTERPRISE_TOKEN / GITHUB_PAT / GITHUB_TOKEN / GH_TOKEN)
#' 3. GitHub CLI `gh auth token --hostname <host>` (or `gh auth token`)
#'
#' @param session_state Mutable session state environment.
#' @param host Optional hostname string (e.g. `"github.com"` or `"github.company.com"`).
#' @param is_enterprise Logical scalar indicating whether the host is an enterprise instance.
#' @param gh_bin Path to GitHub CLI executable.
#' @return A list with `token` (string) and `source` (`"session"`, `"env"`, or `"gh_cli"`),
#'   or `NULL` if no token could be resolved.
#' @noRd
.get_github_token <- function(session_state = NULL, host = "github.com", is_enterprise = FALSE, gh_bin = Sys.which("gh")) {
  if (!is.null(session_state) && !is.null(session_state$github_token)) {
    tok <- trimws(as.character(session_state$github_token)[[1]])
    if (nzchar(tok)) {
      return(list(token = tok, source = "session"))
    }
  }

  env_vars <- if (isTRUE(is_enterprise)) {
    c("GITHUB_ENTERPRISE_TOKEN", "GH_ENTERPRISE_TOKEN", "GITHUB_PAT", "GITHUB_TOKEN", "GH_TOKEN")
  } else {
    c("GITHUB_PAT", "GITHUB_TOKEN", "GH_TOKEN")
  }

  for (var in env_vars) {
    val <- Sys.getenv(var, unset = "")
    if (nzchar(val)) {
      return(list(token = trimws(val), source = "env"))
    }
  }

  if (nzchar(gh_bin)) {
    # If a specific host is provided and not default github.com, pass --hostname
    args <- if (!is.null(host) && nzchar(host) && !identical(tolower(host), "github.com")) {
      c("auth", "token", "--hostname", host)
    } else {
      c("auth", "token")
    }

    res <- tryCatch(
      processx::run(gh_bin, args, error_on_status = FALSE, timeout = 3),
      error = function(e) NULL
    )
    if (!is.null(res) && identical(res$status, 0L)) {
      tok <- trimws(res$stdout)
      if (nzchar(tok)) {
        return(list(token = tok, source = "gh_cli"))
      }
    }
  }

  NULL
}

#' Perform a GitHub REST API request with authorization and timeout
#'
#' @param path API path (e.g. `"/user"` or `"/repos/owner/repo/pulls"`).
#' @param method HTTP method string (`"GET"`, `"POST"`, etc.).
#' @param body Optional R list to serialize as JSON body.
#' @param token Bearer token for authorization.
#' @param api_base Base URL for GitHub API.
#' @param timeout Timeout in seconds.
#' @return A list with `ok` (logical), `status_code` (integer), `data` (parsed JSON list),
#'   and `error` (sanitized string or `NULL`).
#' @noRd
.github_api_call <- function(path, method = "GET", body = NULL, token = NULL, api_base = "https://api.github.com", timeout = 10) {
  if (is.null(token) || !nzchar(token)) {
    return(list(
      ok = FALSE,
      status_code = 401L,
      error = "GitHub authentication required. Connect your GitHub account to proceed.",
      data = NULL
    ))
  }

  url <- if (startsWith(path, "http://") || startsWith(path, "https://")) {
    path
  } else {
    paste0(sub("/+$", "", api_base), "/", sub("^/+", "", path))
  }

  h <- curl::new_handle()
  headers <- c(
    "Accept" = "application/vnd.github+json",
    "Authorization" = paste("Bearer", token),
    "X-GitHub-Api-Version" = "2022-11-28",
    "User-Agent" = "gitneighbr"
  )

  if (!is.null(body)) {
    headers <- c(headers, "Content-Type" = "application/json")
    json_body <- jsonlite::toJSON(body, auto_unbox = TRUE)
    curl::handle_setopt(h, customrequest = method, postfields = json_body)
  } else if (!identical(method, "GET")) {
    curl::handle_setopt(h, customrequest = method)
  }

  curl::handle_setheaders(h, .list = as.list(headers))
  curl::handle_setopt(h, timeout = timeout)

  res <- tryCatch(
    curl::curl_fetch_memory(url, handle = h),
    error = function(e) {
      list(.network_error = conditionMessage(e))
    }
  )

  if (!is.null(res[[".network_error"]])) {
    err_text <- sub(token, "[REDACTED_TOKEN]", res[[".network_error"]], fixed = TRUE)
    return(list(
      ok = FALSE,
      status_code = 0L,
      error = paste0("GitHub network error: ", err_text),
      data = NULL
    ))
  }

  raw_text <- tryCatch(rawToChar(res$content), error = function(e) "")
  parsed <- tryCatch(
    jsonlite::fromJSON(raw_text, simplifyVector = FALSE),
    error = function(e) list(message = raw_text)
  )

  ok <- (res$status_code >= 200L && res$status_code < 300L)
  err_msg <- NULL
  if (!ok) {
    if (is.list(parsed) && !is.null(parsed$message)) {
      err_msg <- sub(token, "[REDACTED_TOKEN]", as.character(parsed$message), fixed = TRUE)
    } else {
      err_msg <- paste0("GitHub API returned status ", res$status_code)
    }
  }

  list(
    ok = ok,
    status_code = res$status_code,
    data = parsed,
    error = err_msg
  )
}

#' Query authenticated GitHub user profile
#' @noRd
.github_get_user <- function(token, api_base = "https://api.github.com") {
  res <- .github_api_call("/user", method = "GET", token = token, api_base = api_base)
  if (!res$ok || !is.list(res$data)) {
    return(NULL)
  }
  list(
    login = res$data$login %||% NULL,
    name = res$data$name %||% NULL,
    avatar_url = res$data$avatar_url %||% NULL,
    html_url = res$data$html_url %||% NULL
  )
}

#' Check whether an upstream branch has branch protection enabled
#' @noRd
.github_check_branch_protection <- function(owner, repo, branch, token, api_base = "https://api.github.com") {
  if (is.null(owner) || is.null(repo) || is.null(branch) || !nzchar(branch)) {
    return(FALSE)
  }
  path <- sprintf("/repos/%s/%s/branches/%s", owner, repo, branch)
  res <- .github_api_call(path, method = "GET", token = token, api_base = api_base)
  if (!res$ok || !is.list(res$data)) {
    return(FALSE)
  }
  isTRUE(res$data$protected)
}

#' Complete GitHub API status diagnosis for a repository
#' @noRd
.github_api_status <- function(repo_root, git_bin = Sys.which("git"), session_state = NULL,
                               policy = NULL, api_base = NULL, gh_bin = Sys.which("gh")) {
  remote_url <- NULL
  branch <- NULL

  if (!is.null(repo_root) && nzchar(repo_root) && file.exists(repo_root)) {
    if (is.null(policy)) {
      policy <- .read_repo_policy(repo_root)
    }
    rem_res <- tryCatch(
      processx::run(git_bin, c("-C", repo_root, "config", "--get", "remote.origin.url"), error_on_status = FALSE, timeout = 5),
      error = function(e) NULL
    )
    if (!is.null(rem_res) && identical(rem_res$status, 0L)) {
      remote_url <- trimws(rem_res$stdout)
    }
    head_res <- tryCatch(
      processx::run(git_bin, c("-C", repo_root, "rev-parse", "--abbrev-ref", "HEAD"), error_on_status = FALSE, timeout = 5),
      error = function(e) NULL
    )
    if (!is.null(head_res) && identical(head_res$status, 0L)) {
      branch <- trimws(head_res$stdout)
      if (identical(branch, "HEAD")) branch <- NULL
    }
  }

  slug <- .parse_github_slug(remote_url, policy = policy)
  if (is.null(api_base) || !nzchar(api_base)) {
    api_base <- .github_api_base(slug, policy = policy)
  }

  host <- if (!is.null(slug) && !is.null(slug$host)) slug$host else (policy$github_host %||% "github.com")
  is_enterprise <- if (!is.null(slug)) isTRUE(slug$is_enterprise) else FALSE

  token_info <- .get_github_token(session_state, host = host, is_enterprise = is_enterprise, gh_bin = gh_bin)
  has_token <- !is.null(token_info)

  user <- NULL
  branch_protected <- FALSE
  connected <- FALSE

  if (has_token) {
    user <- .github_get_user(token_info$token, api_base = api_base)
    connected <- !is.null(user)
    if (connected && !is.null(slug) && !is.null(branch)) {
      branch_protected <- .github_check_branch_protection(slug$owner, slug$repo, branch, token_info$token, api_base = api_base)
    }
  }

  list(
    is_github = !is.null(slug),
    is_enterprise = is_enterprise,
    host = host,
    api_base = api_base,
    owner = if (!is.null(slug)) slug$owner else NULL,
    repo = if (!is.null(slug)) slug$repo else NULL,
    remote_url = remote_url,
    connected = connected,
    token_source = if (connected) token_info$source else NULL,
    user = user,
    branch_protected = branch_protected,
    gh_cli_available = nzchar(gh_bin)
  )
}
