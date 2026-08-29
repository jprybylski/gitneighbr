#' Default policy settings for a repository
#' @noRd
.default_repo_policy <- function() {
  list(
    has_policy_file = FALSE,
    policy_file_path = NULL,
    valid = TRUE,
    error = NULL,
    require_pull_request = FALSE,
    protected_branches = character(0),
    default_tag_prefix = "v",
    require_version_tags = FALSE,
    disallow_untracked_trash = FALSE,
    pr_branch_prefix = "update/",
    github_host = NULL,
    github_api_url = NULL
  )
}

#' Locate a policy file within a repository root
#'
#' Checks for `.gitneighbr.json` and `.github/gitneighbr.json`.
#' @noRd
.find_policy_file <- function(repo_root) {
  if (is.null(repo_root) || !nzchar(repo_root)) {
    return(NULL)
  }
  candidates <- c(
    file.path(repo_root, ".gitneighbr.json"),
    file.path(repo_root, ".github", "gitneighbr.json")
  )
  for (cand in candidates) {
    if (file.exists(cand)) {
      return(cand)
    }
  }
  NULL
}

#' Read and parse repository-specific policy configuration
#'
#' Implements spec Sec 22 (0.3.0 repository policy configuration).
#' Reads configuration from `.gitneighbr.json` or `.github/gitneighbr.json` if
#' present, merging values with standard defaults. Gracefully handles syntax
#' errors without aborting application operation.
#'
#' @param repo_root Canonical path to the Git repository root.
#' @return A list containing resolved policy fields.
#' @noRd
.read_repo_policy <- function(repo_root) {
  defaults <- .default_repo_policy()
  file_path <- .find_policy_file(repo_root)

  if (is.null(file_path)) {
    return(defaults)
  }

  rel_path <- tryCatch(
    as.character(fs::path_rel(file_path, start = repo_root)),
    error = function(e) basename(file_path)
  )

  parsed <- tryCatch(
    jsonlite::fromJSON(file_path, simplifyVector = TRUE),
    error = function(e) {
      list(.parse_error = conditionMessage(e))
    }
  )

  if (!is.null(parsed[[".parse_error"]])) {
    defaults$has_policy_file <- TRUE
    defaults$policy_file_path <- rel_path
    defaults$valid <- FALSE
    defaults$error <- paste0("Could not parse policy file: ", parsed[[".parse_error"]])
    return(defaults)
  }

  if (!is.list(parsed)) {
    defaults$has_policy_file <- TRUE
    defaults$policy_file_path <- rel_path
    defaults$valid <- FALSE
    defaults$error <- "Policy configuration must be a JSON object."
    return(defaults)
  }

  res <- defaults
  res$has_policy_file <- TRUE
  res$policy_file_path <- rel_path
  res$valid <- TRUE
  res$error <- NULL

  if (!is.null(parsed$require_pull_request) && is.logical(parsed$require_pull_request)) {
    res$require_pull_request <- isTRUE(parsed$require_pull_request[[1]])
  }

  if (!is.null(parsed$protected_branches)) {
    branches <- as.character(parsed$protected_branches)
    branches <- branches[!is.na(branches) & nzchar(trimws(branches))]
    res$protected_branches <- branches
  } else if (isTRUE(res$require_pull_request)) {
    res$protected_branches <- c("main", "master")
  }

  if (!is.null(parsed$default_tag_prefix) && is.character(parsed$default_tag_prefix)) {
    res$default_tag_prefix <- as.character(parsed$default_tag_prefix[[1]])
  }

  if (!is.null(parsed$require_version_tags) && is.logical(parsed$require_version_tags)) {
    res$require_version_tags <- isTRUE(parsed$require_version_tags[[1]])
  }

  if (!is.null(parsed$disallow_untracked_trash) && is.logical(parsed$disallow_untracked_trash)) {
    res$disallow_untracked_trash <- isTRUE(parsed$disallow_untracked_trash[[1]])
  }

  if (!is.null(parsed$pr_branch_prefix) && is.character(parsed$pr_branch_prefix)) {
    prefix <- trimws(as.character(parsed$pr_branch_prefix[[1]]))
    if (nzchar(prefix)) {
      res$pr_branch_prefix <- prefix
    }
  }

  if (!is.null(parsed$github_host) && is.character(parsed$github_host)) {
    host <- trimws(as.character(parsed$github_host[[1]]))
    if (nzchar(host)) {
      res$github_host <- host
    }
  }

  if (!is.null(parsed$github_api_url) && is.character(parsed$github_api_url)) {
    api_url <- trimws(as.character(parsed$github_api_url[[1]]))
    if (nzchar(api_url)) {
      res$github_api_url <- sub("/+$", "", api_url)
    }
  }

  res
}

#' Check whether a branch name matches protected branch rules
#'
#' @param branch Branch name to check.
#' @param policy Resolved repository policy list from `.read_repo_policy()`.
#' @param github_protected Logical indicating whether remote GitHub API
#'   reported branch protection for this branch.
#' @return Logical scalar `TRUE` if the branch is protected.
#' @noRd
.is_branch_protected <- function(branch, policy = NULL, github_protected = FALSE) {
  if (isTRUE(github_protected)) {
    return(TRUE)
  }
  if (is.null(branch) || !nzchar(branch)) {
    return(FALSE)
  }
  if (is.null(policy)) {
    policy <- .default_repo_policy()
  }

  patterns <- policy$protected_branches
  if (is.null(patterns) || length(patterns) == 0L) {
    return(FALSE)
  }

  for (pat in patterns) {
    if (identical(branch, pat)) {
      return(TRUE)
    }
    # Support wildcard glob patterns like release/* or prod-*
    if (grepl("[*?]", pat)) {
      rx <- utils::glob2rx(pat)
      if (grepl(rx, branch)) {
        return(TRUE)
      }
    }
  }

  FALSE
}
