#' Validate a tag name against `git check-ref-format`
#'
#' Spec Sec 8.5: tag names must pass `git check-ref-format --allow-onelevel
#' refs/tags/<name>`. This delegates the ref-name rules to Git itself
#' rather than reimplementing them.
#' @noRd
.validate_tag_name <- function(git_bin, name) {
  if (is.null(name) || length(name) != 1L || !is.character(name) || is.na(name) || !nzchar(name)) {
    return(FALSE)
  }
  result <- processx::run(
    git_bin, c("check-ref-format", "--allow-onelevel", paste0("refs/tags/", name)),
    error_on_status = FALSE, timeout = 5
  )
  identical(result$status, 0L)
}

#' Whether a tag name already exists in the local repository
#' @noRd
.git_tag_exists_locally <- function(repo_root, git_bin, name) {
  result <- processx::run(
    git_bin, c("-C", repo_root, "show-ref", "--verify", "--quiet", paste0("refs/tags/", name)),
    error_on_status = FALSE, timeout = 5
  )
  identical(result$status, 0L)
}

#' Map `git push`'s stderr to a stable code when pushing a tag ref
#'
#' Order matters: a tag collision on the remote surfaces as "already
#' exists" (Git never fast-forwards a tag ref, so a same-named tag pointing
#' elsewhere is always rejected rather than moved), which must be
#' classified as `TAG_EXISTS` before falling back to the general push
#' classifier from `git-remote.R`.
#' @noRd
.classify_tag_push_failure <- function(stderr_text) {
  text <- tolower(stderr_text %||% "")
  if (grepl("already exists", text, fixed = TRUE)) {
    return("TAG_EXISTS")
  }
  .classify_push_failure(stderr_text)
}

#' Create one annotated tag on the current `HEAD`
#'
#' Implements spec Sec 8.5: only one annotated tag is ever created, on
#' `HEAD` (called after the triggering commit succeeds). The name is
#' validated with `git check-ref-format` and refused as `INVALID_TAG` when
#' malformed, and refused as `TAG_EXISTS` when a local tag of that name
#' already exists -- existing tags are never moved or overwritten. The
#' annotation is optional (falls back to the tag name itself, since an
#' annotated tag needs some message) and is written to a secure temporary
#' file per spec Sec 13.2 rather than interpolated onto the command line.
#'
#' @return A list. On success: `ok = TRUE`, `name`, `sha`. On failure:
#'   `ok = FALSE`, `code`, `message`, `recoverable`.
#' @noRd
.git_create_tag <- function(repo_root, git_bin, name, annotation = NULL) {
  name <- if (is.null(name)) "" else trimws(as.character(name)[[1]])

  if (!.validate_tag_name(git_bin, name)) {
    return(list(
      ok = FALSE, code = "INVALID_TAG",
      message = "That version label isn't a valid tag name.", recoverable = TRUE
    ))
  }
  if (.git_tag_exists_locally(repo_root, git_bin, name)) {
    return(list(
      ok = FALSE, code = "TAG_EXISTS",
      message = "A tag with that name already exists.", recoverable = TRUE
    ))
  }

  annotation <- if (is.null(annotation) || length(annotation) != 1L || !nzchar(trimws(annotation))) {
    name
  } else {
    annotation
  }

  msg_file <- tempfile("gitneighbr-tag-")
  on.exit(unlink(msg_file), add = TRUE)
  writeLines(annotation, msg_file, useBytes = TRUE)
  Sys.chmod(msg_file, "0600")

  tag_result <- processx::run(
    git_bin, c("-C", repo_root, "tag", "-a", name, "-F", msg_file),
    error_on_status = FALSE, timeout = 15
  )
  if (!identical(tag_result$status, 0L)) {
    return(list(
      ok = FALSE, code = "COMMAND_FAILED",
      message = "Git could not create that tag.", recoverable = TRUE
    ))
  }

  sha <- trimws(processx::run(
    git_bin, c("-C", repo_root, "rev-parse", "--short", paste0(name, "^{commit}")),
    error_on_status = TRUE, timeout = 15
  )$stdout)

  list(ok = TRUE, name = name, sha = sha)
}

#' Push one exact local tag as `refs/tags/<name>`
#'
#' Implements spec Sec 8.5: the tag is pushed explicitly by full ref
#' (`refs/tags/<name>:refs/tags/<name>`), never a bare tag name, and never
#' with `--force`. Git itself refuses to move an existing tag ref, so a
#' remote collision surfaces as a normal push failure that
#' `.classify_tag_push_failure()` maps to `TAG_EXISTS`. Uses the same
#' remote as the current branch's configured upstream, matching the
#' branch-then-tag ordering from spec Sec 8.5 point 6 (the branch push
#' happens first, via a separate `/api/v1/push` call the frontend
#' orchestrates).
#'
#' @return A list. On success: `ok = TRUE`, `remote`, `name`. On failure:
#'   `ok = FALSE`, `code`, `message`, `recoverable`.
#' @noRd
.git_push_tag <- function(repo_root, git_bin, name) {
  name <- if (is.null(name)) "" else trimws(as.character(name)[[1]])

  if (!.validate_tag_name(git_bin, name)) {
    return(list(
      ok = FALSE, code = "INVALID_TAG",
      message = "That version label isn't a valid tag name.", recoverable = TRUE
    ))
  }
  if (!.git_tag_exists_locally(repo_root, git_bin, name)) {
    return(list(
      ok = FALSE, code = "COMMAND_FAILED",
      message = "That tag does not exist locally yet.", recoverable = TRUE
    ))
  }

  status <- .git_status(repo_root, git_bin)
  if (status$detached) {
    return(list(
      ok = FALSE, code = "DETACHED_HEAD",
      message = "Check out a branch before sending a version label.", recoverable = FALSE
    ))
  }
  upstream_info <- .git_upstream_info(repo_root, git_bin, status$branch)
  if (is.null(upstream_info)) {
    return(list(
      ok = FALSE, code = "NO_UPSTREAM",
      message = "This branch has no destination configured on GitHub yet.", recoverable = FALSE
    ))
  }

  refspec <- paste0("refs/tags/", name, ":refs/tags/", name)
  push_result <- processx::run(
    git_bin, c("-C", repo_root, "push", upstream_info$remote, refspec),
    error_on_status = FALSE, timeout = 60
  )
  if (!identical(push_result$status, 0L)) {
    return(list(
      ok = FALSE, code = .classify_tag_push_failure(push_result$stderr),
      message = "GitHub rejected this version label.", recoverable = TRUE
    ))
  }

  list(ok = TRUE, remote = upstream_info$remote, name = name)
}
