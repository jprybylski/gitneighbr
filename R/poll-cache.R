#' Per-session memo for the status poll
#'
#' The page polls `api/v1/status` every 2 seconds. Most of what a poll needs
#' (Git's availability, the repository check, identity and other settings,
#' the hooks folder, line counts of unchanged files) changes far less often
#' than that, and on Windows every Git process costs tens of milliseconds
#' (issue #50). These helpers remember such answers per session, keyed on
#' cheap file signatures read from R, so a steady poll starts one Git
#' process: the `git status` itself.
#'
#' An entry is recomputed when its key changes, when it is older than
#' `getOption("gitneighbr.poll_cache_ttl", 60)` seconds (a safety net for
#' changes no signature sees, such as a config `include` file), and always
#' while a mutation holds the session lock. Releasing that lock clears the
#' whole memo, so nothing gitneighbr changes itself is ever served stale.
#' A TTL of 0 turns the memo off.
#'
#' @name poll-cache
#' @noRd
NULL

#' The memo environment of a session (created on first use)
#' @noRd
.poll_cache <- function(session_state) {
  if (!is.environment(session_state)) {
    return(NULL)
  }
  if (!is.environment(session_state$poll_cache)) {
    session_state$poll_cache <- new.env(parent = emptyenv())
  }
  session_state$poll_cache
}

#' Forget everything the poll memo holds
#' @noRd
.poll_cache_clear <- function(session_state) {
  if (is.environment(session_state) && is.environment(session_state$poll_cache)) {
    rm(list = ls(session_state$poll_cache, all.names = TRUE), envir = session_state$poll_cache)
  }
  invisible(NULL)
}

#' Look up or compute one memo entry
#'
#' @param key Anything `identical()` can compare; the entry is reused only
#'   while it is unchanged.
#' @return A list with `value` and `fresh` (`TRUE` when `compute()` ran).
#' @noRd
.poll_memo_entry <- function(session_state, name, key, compute) {
  cache <- .poll_cache(session_state)
  ttl <- getOption("gitneighbr.poll_cache_ttl", 60)
  if (is.null(cache) || !is.numeric(ttl) || ttl <= 0 || isTRUE(session_state$mutation_lock)) {
    return(list(value = compute(), fresh = TRUE))
  }
  now <- as.numeric(Sys.time())
  entry <- cache[[name]]
  if (!is.null(entry) && identical(entry$key, key) && (now - entry$at) < ttl) {
    return(list(value = entry$value, fresh = FALSE))
  }
  value <- compute()
  cache[[name]] <- list(key = key, value = value, at = now)
  list(value = value, fresh = TRUE)
}

#' Look up or compute one memo entry, returning only the value
#' @noRd
.poll_memo <- function(session_state, name, key, compute) {
  .poll_memo_entry(session_state, name, key, compute)$value
}

#' Cheap signatures of files and folders (no Git process)
#'
#' One string per path: whether it exists, is a folder or a link (and the
#' link's target), its size and its modification time to the microsecond.
#' @noRd
.file_sigs <- function(paths) {
  paths <- as.character(paths)
  if (length(paths) == 0L) {
    return(character())
  }
  info <- suppressWarnings(file.info(paths, extra_cols = FALSE))
  link <- suppressWarnings(Sys.readlink(paths))
  paste(
    paths,
    ifelse(is.na(info$size), "-", ifelse(info$isdir, "d", "f")),
    ifelse(is.na(link), "", link),
    ifelse(is.na(info$size), "", format(info$size, scientific = FALSE)),
    ifelse(is.na(info$mtime), "", sprintf("%.6f", as.numeric(info$mtime))),
    sep = "|"
  )
}

#' The repository's Git folders, found the way Git finds them
#'
#' Reads `<repo>/.git` (a folder, or a `gitdir:` file for a linked worktree
#' or submodule) and `commondir` without starting Git.
#' @return A list with `git_dir` and `common_dir`, or `NULL` when there is
#'   no readable `.git` here.
#' @noRd
.git_dirs <- function(repo_root) {
  dotgit <- file.path(repo_root, ".git")
  is_dir <- file.info(dotgit, extra_cols = FALSE)$isdir
  if (is.na(is_dir)) {
    return(NULL)
  }
  git_dir <- if (isTRUE(is_dir)) {
    dotgit
  } else {
    line <- tryCatch(readLines(dotgit, n = 1L, warn = FALSE), error = function(e) character())
    if (length(line) != 1L || !startsWith(line, "gitdir: ")) {
      return(NULL)
    }
    target <- trimws(sub("^gitdir: ", "", line))
    if (fs::is_absolute_path(target)) target else file.path(repo_root, target)
  }
  if (!dir.exists(git_dir)) {
    return(NULL)
  }
  common_dir <- git_dir
  commondir_file <- file.path(git_dir, "commondir")
  if (file.exists(commondir_file)) {
    target <- tryCatch(trimws(readLines(commondir_file, n = 1L, warn = FALSE)), error = function(e) character())
    if (length(target) == 1L && nzchar(target)) {
      common_dir <- if (fs::is_absolute_path(target)) target else file.path(git_dir, target)
    }
  }
  list(git_dir = git_dir, common_dir = common_dir)
}

#' Signature of what decides whether a folder is a usable repository
#'
#' Covers what `.managed_repo_check()` and `.git_repo_kind()` read, apart
#' from attributes files in the working tree (see
#' `.attributes_fingerprint()`).
#' @noRd
.repo_fingerprint <- function(repo_root) {
  dotgit <- file.path(repo_root, ".git")
  dirs <- .git_dirs(repo_root)
  pointer <- if (!is.null(dirs) && !identical(dirs$git_dir, dotgit)) dirs$git_dir
  # The `.git` folder's own timestamp moves whenever Git writes its index
  # (via a lock file), so only its kind and any link target count; a `.git`
  # file (a pointer) counts in full.
  dotgit_sig <- .file_sigs(dotgit)
  if (!is.null(dirs) && identical(dirs$git_dir, dotgit)) {
    dotgit_sig <- sub("\\|[^|]*\\|[^|]*$", "", dotgit_sig)
  }
  c(
    dotgit_sig,
    .file_sigs(c(file.path(repo_root, ".gitmodules"), file.path(dotgit, .MANAGED_GIT_DIR_REFUSED))),
    pointer,
    if (!is.null(dirs)) {
      .file_sigs(c(
        file.path(dirs$common_dir, c("config", "info/attributes")),
        file.path(dirs$git_dir, "config.worktree")
      ))
    }
  )
}

#' The Git configuration files outside the repository that this Git reads
#' @noRd
.outside_config_files <- function(git_bin) {
  managed <- attr(git_bin, "gitneighbr_managed", exact = TRUE)
  if (!is.null(managed)) {
    return(unname(managed$env[c("GIT_CONFIG_GLOBAL", "GIT_CONFIG_SYSTEM")]))
  }
  global <- Sys.getenv("GIT_CONFIG_GLOBAL")
  xdg <- Sys.getenv("XDG_CONFIG_HOME")
  c(
    if (nzchar(global)) global else c(path.expand("~/.gitconfig"), file.path(if (nzchar(xdg)) xdg else path.expand("~/.config"), "git", "config")),
    Sys.getenv("GIT_CONFIG_SYSTEM")
  )
}

#' Signature of every configuration file whose values a poll reads
#' @noRd
.config_fingerprint <- function(repo_root, git_bin) {
  outside <- .outside_config_files(git_bin)
  c(.repo_fingerprint(repo_root), .file_sigs(outside[nzchar(outside)]))
}

#' Signature of the local tags
#' @noRd
.tags_fingerprint <- function(repo_root) {
  dirs <- .git_dirs(repo_root)
  if (is.null(dirs)) {
    return(NULL)
  }
  .file_sigs(file.path(dirs$common_dir, c("packed-refs", "refs/tags")))
}

#' Signature of the attributes files a status lists, and the commit
#'
#' A `.gitattributes` file that is new, edited, staged or deleted shows up
#' in `git status`; one that arrives with a different commit changes
#' `branch.oid`. Either changes this signature.
#' @param fields `git status --porcelain=v2 -z --branch` fields.
#' @noRd
.attributes_fingerprint <- function(repo_root, fields) {
  if (is.null(fields)) {
    return(NULL)
  }
  entries <- .status_entries_from_fields(fields)
  paths <- unique(unlist(lapply(entries, function(e) c(e$path, e$old_path)), use.names = FALSE))
  paths <- paths[grepl("(^|/)\\.gitattributes$", paths)]
  c(fields[startsWith(fields, "# branch.oid ")], paths, .file_sigs(file.path(repo_root, paths)))
}
