#' Repository configuration keys a managed session accepts
#'
#' Matched against `git config --local --list` keys (section and key in
#' lower case, as Git prints them). Anything else makes the repository
#' "unsupported" in a managed session: the safe answer to configuration we
#' haven't reviewed is to do nothing. Settings that `.managed_git()`
#' overrides on every call (hooks, fsmonitor, pager, signing) are accepted
#' because they can no longer take effect.
#' @noRd
.MANAGED_CONFIG_ALLOWED <- c(
  "^core\\.(repositoryformatversion|filemode|bare|logallrefupdates|ignorecase|precomposeunicode|symlinks|autocrlf|eol|safecrlf|longpaths|quotepath|checkstat|trustctime|untrackedcache|commentchar|whitespace|hookspath|fsmonitor|pager|editor)$",
  "^remote\\.[^=]+\\.(url|pushurl|fetch|tagopt|prune)$",
  "^branch\\.[^=]+\\.(remote|merge|description|pushremote)$",
  "^user\\.(name|email|signingkey)$",
  "^init\\.defaultbranch$",
  "^pull\\.(ff|rebase)$",
  "^push\\.(default|autosetupremote)$",
  "^extensions\\.objectformat$",
  "^(commit|tag)\\.gpgsign$",
  "^gpg\\.format$",
  "^color\\.[a-z.]+$",
  "^i18n\\.(commitencoding|logoutputencoding)$",
  "^gui\\.[a-z]+$"
)

#' Files inside `.git/` that make Git read objects, refs or history from
#' elsewhere, or rewrite what history looks like
#' @noRd
.MANAGED_GIT_DIR_REFUSED <- c("commondir", "gitdir", "objects/info/alternates", "objects/info/http-alternates", "info/grafts", "modules")

#' Classify the exact folder a managed session serves
#'
#' A managed session acts only on the folder it was given, never on a
#' repository above it, and only when nothing in the repository's own
#' configuration could make Git run another program or reach outside the
#' folder.
#'
#' @return A list with `state` (`"worktree"`, `"not_a_repository"`,
#'   `"enclosing_repository"` or `"unsupported"`), and for anything but a
#'   worktree a `code`, a plain-language `message` and an optional `detail`
#'   (a setting or file name, never a value).
#' @noRd
.managed_repo_check <- function(repo_root, git_bin) {
  unsupported <- function(code, message, detail = NULL) {
    list(state = "unsupported", code = code, message = message, detail = detail)
  }
  run <- function(...) {
    tryCatch(
      .git_exec(git_bin, c("-C", repo_root, ...), error_on_status = FALSE, timeout = 10),
      error = function(e) list(status = -1L, stdout = "", stderr = "")
    )
  }

  dotgit <- file.path(repo_root, ".git")
  link <- Sys.readlink(dotgit)
  # Sys.readlink() doesn't report every kind of link (Windows junctions), so
  # also require .git to resolve to this folder's own .git.
  moved <- dir.exists(dotgit) && !identical(.real_path(dotgit), file.path(.real_path(repo_root), ".git"))
  if ((!is.na(link) && nzchar(link)) || moved) {
    return(unsupported("GIT_DIR_LINK", "This folder's .git is a link to somewhere else, so it can't be used here."))
  }
  is_dir <- file.info(dotgit)$isdir

  if (is.na(is_dir)) {
    top <- run("rev-parse", "--show-toplevel")
    if (identical(top$status, 0L)) {
      return(list(
        state = "enclosing_repository", code = "ENCLOSING_REPOSITORY",
        message = paste(
          "This folder is inside a Git project that starts in a folder above it.",
          "Version history here only works on this folder's own Git project, so nothing can be changed."
        )
      ))
    }
    bare <- run("rev-parse", "--is-bare-repository")
    if (identical(bare$status, 0L) && identical(trimws(bare$stdout), "true")) {
      return(unsupported("BARE_REPOSITORY", "This folder is a bare Git repository (it has no working files)."))
    }
    return(list(state = "not_a_repository", code = "NOT_REPOSITORY", message = "This folder isn't a Git project yet."))
  }
  if (!isTRUE(is_dir)) {
    return(unsupported(
      "GIT_FILE",
      "This folder's .git is a file pointing to a repository elsewhere (a linked worktree or submodule), so it can't be used here."
    ))
  }

  top <- run("rev-parse", "--show-toplevel")
  if (!identical(top$status, 0L)) {
    return(unsupported("GIT_DIR_INVALID", "Git can't read this folder's .git directory."))
  }
  if (!identical(.real_path(trimws(top$stdout)), .real_path(repo_root))) {
    return(unsupported("WORKTREE_ELSEWHERE", "This repository's files are configured to live in another folder.", "core.worktree"))
  }
  for (entry in .MANAGED_GIT_DIR_REFUSED) {
    if (file.exists(file.path(dotgit, entry))) {
      return(unsupported(
        "GIT_DIR_REFERENCE",
        "This repository reads part of its history from somewhere else, which isn't supported here.",
        file.path(".git", entry)
      ))
    }
  }
  if (file.exists(file.path(repo_root, ".gitmodules"))) {
    return(unsupported("SUBMODULES", "This repository contains other repositories (submodules), which aren't supported here.", ".gitmodules"))
  }

  listed <- run("config", "--local", "--list", "--name-only")
  if (!identical(listed$status, 0L)) {
    return(unsupported("CONFIG_UNREADABLE", "Git can't read this repository's settings."))
  }
  for (key in .config_keys(listed$stdout)) {
    if (!any(vapply(.MANAGED_CONFIG_ALLOWED, grepl, logical(1), x = key, perl = TRUE))) {
      return(unsupported(
        "UNSUPPORTED_CONFIGURATION",
        "This repository has a Git setting that isn't supported here, so nothing can be changed.",
        .config_key_display(key)
      ))
    }
  }
  bare <- run("config", "--local", "--get", "core.bare")
  if (identical(trimws(bare$stdout), "true")) {
    return(unsupported("BARE_REPOSITORY", "This folder is a bare Git repository (it has no working files)."))
  }

  attributes <- .attributes_files(repo_root, git_bin)
  for (file in attributes) {
    lines <- tryCatch(readLines(file.path(repo_root, file), warn = FALSE), error = function(e) character())
    lines <- sub("#.*$", "", lines)
    if (any(grepl("(^|\\s)filter=", lines, perl = TRUE))) {
      return(unsupported(
        "FILTER_ATTRIBUTE",
        paste(
          "This repository changes files as they are saved (a Git filter such as Git LFS or encryption).",
          "Saving snapshots here without it could store the wrong content, so nothing can be changed."
        ),
        file
      ))
    }
  }

  list(state = "worktree")
}

#' Config keys from `git config --list --name-only` output (one per line;
#' a subsection name can't contain a newline)
#' @noRd
.config_keys <- function(stdout) {
  keys <- strsplit(stdout, "\n", fixed = TRUE)[[1]]
  unique(keys[nzchar(keys)])
}

#' Show a config key without any user-chosen part that might be sensitive
#'
#' `remote.<name>.x` and `branch.<name>.x` carry names the user chose; the
#' section and key are enough to act on.
#' @noRd
.config_key_display <- function(key) {
  parts <- strsplit(key, ".", fixed = TRUE)[[1]]
  if (length(parts) > 2L) {
    paste(parts[[1]], "<name>", parts[[length(parts)]], sep = ".")
  } else {
    key
  }
}

#' Every attributes file Git would read for this repository
#' @noRd
.attributes_files <- function(repo_root, git_bin) {
  listed <- tryCatch(
    .git_exec(
      git_bin,
      c(
        "-C", repo_root, "ls-files", "--cached", "--others", "--exclude-standard",
        "--", ".gitattributes", ":(glob)**/.gitattributes"
      ),
      error_on_status = FALSE, timeout = 15
    ),
    error = function(e) list(status = -1L, stdout = "")
  )
  files <- if (identical(listed$status, 0L)) strsplit(listed$stdout, "\n", fixed = TRUE)[[1]] else character()
  files <- unique(files[nzchar(files)])
  if (file.exists(file.path(repo_root, ".git", "info", "attributes"))) {
    files <- c(files, ".git/info/attributes")
  }
  files
}

#' A path's canonical form, or the path itself if it can't be resolved
#' @noRd
.real_path <- function(path) {
  tryCatch(as.character(fs::path_real(path)), error = function(e) path)
}
