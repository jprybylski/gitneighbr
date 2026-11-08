#' Read and validate a launcher policy file
#'
#' A launcher (not the repository) describes how this folder's files should
#' be presented and protected. The file lives outside the repository, so
#' repository content can't change it. Schema (version 1):
#'
#' ```json
#' {"schema_version": 1,
#'  "groups": [{"id": "sources", "label": "Editable sources",
#'              "description": "...", "globs": ["recipe/**"]}],
#'  "default_group": {"id": "other", "label": "Other files", "description": "..."},
#'  "protected": ["data/runs/**"],
#'  "review": [{"id": "data", "label": "Study data", "message": "...", "globs": ["data/**"]}],
#'  "send_blocked": ["secrets/**"],
#'  "ignore_template": [{"rule": "OUTPUTS/", "reason": "..."}],
#'  "max_diff_bytes": 200000}
#' ```
#'
#' Globs are relative to the folder, use `/`, and support `*` (within one
#' folder), `?`, and `**` (any number of folders). A glob ending in `/`
#' matches everything under that folder.
#'
#' @return The validated policy (a list), or an error naming the problem.
#' @noRd
.read_launcher_policy <- function(path) {
  raw <- tryCatch(
    jsonlite::fromJSON(path, simplifyVector = FALSE),
    error = function(e) stop("The policy file isn't valid JSON: ", conditionMessage(e), call. = FALSE)
  )
  .validate_launcher_policy(raw)
}

#' @noRd
.validate_launcher_policy <- function(raw) {
  bad <- function(...) stop("Invalid policy file: ", ..., call. = FALSE)
  if (!is.list(raw) || is.null(names(raw))) bad("expected a JSON object.")
  known <- c("schema_version", "groups", "default_group", "protected", "review", "send_blocked", "ignore_template", "max_diff_bytes")
  unknown <- setdiff(names(raw), known)
  if (length(unknown) > 0L) bad("unknown field(s) ", paste(unknown, collapse = ", "), ".")
  if (!identical(raw$schema_version, 1L)) bad("schema_version must be 1.")

  string <- function(x, what, optional = FALSE) {
    if (is.null(x) && optional) {
      return(NULL)
    }
    if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(x) || nchar(x) > 500L) bad(what, " must be a non-empty string.")
    x
  }
  id <- function(x, what) {
    x <- string(x, what)
    if (!grepl("^[a-z][a-z0-9_-]{0,39}$", x)) bad(what, " must be a short lower-case id.")
    x
  }
  globs <- function(x, what) {
    if (is.null(x)) {
      return(character())
    }
    if (!is.list(x)) bad(what, " must be a list of globs.")
    out <- vapply(x, function(g) string(g, what), character(1))
    for (g in out) .check_glob(g, what, bad)
    out
  }
  object <- function(x, what, fields) {
    if (!is.list(x) || is.null(names(x))) bad(what, " must be an object.")
    extra <- setdiff(names(x), fields)
    if (length(extra) > 0L) bad(what, ": unknown field(s) ", paste(extra, collapse = ", "), ".")
    x
  }

  groups <- lapply(raw$groups %||% list(), function(g) {
    g <- object(g, "groups[]", c("id", "label", "description", "globs"))
    list(id = id(g$id, "groups[].id"), label = string(g$label, "groups[].label"),
      description = string(g$description, "groups[].description", optional = TRUE),
      globs = globs(g$globs, "groups[].globs"))
  })
  default_group <- if (is.null(raw$default_group)) {
    list(id = "other", label = "Other files", description = NULL)
  } else {
    d <- object(raw$default_group, "default_group", c("id", "label", "description"))
    list(id = id(d$id, "default_group.id"), label = string(d$label, "default_group.label"),
      description = string(d$description, "default_group.description", optional = TRUE))
  }
  ids <- c(vapply(groups, `[[`, character(1), "id"), default_group$id)
  if (anyDuplicated(ids)) bad("group ids must be unique.")

  review <- lapply(raw$review %||% list(), function(r) {
    r <- object(r, "review[]", c("id", "label", "message", "globs"))
    list(id = id(r$id, "review[].id"), label = string(r$label, "review[].label"),
      message = string(r$message, "review[].message"), globs = globs(r$globs, "review[].globs"))
  })
  template <- lapply(raw$ignore_template %||% list(), function(t) {
    t <- object(t, "ignore_template[]", c("rule", "reason"))
    rule <- string(t$rule, "ignore_template[].rule")
    if (grepl("[\r\n]", rule) || startsWith(rule, "#")) bad("ignore_template[].rule must be one .gitignore line.")
    list(rule = rule, reason = string(t$reason, "ignore_template[].reason"))
  })
  max_diff <- raw$max_diff_bytes %||% 200000L
  if (!is.numeric(max_diff) || length(max_diff) != 1L || max_diff < 1000 || max_diff > 50e6) bad("max_diff_bytes must be between 1000 and 50000000.")

  list(
    groups = groups,
    default_group = default_group,
    protected = globs(raw$protected, "protected"),
    send_blocked = globs(raw$send_blocked, "send_blocked"),
    review = review,
    ignore_template = template,
    max_diff_bytes = as.integer(max_diff)
  )
}

#' Refuse globs that could never mean "a path inside this folder"
#' @noRd
.check_glob <- function(glob, what, bad) {
  if (startsWith(glob, "/") || grepl("^[A-Za-z]:", glob) || grepl("\\\\", glob)) bad(what, ": '", glob, "' must be relative and use /.")
  segments <- strsplit(sub("/$", "", glob), "/", fixed = TRUE)[[1]]
  if (any(segments %in% c("", ".", ".."))) bad(what, ": '", glob, "' has an empty, . or .. part.")
  if (grepl("[][{}!]", glob)) bad(what, ": '", glob, "' uses unsupported glob syntax (only *, ? and ** are supported).")
  invisible(TRUE)
}

#' Convert a policy glob to an anchored regular expression
#' @noRd
.glob_regex <- function(glob) {
  dir_prefix <- endsWith(glob, "/")
  if (dir_prefix) glob <- paste0(glob, "**")
  out <- ""
  i <- 1L
  chars <- strsplit(glob, "")[[1]]
  n <- length(chars)
  while (i <= n) {
    ch <- chars[[i]]
    if (ch == "*" && i < n && chars[[i + 1L]] == "*") {
      if (i + 1L < n && chars[[i + 2L]] == "/") {
        out <- paste0(out, "(?:.*/)?")
        i <- i + 3L
      } else {
        out <- paste0(out, ".*")
        i <- i + 2L
      }
      next
    }
    out <- paste0(out, switch(ch,
      "*" = "[^/]*",
      "?" = "[^/]",
      gsub("([.\\\\+^$|()])", "\\\\\\1", ch)
    ))
    i <- i + 1L
  }
  paste0("^", out, "$")
}

#' Does `path` match any of `globs`?
#' @noRd
.glob_match <- function(path, globs) {
  if (length(globs) == 0L) {
    return(FALSE)
  }
  path <- gsub("\\\\", "/", path)
  any(vapply(globs, function(g) grepl(.glob_regex(g), path, perl = TRUE), logical(1)))
}

#' Add `group`, `protected` and `review` to each change
#' @noRd
.annotate_changes <- function(changes, policy) {
  if (is.null(policy)) {
    return(changes)
  }
  lapply(changes, function(change) {
    paths <- c(change$path, change$old_path)
    group <- policy$default_group$id
    for (g in policy$groups) {
      if (.glob_match(change$path, g$globs)) {
        group <- g$id
        break
      }
    }
    review <- vapply(Filter(function(r) any(vapply(paths, .glob_match, logical(1), globs = r$globs)), policy$review), `[[`, character(1), "id")
    change$group <- group
    change$protected <- any(vapply(paths, .glob_match, logical(1), globs = policy$protected))
    change$review <- as.list(review)
    change
  })
}

#' What the page needs to present a launcher policy (no globs)
#' @noRd
.policy_layout <- function(policy, repo_root) {
  if (is.null(policy)) {
    return(NULL)
  }
  strip <- function(x) x[setdiff(names(x), "globs")]
  list(
    groups = c(lapply(policy$groups, strip), list(policy$default_group)),
    review = lapply(policy$review, strip),
    ignore_template = policy$ignore_template,
    gitignore_exists = file.exists(file.path(repo_root, ".gitignore"))
  )
}

#' The first of `paths` a launcher policy protects, or `NULL`
#' @noRd
.protected_path <- function(paths, policy) {
  if (is.null(policy)) {
    return(NULL)
  }
  for (path in paths) {
    if (.glob_match(path, policy$protected)) {
      return(path)
    }
  }
  NULL
}

#' Write the chosen ignore-template rules as a new `.gitignore`
#'
#' Only rules from the launcher's template can be written, only when the
#' user picked them, and never over an existing `.gitignore`.
#' @noRd
.write_ignore_template <- function(repo_root, policy, rules) {
  rules <- unique(as.character(unlist(rules %||% list())))
  if (length(rules) == 0L) {
    return(list(ok = TRUE, written = FALSE))
  }
  template <- policy$ignore_template %||% list()
  offered <- vapply(template, `[[`, character(1), "rule")
  if (!all(rules %in% offered)) {
    return(list(ok = FALSE, code = "COMMAND_FAILED", message = "Only the suggested ignore rules can be added here.", recoverable = FALSE))
  }
  target <- file.path(repo_root, ".gitignore")
  if (file.exists(target)) {
    return(list(ok = FALSE, code = "GITIGNORE_EXISTS", message = "This folder already has a .gitignore; it was left unchanged.", recoverable = TRUE))
  }
  lines <- c("# Suggested when version history was set up. Edit freely.", "")
  for (entry in template) {
    if (entry$rule %in% rules) {
      lines <- c(lines, paste("#", entry$reason), entry$rule, "")
    }
  }
  tmp <- tempfile(tmpdir = repo_root, pattern = ".gitignore-")
  writeLines(lines, tmp, useBytes = TRUE)
  if (file.exists(target) || !file.rename(tmp, target)) {
    unlink(tmp)
    return(list(ok = FALSE, code = "GITIGNORE_EXISTS", message = "This folder already has a .gitignore; it was left unchanged.", recoverable = TRUE))
  }
  list(ok = TRUE, written = TRUE)
}
