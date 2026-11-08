#' Send and Update with a full, preview-bound history check
#'
#' The `remote` feature's routes (see `.build_api()`):
#'
#' 1. **Check destination** fetches the destination branch only, on request.
#' 2. A **preview** lists every commit and changed path that would leave
#'    (Send) or arrive (Update), and is bound to the exact local commit,
#'    destination address and destination commit it was computed from.
#' 3. **Send** pushes exactly the previewed commit; **Update** fast-forwards
#'    to exactly the previewed commit. Anything that changed in between
#'    refuses the action, and nothing is ever forced, merged or rebased.
#'
#' Only HTTPS addresses are used. A token the user enters is kept in memory
#' and given to Git per command as an `http.<address>.extraHeader` setting
#' through `GIT_CONFIG_*` environment variables -- never on a command line,
#' on disk, or in any response.
#' @name managed-remote
#' @noRd
NULL

#' How long a destination check and a preview stay valid
#' @noRd
.REMOTE_CHECK_TTL <- 30 * 60
.PREVIEW_TTL <- 10 * 60

#' The most commits a preview lists before refusing as too large to review
#' @noRd
.PREVIEW_MAX_COMMITS <- 2000L

#' Is `url` a plain HTTPS address (no embedded user name or password)?
#' @noRd
.is_https_url <- function(url) {
  is.character(url) && length(url) == 1L && !is.na(url) &&
    grepl("^https://[A-Za-z0-9.-]+(:[0-9]+)?/[^\\s@]+$", url, perl = TRUE)
}

#' The host part of an HTTPS address
#' @noRd
.url_host <- function(url) {
  sub("^https://([^/:]+).*$", "\\1", url)
}

#' The current branch and where it would be sent
#'
#' The branch's configured upstream if there is one, otherwise the same
#' branch name on `origin`.
#' @return A list (`ok`, `remote`, `branch`, `dest`, `url`, `host`) or an
#'   error list.
#' @noRd
.managed_destination <- function(repo_root, git_bin) {
  status <- .git_status(repo_root, git_bin)
  if (isTRUE(status$detached) || is.null(status$branch)) {
    return(list(ok = FALSE, code = "DETACHED_HEAD", message = "Switch to a branch before sending or getting updates.", recoverable = FALSE))
  }
  upstream <- .git_upstream_info(repo_root, git_bin, status$branch)
  remote <- upstream$remote %||% "origin"
  dest <- upstream$branch %||% status$branch
  url_result <- .git_exec(git_bin, c("-C", repo_root, "config", "--get", paste0("remote.", remote, ".url")), error_on_status = FALSE, timeout = 5)
  url <- trimws(url_result$stdout)
  if (!identical(url_result$status, 0L) || !nzchar(url)) {
    return(list(ok = FALSE, code = "NO_REMOTE", message = "This project isn't connected to a shared copy yet.", recoverable = TRUE))
  }
  if (!.is_https_url(url)) {
    return(list(
      ok = FALSE, code = "REMOTE_NOT_HTTPS",
      message = "Only HTTPS addresses (starting with https://) can be used here. Ask for the repository's HTTPS address.",
      recoverable = FALSE
    ))
  }
  push_url <- .git_exec(git_bin, c("-C", repo_root, "config", "--get", paste0("remote.", remote, ".pushurl")), error_on_status = FALSE, timeout = 5)
  if (identical(push_url$status, 0L) && !identical(trimws(push_url$stdout), url)) {
    return(list(ok = FALSE, code = "REMOTE_PUSHURL", message = "This project sends to a different address than it reads from, which isn't supported here.", recoverable = FALSE))
  }
  list(ok = TRUE, remote = remote, branch = status$branch, dest = dest, url = url, host = .url_host(url))
}

#' Environment that hands the session's token to Git for one address
#' @noRd
.remote_auth_env <- function(session_state, url) {
  auth <- session_state$remote_auth
  if (is.null(auth) || !identical(auth$host, .url_host(url))) {
    return(NULL)
  }
  basic <- openssl::base64_encode(charToRaw(paste0(auth$username, ":", auth$token)))
  c(
    GIT_CONFIG_COUNT = "1",
    GIT_CONFIG_KEY_0 = paste0("http.", url, ".extraHeader"),
    GIT_CONFIG_VALUE_0 = paste("Authorization: Basic", basic)
  )
}

#' Remove the session token (and its encodings) from Git output
#' @noRd
.redact_token <- function(text, session_state) {
  auth <- session_state$remote_auth
  if (is.null(auth) || is.null(text)) {
    return(text)
  }
  basic <- openssl::base64_encode(charToRaw(paste0(auth$username, ":", auth$token)))
  for (secret in c(basic, auth$token)) {
    text <- gsub(secret, "***", text, fixed = TRUE)
  }
  text
}

#' The commit a ref points at, or `""`
#' @noRd
.rev <- function(repo_root, git_bin, ref) {
  res <- .git_exec(git_bin, c("-C", repo_root, "rev-parse", "--verify", "-q", paste0(ref, "^{commit}")), error_on_status = FALSE, timeout = 10)
  if (identical(res$status, 0L)) trimws(res$stdout) else ""
}

#' Is `ancestor` an ancestor of (or equal to) `descendant`?
#' @noRd
.is_ancestor <- function(repo_root, git_bin, ancestor, descendant) {
  res <- .git_exec(git_bin, c("-C", repo_root, "merge-base", "--is-ancestor", ancestor, descendant), error_on_status = FALSE, timeout = 30)
  identical(res$status, 0L)
}

#' Check the destination: fetch its branch, on request only
#' @noRd
.managed_remote_check <- function(repo_root, git_bin, session_state) {
  dest <- .managed_destination(repo_root, git_bin)
  if (!isTRUE(dest$ok)) {
    return(dest)
  }
  tracking_ref <- paste0("refs/remotes/", dest$remote, "/", dest$dest)
  args <- c("-C", repo_root, "fetch", "--no-tags", "--no-write-fetch-head", dest$remote, paste0("+refs/heads/", dest$dest, ":", tracking_ref))
  result <- .git_exec(git_bin, args, error_on_status = FALSE, timeout = 120, env = .remote_auth_env(session_state, dest$url))
  result$stderr <- .redact_token(result$stderr, session_state)
  if (!identical(result$status, 0L)) {
    if (grepl("couldn't find remote ref", result$stderr, fixed = TRUE)) {
      # The destination branch doesn't exist yet: everything is new. Drop a
      # stale tracking ref so it can't hide commits from the preview.
      .git_exec(git_bin, c("-C", repo_root, "update-ref", "-d", tracking_ref), error_on_status = FALSE, timeout = 10)
    } else {
      code <- .classify_fetch_failure(result$stderr)
      return(list(
        ok = FALSE, code = code,
        message = switch(code,
          AUTH_REQUIRED = paste0("The shared copy at ", dest$host, " needs an access token, or the token was refused."),
          REMOTE_UNREACHABLE = paste0("Couldn't reach ", dest$host, ". Check the network connection and try again."),
          "Checking the destination failed."
        ),
        recoverable = TRUE, advanced = .advanced_block(args, result)
      ))
    }
  }
  session_state$remote_check <- list(
    remote = dest$remote, dest = dest$dest, url = dest$url,
    tracking = .rev(repo_root, git_bin, tracking_ref), at = Sys.time()
  )
  list(ok = TRUE, exists = nzchar(session_state$remote_check$tracking))
}

#' The destination check, if it is still valid for `dest`
#' @noRd
.valid_remote_check <- function(session_state, dest) {
  check <- session_state$remote_check
  if (is.null(check) || !identical(check$url, dest$url) || !identical(check$remote, dest$remote) ||
    !identical(check$dest, dest$dest) || difftime(Sys.time(), check$at, units = "secs") > .REMOTE_CHECK_TTL) {
    return(NULL)
  }
  check
}

#' Every path a commit changes, with sizes and binary flags
#'
#' Compared with its first parent (or nothing, for a root commit), with
#' renames shown as a delete and an add so no path is hidden.
#' @noRd
.commit_files <- function(repo_root, git_bin, sha, parent = NULL) {
  base <- if (is.null(parent)) c("--root", sha) else c(parent, sha)
  raw <- .split_nul(.run_git_raw(git_bin, c("-C", repo_root, "diff-tree", "--no-commit-id", "-r", "--no-renames", "--raw", "-z", base)))
  numstat <- .split_nul(.run_git_raw(git_bin, c("-C", repo_root, "diff-tree", "--no-commit-id", "-r", "--no-renames", "--numstat", "-z", base)))
  binary <- character()
  for (field in numstat) {
    parts <- strsplit(field, "\t", fixed = TRUE)[[1]]
    if (length(parts) == 3L && identical(parts[[1]], "-")) binary <- c(binary, parts[[3]])
  }
  files <- list()
  i <- 1L
  while (i < length(raw)) {
    meta <- raw[[i]]
    path <- raw[[i + 1L]]
    i <- i + 2L
    m <- regmatches(meta, regexec("^:(\\d+) (\\d+) ([0-9a-f]+) ([0-9a-f]+) ([A-Z])", meta))[[1]]
    if (length(m) != 6L) next
    files[[length(files) + 1L]] <- list(path = path, status = m[[6]], new_mode = m[[3]], oid = m[[5]], binary = path %in% binary)
  }
  oids <- unique(vapply(Filter(function(f) !grepl("^0+$", f$oid), files), `[[`, character(1), "oid"))
  sizes <- .blob_sizes(repo_root, git_bin, oids)
  lapply(files, function(f) {
    list(path = f$path, status = f$status, binary = f$binary, size = if (f$oid %in% names(sizes)) sizes[[f$oid]] else 0, symlink = identical(f$new_mode, "120000"))
  })
}

#' Sizes of blobs by object id, in one Git call
#' @noRd
.blob_sizes <- function(repo_root, git_bin, oids) {
  if (length(oids) == 0L) {
    return(numeric())
  }
  input <- tempfile("gitneighbr-oids-")
  on.exit(unlink(input), add = TRUE)
  writeLines(oids, input)
  res <- .git_exec(git_bin, c("-C", repo_root, "cat-file", "--batch-check=%(objectname) %(objectsize)"),
    stdin = input, error_on_status = FALSE, timeout = 60
  )
  lines <- strsplit(trimws(res$stdout), "\n", fixed = TRUE)[[1]]
  parts <- strsplit(lines[nzchar(lines)], " ", fixed = TRUE)
  stats::setNames(vapply(parts, function(p) as.numeric(p[[2]]), numeric(1)), vapply(parts, `[[`, character(1), 1L))
}

#' Describe a list of commits for a preview
#' @noRd
.describe_commits <- function(repo_root, git_bin, shas, policy) {
  lapply(shas, function(sha) {
    info <- .git_exec(git_bin, c("-C", repo_root, "show", "-s", "--format=%P%n%an%n%aI%n%s", sha), error_on_status = TRUE, timeout = 15)
    lines <- strsplit(info$stdout, "\n", fixed = TRUE)[[1]]
    parents <- strsplit(lines[[1]], " ", fixed = TRUE)[[1]]
    parents <- parents[nzchar(parents)]
    files <- .commit_files(repo_root, git_bin, sha, if (length(parents) > 0L) parents[[1]])
    files <- lapply(files, function(f) {
      f$review <- as.list(vapply(Filter(function(r) .glob_match(f$path, r$globs), policy$review %||% list()), `[[`, character(1), "id"))
      f$blocked <- .glob_match(f$path, policy$send_blocked %||% character())
      f
    })
    list(
      sha = sha, short = substr(sha, 1, 10), author = lines[[2]] %||% "", date = lines[[3]] %||% "",
      summary = paste(lines[-(1:3)], collapse = "\n"), merge = length(parents) > 1L, files = files
    )
  })
}

#' Totals for a list of described commits
#' @noRd
.preview_totals <- function(commits) {
  files <- unlist(lapply(commits, function(c) lapply(c$files, function(f) list(f))), recursive = FALSE)
  paths <- unique(vapply(files, function(f) f[[1]]$path, character(1)))
  bytes <- sum(vapply(files, function(f) as.numeric(f[[1]]$size), numeric(1)))
  list(commits = length(commits), paths = length(paths), bytes = bytes)
}

#' Build the Send preview
#' @noRd
.managed_send_preview <- function(repo_root, git_bin, session_state) {
  dest <- .managed_destination(repo_root, git_bin)
  if (!isTRUE(dest$ok)) {
    return(dest)
  }
  not_inspectable <- function(message) {
    list(ok = FALSE, code = "OUTGOING_NOT_INSPECTABLE", message = message, recoverable = TRUE)
  }
  check <- .valid_remote_check(session_state, dest)
  if (is.null(check)) {
    return(not_inspectable("Check the destination first, so the list of what would be sent is complete."))
  }
  shallow <- .git_exec(git_bin, c("-C", repo_root, "rev-parse", "--is-shallow-repository"), error_on_status = FALSE, timeout = 10)
  if (identical(trimws(shallow$stdout), "true")) {
    return(not_inspectable("This project has only part of its history, so what would be sent can't be listed completely."))
  }
  head <- .rev(repo_root, git_bin, "HEAD")
  if (!nzchar(head)) {
    return(list(ok = FALSE, code = "NOTHING_TO_SEND", message = "There are no snapshots to send yet.", recoverable = TRUE))
  }
  if (nzchar(check$tracking) && !.is_ancestor(repo_root, git_bin, check$tracking, head)) {
    code <- if (.is_ancestor(repo_root, git_bin, head, check$tracking)) "REMOTE_AHEAD" else "DIVERGED"
    return(list(
      ok = FALSE, code = code,
      message = if (identical(code, "REMOTE_AHEAD")) "The shared copy has newer snapshots. Get updates first." else "This project and the shared copy have each changed; they need help to combine.",
      recoverable = FALSE
    ))
  }
  range <- if (nzchar(check$tracking)) c(head, "--not", check$tracking) else head
  listed <- .git_exec(git_bin, c("-C", repo_root, "rev-list", "--reverse", "--topo-order", paste0("--max-count=", .PREVIEW_MAX_COMMITS + 1L), range), error_on_status = FALSE, timeout = 60)
  if (!identical(listed$status, 0L)) {
    return(not_inspectable("The history to send couldn't be read completely."))
  }
  shas <- strsplit(trimws(listed$stdout), "\n", fixed = TRUE)[[1]]
  shas <- shas[nzchar(shas)]
  if (length(shas) > .PREVIEW_MAX_COMMITS) {
    return(not_inspectable("There are too many snapshots to review in one send."))
  }
  policy <- session_state$launcher_policy
  commits <- .describe_commits(repo_root, git_bin, shas, policy)
  blocked <- unique(unlist(lapply(commits, function(c) vapply(Filter(function(f) isTRUE(f$blocked), c$files), `[[`, character(1), "path"))))
  review <- unique(unlist(lapply(commits, function(c) lapply(c$files, function(f) unlist(f$review)))))

  id <- .new_operation_id()
  session_state$send_preview <- list(
    id = id, kind = "send", head = head, tracking = check$tracking, url = dest$url, remote = dest$remote,
    branch = dest$branch, dest = dest$dest, review = length(review) > 0L, blocked = length(blocked) > 0L,
    empty = length(commits) == 0L, at = Sys.time()
  )
  list(
    ok = TRUE, preview_id = id,
    destination = list(host = dest$host, address = dest$url, branch = dest$dest, remote = dest$remote, exists = nzchar(check$tracking)),
    commits = commits, totals = .preview_totals(commits),
    blocked = as.list(blocked), review = as.list(review),
    can_send = length(commits) > 0L && length(blocked) == 0L
  )
}

#' The stored preview for `id`, if still valid
#' @noRd
.valid_preview <- function(session_state, slot, id) {
  preview <- session_state[[slot]]
  if (is.null(preview) || !is.character(id) || length(id) != 1L || !identical(preview$id, id) ||
    difftime(Sys.time(), preview$at, units = "secs") > .PREVIEW_TTL) {
    return(NULL)
  }
  preview
}

#' Send exactly the previewed commit
#' @noRd
.managed_send <- function(repo_root, git_bin, session_state, body) {
  preview <- .valid_preview(session_state, "send_preview", body$preview_id)
  changed <- list(ok = FALSE, code = "STATE_CHANGED", message = "Something changed since the list was shown. Check again before sending.", recoverable = TRUE)
  if (is.null(preview)) {
    return(changed)
  }
  if (!isTRUE(body$confirm)) {
    return(list(ok = FALSE, code = "CONFIRMATION_REQUIRED", message = "Confirm that you've reviewed what will be sent.", recoverable = TRUE))
  }
  if (preview$review && !isTRUE(body$confirm_review)) {
    return(list(ok = FALSE, code = "CONFIRMATION_REQUIRED", message = "Confirm the files marked for extra review.", recoverable = TRUE))
  }
  if (preview$blocked || preview$empty) {
    return(list(ok = FALSE, code = "SEND_BLOCKED", message = "This can't be sent from here.", recoverable = FALSE))
  }
  dest <- .managed_destination(repo_root, git_bin)
  check <- if (isTRUE(dest$ok)) .valid_remote_check(session_state, dest)
  tracking_ref <- paste0("refs/remotes/", preview$remote, "/", preview$dest)
  if (!isTRUE(dest$ok) || is.null(check) || !identical(dest$url, preview$url) || !identical(dest$dest, preview$dest) ||
    !identical(.rev(repo_root, git_bin, "HEAD"), preview$head) || !identical(.rev(repo_root, git_bin, tracking_ref), preview$tracking)) {
    return(changed)
  }

  args <- c("-C", repo_root, "push", "--porcelain", "--no-follow-tags", "--no-verify", preview$remote, paste0(preview$head, ":refs/heads/", preview$dest))
  result <- .git_exec(git_bin, args, error_on_status = FALSE, timeout = 300, env = .remote_auth_env(session_state, preview$url))
  result$stderr <- .redact_token(result$stderr, session_state)
  session_state$send_preview <- NULL
  session_state$remote_check <- NULL
  if (!identical(result$status, 0L)) {
    code <- .classify_push_failure(paste(result$stderr, result$stdout))
    return(list(
      ok = FALSE, code = code,
      message = switch(code,
        AUTH_REQUIRED = "The shared copy refused the access token.",
        REMOTE_AHEAD = "The shared copy changed while sending. Nothing was sent; check again.",
        REMOTE_UNREACHABLE = "Couldn't reach the shared copy. Nothing was sent.",
        "Sending failed. Nothing on this computer changed."
      ),
      recoverable = TRUE, advanced = .advanced_block(args, result)
    ))
  }
  # Remember where this branch is sent, if it wasn't set.
  if (is.null(.git_upstream_info(repo_root, git_bin, preview$branch))) {
    .git_exec(git_bin, c("-C", repo_root, "config", paste0("branch.", preview$branch, ".remote"), preview$remote), error_on_status = FALSE, timeout = 5)
    .git_exec(git_bin, c("-C", repo_root, "config", paste0("branch.", preview$branch, ".merge"), paste0("refs/heads/", preview$dest)), error_on_status = FALSE, timeout = 5)
  }
  list(ok = TRUE, sha = preview$head, branch = preview$dest, host = .url_host(preview$url))
}

#' Build the Update preview
#' @noRd
.managed_receive_preview <- function(repo_root, git_bin, session_state) {
  dest <- .managed_destination(repo_root, git_bin)
  if (!isTRUE(dest$ok)) {
    return(dest)
  }
  check <- .valid_remote_check(session_state, dest)
  if (is.null(check)) {
    return(list(ok = FALSE, code = "INCOMING_NOT_INSPECTABLE", message = "Check the destination first, so the list of updates is complete.", recoverable = TRUE))
  }
  head <- .rev(repo_root, git_bin, "HEAD")
  if (!nzchar(check$tracking) || identical(head, check$tracking) || (nzchar(head) && .is_ancestor(repo_root, git_bin, check$tracking, head))) {
    return(list(ok = TRUE, preview_id = NULL, commits = list(), touched = list(), blocked = list(), can_update = FALSE, up_to_date = TRUE))
  }
  if (nzchar(head) && !.is_ancestor(repo_root, git_bin, head, check$tracking)) {
    return(list(ok = FALSE, code = "DIVERGED", message = "This project and the shared copy have each changed; they need help to combine. Nothing was changed.", recoverable = FALSE))
  }
  range <- if (nzchar(head)) c(check$tracking, "--not", head) else check$tracking
  listed <- .git_exec(git_bin, c("-C", repo_root, "rev-list", "--reverse", "--topo-order", paste0("--max-count=", .PREVIEW_MAX_COMMITS + 1L), range), error_on_status = TRUE, timeout = 60)
  shas <- strsplit(trimws(listed$stdout), "\n", fixed = TRUE)[[1]]
  shas <- shas[nzchar(shas)]
  if (length(shas) > .PREVIEW_MAX_COMMITS) {
    return(list(ok = FALSE, code = "INCOMING_NOT_INSPECTABLE", message = "There are too many updates to review at once.", recoverable = FALSE))
  }
  policy <- session_state$launcher_policy
  commits <- .describe_commits(repo_root, git_bin, shas, policy)
  touched <- if (nzchar(head)) {
    vapply(.commit_files(repo_root, git_bin, check$tracking, head), `[[`, character(1), "path")
  } else {
    vapply(.commit_files(repo_root, git_bin, check$tracking), `[[`, character(1), "path")
  }
  blocked <- list()
  protected <- Filter(function(p) .glob_match(p, policy$protected %||% character()), touched)
  for (p in protected) blocked[[length(blocked) + 1L]] <- list(path = p, reason = "protected")
  local <- vapply(.git_status_entries(repo_root, git_bin), function(e) e$path, character(1))
  for (p in intersect(touched, local)) blocked[[length(blocked) + 1L]] <- list(path = p, reason = "local_changes")

  id <- .new_operation_id()
  session_state$receive_preview <- list(id = id, head = head, tracking = check$tracking, url = dest$url, remote = dest$remote, dest = dest$dest, blocked = length(blocked) > 0L, at = Sys.time())
  list(
    ok = TRUE, preview_id = id, commits = commits, touched = as.list(touched), blocked = blocked,
    can_update = length(blocked) == 0L, up_to_date = FALSE
  )
}

#' Fast-forward to exactly the previewed commit
#' @noRd
.managed_receive <- function(repo_root, git_bin, session_state, body) {
  preview <- .valid_preview(session_state, "receive_preview", body$preview_id)
  changed <- list(ok = FALSE, code = "STATE_CHANGED", message = "Something changed since the list was shown. Check again before getting updates.", recoverable = TRUE)
  if (is.null(preview)) {
    return(changed)
  }
  if (!isTRUE(body$confirm)) {
    return(list(ok = FALSE, code = "CONFIRMATION_REQUIRED", message = "Confirm that you've reviewed the updates.", recoverable = TRUE))
  }
  if (preview$blocked) {
    return(list(ok = FALSE, code = "UPDATE_BLOCKED", message = "These updates can't be applied here.", recoverable = FALSE))
  }
  dest <- .managed_destination(repo_root, git_bin)
  tracking_ref <- paste0("refs/remotes/", preview$remote, "/", preview$dest)
  if (!isTRUE(dest$ok) || !identical(dest$url, preview$url) || !identical(.rev(repo_root, git_bin, "HEAD"), preview$head) ||
    !identical(.rev(repo_root, git_bin, tracking_ref), preview$tracking)) {
    return(changed)
  }
  # Re-check, at the moment of acting, that no incoming path has local work.
  touched <- vapply(.commit_files(repo_root, git_bin, preview$tracking, if (nzchar(preview$head)) preview$head), `[[`, character(1), "path")
  local <- vapply(.git_status_entries(repo_root, git_bin), function(e) e$path, character(1))
  if (length(intersect(touched, local)) > 0L) {
    return(changed)
  }
  args <- c("-C", repo_root, "merge", "--ff-only", "--no-edit", "--no-verify", preview$tracking)
  result <- .git_exec(git_bin, args, error_on_status = FALSE, timeout = 120)
  session_state$receive_preview <- NULL
  if (!identical(result$status, 0L)) {
    return(list(ok = FALSE, code = "COMMAND_FAILED", message = "Getting updates failed; nothing was changed.", recoverable = TRUE, advanced = .advanced_block(args, result)))
  }
  list(ok = TRUE, sha = preview$tracking, count = length(touched))
}

#' Connect this project to a shared copy (HTTPS address only)
#' @noRd
.managed_remote_connect <- function(repo_root, git_bin, url) {
  url <- trimws(as.character(url %||% "")[[1]])
  if (!.is_https_url(url)) {
    return(list(ok = FALSE, code = "INVALID_REMOTE_URL", message = "Enter the repository's HTTPS address (https://host/owner/name), with no user name or password in it.", recoverable = TRUE))
  }
  existing <- .git_exec(git_bin, c("-C", repo_root, "config", "--get", "remote.origin.url"), error_on_status = FALSE, timeout = 5)
  if (identical(existing$status, 0L)) {
    if (identical(trimws(existing$stdout), url)) {
      return(list(ok = TRUE, url = url))
    }
    return(list(ok = FALSE, code = "REMOTE_ALREADY_SET", message = "This project is already connected to a different address.", recoverable = FALSE))
  }
  args <- c("-C", repo_root, "remote", "add", "--", "origin", url)
  result <- .git_exec(git_bin, args, error_on_status = FALSE, timeout = 10)
  if (!identical(result$status, 0L)) {
    return(list(ok = FALSE, code = "COMMAND_FAILED", message = "Couldn't save the address.", recoverable = TRUE, advanced = .advanced_block(args, result)))
  }
  list(ok = TRUE, url = url)
}

#' What the page shows about the shared copy (never the token)
#' @noRd
.managed_remote_status <- function(repo_root, git_bin, session_state, status) {
  if (is.null(status) || isTRUE(status$detached)) {
    return(NULL)
  }
  dest <- .managed_destination(repo_root, git_bin)
  check <- if (isTRUE(dest$ok)) .valid_remote_check(session_state, dest)
  list(
    configured = !identical(dest$code, "NO_REMOTE"),
    usable = isTRUE(dest$ok),
    problem = if (!isTRUE(dest$ok)) list(code = dest$code, message = dest$message),
    host = dest$host, address = dest$url, branch = dest$dest,
    has_token = !is.null(session_state$remote_auth) && identical(session_state$remote_auth$host, dest$host),
    checked = !is.null(check)
  )
}
