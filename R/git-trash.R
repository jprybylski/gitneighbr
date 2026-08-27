#' Which OS-specific trash mechanism to dispatch to
#' @noRd
.trash_os_family <- function() {
  sysname <- Sys.info()[["sysname"]]
  if (identical(sysname, "Darwin")) {
    "macos"
  } else if (identical(sysname, "Windows")) {
    "windows"
  } else if (identical(sysname, "Linux")) {
    "linux"
  } else {
    "other"
  }
}

#' Move one file to macOS's Trash via Finder
#'
#' Asks Finder (not a raw filesystem move) so the item keeps its "Put Back"
#' original-location metadata like a normal Finder delete. The path is
#' passed as an `osascript` script *argument* (`argv`), never interpolated
#' into the AppleScript source text, so no quoting/escaping of the path is
#' needed and no injection is possible. Requires the user to have granted
#' the calling process Automation access to Finder; a refusal or missing
#' `osascript` is reported as unavailable rather than erroring.
#' @noRd
.trash_file_macos <- function(full_path) {
  osascript <- unname(Sys.which("osascript"))
  if (!nzchar(osascript)) {
    return(list(ok = FALSE))
  }
  script <- paste(
    "on run argv",
    "  set thePath to POSIX file (item 1 of argv)",
    "  tell application \"Finder\" to delete thePath",
    "end run",
    sep = "\n"
  )
  result <- tryCatch(
    processx::run(osascript, c("-e", script, full_path), error_on_status = FALSE, timeout = 15),
    error = function(e) NULL
  )
  list(ok = !is.null(result) && identical(result$status, 0L))
}

#' Move one file to the Windows Recycle Bin
#'
#' The path is forwarded as a trailing `powershell` argument and read back
#' as `$args[0]` inside the `-Command` script, rather than interpolated
#' into the command string, so no quoting/escaping of the path is needed.
#' @noRd
.trash_file_windows <- function(full_path) {
  powershell <- unname(Sys.which("powershell"))
  if (!nzchar(powershell)) {
    return(list(ok = FALSE))
  }
  command <- paste(
    "Add-Type -AssemblyName Microsoft.VisualBasic;",
    "[Microsoft.VisualBasic.FileIO.FileSystem]::DeleteFile(",
    "$args[0], 'OnlyErrorDialogs', 'SendToRecycleBin')"
  )
  result <- tryCatch(
    processx::run(
      powershell, c("-NoProfile", "-NonInteractive", "-Command", command, full_path),
      error_on_status = FALSE, timeout = 15
    ),
    error = function(e) NULL
  )
  list(ok = !is.null(result) && identical(result$status, 0L))
}

#' Move one file into the freedesktop.org "home trash" (XDG Trash spec)
#'
#' Implemented directly against the spec (`~/.local/share/Trash/{files,info}`,
#' or `$XDG_DATA_HOME/Trash` when set) rather than shelling out to a
#' desktop-specific helper, since no such helper is guaranteed to be
#' installed; this is the same location Nautilus, Dolphin, and `trash-cli`
#' read from, so the file shows up in the user's normal trash can. A name
#' collision in `files/` gets a numeric suffix before the extension. Only
#' the home trash (same-filesystem case) is implemented; a cross-device
#' repository is reported as unavailable rather than silently falling back
#' to a permanent delete.
#' @noRd
.trash_file_linux <- function(full_path) {
  data_home <- Sys.getenv("XDG_DATA_HOME", unset = "")
  trash_dir <- if (nzchar(data_home)) {
    fs::path(data_home, "Trash")
  } else {
    fs::path(fs::path_home(), ".local", "share", "Trash")
  }
  files_dir <- fs::path(trash_dir, "files")
  info_dir <- fs::path(trash_dir, "info")

  created <- tryCatch(
    {
      fs::dir_create(files_dir, recurse = TRUE)
      fs::dir_create(info_dir, recurse = TRUE)
      TRUE
    },
    error = function(e) FALSE
  )
  if (!created) {
    return(list(ok = FALSE))
  }

  base <- fs::path_file(full_path)
  stem <- fs::path_ext_remove(base)
  ext <- fs::path_ext(base)
  dest_name <- base
  n <- 1L
  while (fs::file_exists(fs::path(files_dir, dest_name)) ||
    fs::file_exists(fs::path(info_dir, paste0(dest_name, ".trashinfo")))) {
    n <- n + 1L
    dest_name <- if (nzchar(ext)) paste0(stem, " ", n, ".", ext) else paste0(stem, " ", n)
  }

  info_path <- fs::path(info_dir, paste0(dest_name, ".trashinfo"))
  info_text <- paste0(
    "[Trash Info]\n",
    "Path=", utils::URLencode(full_path, reserved = FALSE), "\n",
    "DeletionDate=", format(Sys.time(), "%Y-%m-%dT%H:%M:%S"), "\n"
  )

  moved <- tryCatch(
    {
      writeLines(info_text, info_path, useBytes = TRUE)
      fs::file_move(full_path, fs::path(files_dir, dest_name))
      TRUE
    },
    error = function(e) FALSE
  )
  if (!moved) {
    unlink(info_path)
    return(list(ok = FALSE))
  }
  list(ok = TRUE)
}

#' Dispatch a single-file trash to the current OS's mechanism
#' @noRd
.trash_file <- function(full_path) {
  switch(.trash_os_family(),
    macos = .trash_file_macos(full_path),
    windows = .trash_file_windows(full_path),
    linux = .trash_file_linux(full_path),
    list(ok = FALSE)
  )
}

#' Remove one untracked file via the OS trash/recycle bin
#'
#' Implements spec Sec 8.8: never `git clean`, never recursive/bulk, and
#' never a permanent delete. `path` must resolve to exactly one path that
#' the *current* `git status` still classifies as untracked (a fresh
#' `.git_status_entries()` read, mirroring the freshness check
#' `.git_restore_tracked_file()` uses for Sec 8.7) -- anything else,
#' including a path that has since been staged/committed or no longer
#' exists, is refused as `STATE_CHANGED` rather than acted on. A directory
#' is refused outright (`.git_status_entries()` reports an untracked
#' directory as one collapsed entry, and trashing it would delete
#' everything inside at once, which is exactly the bulk removal the spec
#' rules out). If the platform's trash mechanism is unavailable or refuses
#' the operation, the file is left in place and `TRASH_UNAVAILABLE` is
#' returned so the interface can explain manual removal instead.
#'
#' @return A list. On success: `ok = TRUE`, `path`. On failure: `ok =
#'   FALSE`, `code`, `message`, `recoverable`.
#' @noRd
.git_trash_untracked_file <- function(repo_root, git_bin, path) {
  path <- if (is.null(path)) "" else as.character(path)[[1]]

  if (!.validate_repo_relative_path(repo_root, path)) {
    return(list(
      ok = FALSE, code = "PATH_OUTSIDE_REPOSITORY",
      message = "That path is not inside this repository.", recoverable = FALSE
    ))
  }

  status_entries <- .git_status_entries(repo_root, git_bin)
  entry <- Find(function(e) identical(e$path, path), status_entries)

  if (is.null(entry) || entry$kind != "untracked") {
    return(list(
      ok = FALSE, code = "STATE_CHANGED",
      message = "The repository changed since this was shown. Refresh and try again.",
      recoverable = TRUE
    ))
  }

  full_path <- fs::path(repo_root, path)
  if (fs::is_dir(full_path)) {
    return(list(
      ok = FALSE, code = "PATH_IS_DIRECTORY",
      message = "Only a single file can be moved to the trash, not a folder.",
      recoverable = FALSE
    ))
  }
  if (!fs::file_exists(full_path)) {
    return(list(
      ok = FALSE, code = "STATE_CHANGED",
      message = "The repository changed since this was shown. Refresh and try again.",
      recoverable = TRUE
    ))
  }

  trash_result <- .trash_file(full_path)
  if (!isTRUE(trash_result$ok)) {
    return(list(
      ok = FALSE, code = "TRASH_UNAVAILABLE",
      message = paste0(
        "gitneighbr couldn't move this file to your trash or recycle bin. ",
        "Delete it yourself if you don't want it: ", path
      ),
      recoverable = FALSE
    ))
  }

  list(ok = TRUE, path = path)
}
