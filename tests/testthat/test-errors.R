test_that(".error_title resolves every code in the spec Sec 15 taxonomy", {
  taxonomy_codes <- c(
    "GIT_UNAVAILABLE", "GIT_TOO_OLD", "NOT_REPOSITORY", "BARE_REPOSITORY",
    "IDENTITY_MISSING", "INVALID_NAME", "INVALID_EMAIL",
    "DETACHED_HEAD", "NO_UPSTREAM", "REMOTE_NOT_GITHUB", "AUTH_REQUIRED",
    "REMOTE_UNREACHABLE", "REMOTE_AHEAD", "DIVERGED", "DIRTY_BLOCKS_UPDATE",
    "CONFLICTS_PRESENT", "PROTECTED_BRANCH", "HOOK_FAILED", "SIGNING_FAILED",
    "LARGE_FILE_REJECTED", "TAG_EXISTS", "INVALID_TAG", "EMPTY_SELECTION",
    "EMPTY_COMMIT", "STATE_CHANGED", "OPERATION_IN_PROGRESS",
    "PATH_OUTSIDE_REPOSITORY", "COMMAND_FAILED"
  )
  for (code in taxonomy_codes) {
    title <- .error_title(code)
    expect_true(is.character(title) && nzchar(title), info = code)
    expect_false(identical(title, "Something went wrong"), info = code)
  }
})

test_that(".error_title falls back for an unknown code", {
  expect_equal(.error_title("SOME_FUTURE_CODE"), "Something went wrong")
})

test_that(".advanced_block sanitizes stderr and assembles a display command", {
  block <- .advanced_block(
    c("-C", "/x", "push", "origin", "main"),
    list(status = 1L, stderr = "fatal: Authentication failed for 'https://alice:hunter2@github.com/x/y.git/'")
  )
  expect_equal(block$command, "git -C /x push origin main")
  expect_equal(block$exit_status, 1L)
  expect_false(grepl("hunter2", block$stderr, fixed = TRUE))
})

test_that(".error_envelope includes title and omits advanced when not supplied", {
  env <- .error_envelope("EMPTY_SELECTION", "Select at least one file to save.")
  expect_false(env$ok)
  expect_equal(env$error$code, "EMPTY_SELECTION")
  expect_equal(env$error$title, .error_title("EMPTY_SELECTION"))
  expect_null(env$error$advanced)
})

test_that(".error_envelope carries an advanced block through when supplied", {
  advanced <- list(command = "git push origin main", exit_status = 1L, stderr = "fatal: ...")
  env <- .error_envelope("COMMAND_FAILED", "Git rejected this.", advanced = advanced)
  expect_equal(env$error$advanced, advanced)
})

test_that(".gitneighbr_error / .stop_gitneighbr_error carry a stable code on the condition", {
  err <- tryCatch(.stop_gitneighbr_error("GIT_TOO_OLD", "too old"), error = function(e) e)
  expect_s3_class(err, "gitneighbr_error")
  expect_s3_class(err, "gitneighbr_error_git_too_old")
  expect_equal(err$code, "GIT_TOO_OLD")
  expect_equal(conditionMessage(err), "too old")
})
