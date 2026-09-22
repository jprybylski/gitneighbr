# The PID-based kill/stop machinery in R/serve.R (.pids_listening_on_port,
# .pid_looks_like_gitneighbr_server, .parent_pid, .kill_pid,
# .kill_gitneighbr_server_pid, .stop_server_on_port) all run in the *calling*
# process, not inside a spawned server's own child process, so real spawned
# processes here are visible to covr -- unlike server.R's route handlers.

test_that(".package_root errors clearly when the package can't be found on the search path", {
  testthat::local_mocked_bindings(find.package = function(...) stop("not found"), .package = "base")
  expect_error(.package_root(), "run devtools::load_all")
})

test_that(".wait_for_server reports a clear error when the process exits during startup", {
  fake <- list(
    is_alive = function() FALSE,
    wait = function() invisible(NULL),
    read_all_error = function() "boom: something went wrong"
  )
  expect_error(
    .wait_for_server("127.0.0.1", 65500L, fake, timeout = 5),
    "server process exited during startup"
  )
})

test_that(".wait_for_server times out and kills the process tree when the port never opens", {
  killed <- FALSE
  fake <- list(
    is_alive = function() TRUE,
    kill_tree = function() killed <<- TRUE
  )
  # An unused high port that is never going to open on its own.
  free_port <- .find_free_port()
  expect_error(
    .wait_for_server("127.0.0.1", free_port, fake, timeout = 0.5),
    "did not become ready"
  )
  expect_true(killed)
})

test_that(".pids_listening_on_port finds a real listening process and returns none for a free port", {
  skip_on_os("windows")
  # `.find_free_port()`'s own TOCTOU race (documented on the function
  # itself) means an unrelated process can occasionally grab the "free"
  # port between that check and this one; retry a few times rather than
  # let that rare collision flake the suite.
  free_port <- NULL
  for (attempt in 1:5) {
    candidate <- .find_free_port()
    if (length(.pids_listening_on_port(candidate)) == 0L) {
      free_port <- candidate
      break
    }
  }
  expect_false(is.null(free_port))
  expect_equal(.pids_listening_on_port(free_port), integer())

  con <- serverSocket(free_port)
  on.exit(close(con), add = TRUE)
  pids <- .pids_listening_on_port(free_port)
  expect_true(Sys.getpid() %in% pids)
})

test_that(".pid_looks_like_gitneighbr_server distinguishes a real server process from an unrelated one", {
  skip_on_os("windows")
  git <- unname(Sys.which("git"))
  skip_if(!nzchar(git), "git not available")

  unrelated <- processx::process$new("sleep", "30")
  withr::defer(unrelated$kill())
  expect_false(.pid_looks_like_gitneighbr_server(unrelated$get_pid()))

  dir <- withr::local_tempdir()
  session <- .gitneighbr_serve(repo_root = dir, git_bin = git)
  withr::defer(session$stop())
  server_pid <- .pids_listening_on_port(session$.__enclos_env__$private$port_)
  expect_true(length(server_pid) >= 1L)
  expect_true(.pid_looks_like_gitneighbr_server(server_pid[[1]]))
})

test_that(".parent_pid resolves to the spawning process, and NA for a nonexistent PID", {
  skip_on_os("windows")
  expect_true(is.na(.parent_pid(999999999L)))

  child <- processx::process$new("sleep", "30")
  withr::defer(child$kill())
  expect_equal(.parent_pid(child$get_pid()), Sys.getpid())
})

test_that(".kill_pid force-kills an arbitrary process by PID", {
  skip_on_os("windows")
  child <- processx::process$new("sleep", "30")
  expect_true(child$is_alive())
  .kill_pid(child$get_pid())
  Sys.sleep(0.3)
  expect_false(child$is_alive())
})

test_that(".kill_gitneighbr_server_pid refuses to kill a process that isn't a gitneighbr server", {
  skip_on_os("windows")
  child <- processx::process$new("sleep", "30")
  withr::defer(child$kill())
  expect_error(.kill_gitneighbr_server_pid(child$get_pid()), "refusing to kill")
  expect_true(child$is_alive())
})

test_that(".stop_server_on_port reports FALSE for a port nothing is listening on", {
  free_port <- .find_free_port()
  expect_false(.stop_server_on_port("127.0.0.1", free_port))
})

test_that(".stop_server_on_port finds and kills a real gitneighbr server end-to-end", {
  git <- unname(Sys.which("git"))
  skip_if(!nzchar(git), "git not available")
  dir <- withr::local_tempdir()
  session <- .gitneighbr_serve(repo_root = dir, git_bin = git)
  port <- session$.__enclos_env__$private$port_

  expect_true(.port_is_open("127.0.0.1", port))
  expect_true(.stop_server_on_port("127.0.0.1", port))
  Sys.sleep(0.3)
  expect_false(.port_is_open("127.0.0.1", port))
})

test_that(".gitneighbr_serve retries an auto-picked port that loses the bind race", {
  git <- unname(Sys.which("git"))
  skip_if(!nzchar(git), "git not available")
  dir <- withr::local_tempdir()

  taken_port <- .find_free_port()
  con <- serverSocket(taken_port)
  withr::defer(close(con))
  real_free_port <- .find_free_port()

  calls <- 0L
  testthat::local_mocked_bindings(
    .find_free_port = function(...) {
      calls <<- calls + 1L
      if (calls == 1L) taken_port else real_free_port
    }
  )
  session <- .gitneighbr_serve(repo_root = dir, port = 0L, git_bin = git)
  withr::defer(session$stop())
  expect_true(calls >= 2L)
})
