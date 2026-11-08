# A real HTTPS Git remote for the managed Send/Update tests: a bare repo
# served by `git http-backend` behind Basic auth, over TLS with a throwaway
# self-signed certificate. Needs python3 and openssl; tests skip otherwise.
.local_git_https_remote <- function(env = parent.frame(), user = "x-access-token", password = "s3cret-token-value") {
  testthat::skip_on_cran()
  python <- unname(Sys.which("python3"))
  openssl <- unname(Sys.which("openssl"))
  git <- unname(Sys.which("git"))
  testthat::skip_if(!nzchar(python) || !nzchar(openssl) || !nzchar(git), "python3, openssl or git not available")

  root <- withr::local_tempdir(.local_envir = env)
  cert <- file.path(root, "cert.pem")
  key <- file.path(root, "key.pem")
  made <- processx::run(openssl, c(
    "req", "-x509", "-newkey", "rsa:2048", "-nodes", "-keyout", key, "-out", cert, "-days", "1",
    "-subj", "/CN=127.0.0.1", "-addext", "subjectAltName=IP:127.0.0.1"
  ), error_on_status = FALSE)
  testthat::skip_if(!identical(made$status, 0L), "openssl can't make a test certificate")

  projects <- file.path(root, "projects")
  dir.create(projects)
  bare <- file.path(projects, "study.git")
  processx::run(git, c("init", "-q", "--bare", "-b", "main", bare))
  processx::run(git, c("-C", bare, "config", "http.receivepack", "true"))
  log <- file.path(root, "requests.log")
  file.create(log)

  port <- gitneighbr:::.find_free_port()
  script <- testthat::test_path("fixtures", "git_https_server.py")
  proc <- processx::process$new(python, c(script, port, projects, cert, key, paste0(user, ":", password), log),
    stdout = "|", stderr = "|", cleanup_tree = TRUE
  )
  withr::defer(proc$kill_tree(), envir = env)
  proc$poll_io(10000)
  testthat::skip_if(!identical(proc$read_output_lines(), "READY"), "test HTTPS server didn't start")

  list(
    url = sprintf("https://127.0.0.1:%d/study.git", port), bare = bare, cert = cert, log = log,
    user = user, password = password, git = git,
    requests = function() {
      lines <- readLines(log, warn = FALSE)
      lines[nzchar(lines)]
    }
  )
}

#' A managed session with the remote feature that trusts the test server
#' @noRd
.test_remote_api <- function(repo_root, remote, policy = NULL, env = parent.frame()) {
  config <- withr::local_tempfile(.local_envir = env, fileext = ".gitconfig")
  .write_ca_config(config, remote$cert)
  if (!is.null(policy)) {
    policy <- gitneighbr:::.validate_launcher_policy(policy)
  }
  managed <- gitneighbr:::.managed_config(features = "local,remote", policy = policy)
  git_bin <- gitneighbr:::.managed_git(remote$git, config, remote = TRUE)
  .test_api(repo_root, git_bin, managed = managed, env = env)
}

#' A Git config file that trusts `cert`
#'
#' Git reads backslashes in a config value as escapes, so a Windows path is
#' written with forward slashes. On Windows a managed Git checks certificates
#' with the Windows store (schannel), which can't trust a throwaway file, so
#' the test server's sessions use OpenSSL with that file instead.
#' @noRd
.write_ca_config <- function(config, cert) {
  backend <- if (.Platform$OS.type == "windows") "\tsslBackend = openssl"
  writeLines(c("[http]", backend, paste0("\tsslCAInfo = ", normalizePath(cert, winslash = "/"))), config)
}
