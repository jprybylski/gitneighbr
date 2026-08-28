#' A running gitneighbr app session
#'
#' Wraps the background `processx` server process for one repository.
#' Printing a session never reveals its token.
#'
#' @importFrom R6 R6Class
#' @importFrom utils browseURL tail
#' @export
GitneighbrSession <- R6::R6Class(
  "gitneighbr_session",
  public = list(
    #' @description Create a session. Not normally called directly; use
    #'   [open_repo()].
    #' @param process A live `processx::process`.
    #' @param host,port Where the server is bound.
    #' @param token The bearer token required by the API.
    #' @param repo_root The repository root this session serves.
    initialize = function(process, host, port, token, repo_root) {
      private$process_ <- process
      private$host_ <- host
      private$port_ <- port
      private$token_ <- token
      private$repo_root_ <- repo_root
    },

    #' @description Is the server process still running?
    is_alive = function() {
      private$process_$is_alive()
    },

    #' @description The repository root this session serves.
    repo_path = function() {
      private$repo_root_
    },

    #' @description The session URL.
    #' @param redact If `TRUE` (the default), omit the token fragment.
    url = function(redact = TRUE) {
      base <- paste0("http://", private$host_, ":", private$port_, "/")
      if (redact) base else paste0(base, "#token=", private$token_)
    },

    #' @description Open the session, preferring the RStudio/Positron Viewer
    #'   pane (via `getOption("viewer")`, the same hook `shiny`, `profvis`,
    #'   and `htmlwidgets` use) and falling back to the default system
    #'   browser everywhere else.
    browse = function() {
      if (!self$is_alive()) {
        stop("gitneighbr: session is no longer running.", call. = FALSE)
      }
      url <- self$url(redact = FALSE)
      viewer <- getOption("viewer")
      if (is.function(viewer)) {
        viewer(url)
      } else {
        utils::browseURL(url)
      }
      invisible(self)
    },

    #' @description The last `n` lines of newly captured server output (both
    #'   stdout and stderr), for troubleshooting. Each call drains the
    #'   process's output buffer, so previously returned lines are not
    #'   repeated.
    #' @param n Number of lines to return.
    logs = function(n = 100L) {
      out <- private$process_$read_output_lines(n = -1)
      err <- private$process_$read_error_lines(n = -1)
      utils::tail(c(out, err), n)
    },

    #' @description Stop the server and clean up its process tree.
    stop = function() {
      if (self$is_alive()) {
        private$process_$kill_tree()
      }
      invisible(self)
    },

    #' @description Custom print method that never reveals the token.
    #' @param ... Unused; present for S3/R6 print-method compatibility.
    print = function(...) {
      cat(
        "<gitneighbr_session>\n",
        "  repo:  ", private$repo_root_, "\n",
        "  url:   ", self$url(redact = TRUE), "\n",
        "  alive: ", self$is_alive(), "\n",
        sep = ""
      )
      invisible(self)
    }
  ),
  private = list(
    process_ = NULL,
    host_ = NULL,
    port_ = NULL,
    token_ = NULL,
    repo_root_ = NULL
  )
)
