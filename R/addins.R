#' RStudio addin: open gitneighbr for the current directory
#'
#' Registered in the RStudio Addins menu (see `inst/rstudio/addins.dcf`).
#' Equivalent to calling [open_repo()] with its defaults, which opens the
#' app for the current working directory (an RStudio project's directory,
#' when run from one) and launches the browser.
#'
#' @return Invisibly, the `gitneighbr_session` from [open_repo()].
#' @export
gitneighbr_addin <- function() {
  invisible(open_repo())
}
