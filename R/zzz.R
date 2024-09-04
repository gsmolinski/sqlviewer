globalVariables("connection")

#' Add 'future' to renv.lock
#'
#' @return
#' Side effect - function is not used, we want to discover
#' future package to add to renv.lock file
#' @noRd
placeholder_fun <- function() {
  future::plan()
}
