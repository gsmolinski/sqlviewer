#' Set UI For App
#'
#' @param rstudio_theme_mode
#'
#' @return
#' HTML tags (UI).
#' @details
#' Needs to be a function to be able
#' to put it in the package.
#' @import shiny
#' @noRd
set_ui <- function(rstudio_theme_mode) {
  bslib::page_fluid(
    theme = bslib::bs_theme(5)

  )
}
