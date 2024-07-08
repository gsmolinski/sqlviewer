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
#' @import bslib
#' @noRd
set_ui <- function(rstudio_theme_mode) {
  page_fluid(
    htmltools::tags$head(
      include_dependency()
    ),
    lang = "en",
    theme = bs_theme(5),
    htmltools::tags$div(id = "sqlviewer_header",
      input_switch("observe_clipboard", NULL, value = TRUE),
      input_dark_mode(mode = rstudio_theme_mode, id = "sqlviewer_color_mode")
      ),
    tbl_preview_UI("tbl_preview")
  )
}
