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
    tags$head(
      include_css_dependency()
    ),
    lang = "en",
    theme = bs_theme(5),
    tags$div(id = "sqlviewer_header",
      input_switch("sqlviewer_clipboard", NULL, value = TRUE) |>
        tooltip("Observe clipboard?", placement = "right"),
      input_dark_mode(mode = rstudio_theme_mode, id = "sqlviewer_input_dark")
      ),
    tbl_preview_UI("tbl_preview")
  )
}
