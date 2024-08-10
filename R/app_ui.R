#' Set UI For App
#'
#' @return
#' HTML tags (UI).
#' @details
#' Needs to be a function to be able
#' to put it in the package.
#' @import shiny
#' @import bslib
#' @noRd
set_ui <- function() {
  page_fluid(
    htmltools::tags$head(
      include_dependency()
    ),
    lang = "en",
    theme = bs_theme(5),
    useBusyIndicators(),
    busyIndicatorOptions(spinner_selector = "html", fade_selector = "html", spinner_type = "dots"),
    htmltools::tags$div(id = "sqlviewer_header",
      input_switch("observe_clipboard", NULL, value = TRUE) |>
        tooltip("Observe cipboard?", placement = "right"),
      input_dark_mode(id = "sqlviewer_color_mode")
      ),
    tbl_preview_UI("tbl_preview")
  )
}
