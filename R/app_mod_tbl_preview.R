#' Module UI
#'
#' @param id module id.
#'
#' @return
#' mod UI
#' @noRd
tbl_preview_UI <- function(id) {
  ns <- NS(id)
  tagList(

  )
}

#' Module server
#'
#' @param id module id.
#' @param conn connection to database.
#'
#' @return
#' server function
#' @noRd
tbl_preview_server <- function(id, conn) {
  moduleServer(
    id,
    function(input, output, session) {

    }
  )
}

display_tbl <- function(tbl_data) {
  reactable::reactable(tbl_data,
                       compact = TRUE,
                       wrap = FALSE,
                       borderless = TRUE,
                       highlight = TRUE,
                       paginationType = "jump",
                       language = reactable::reactableLang(
                         pagePrevious = "\u276e",
                         pageNext = "\u276f",
                         pagePreviousLabel = "Previous page",
                         pageNextLabel = "Next page"
                       ))
}
