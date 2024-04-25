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
    tags$div(id = ns("preview_sql")),
    textOutput(ns("test"))
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
tbl_preview_server <- function(id, conn, observe_clipboard) {
  moduleServer(
    id,
    function(input, output, session) {
      clipboard <- reactiveVal()

      observe({
        invalidateLater(1000)
        req(observe_clipboard())
        current_content <- clipr::read_clip()
        req(current_content)
        req(grepl("--#", sub(" ", "", current_content[[1]], fixed = TRUE), fixed = TRUE))
        if (paste(current_content, collapse = " ") != paste(clipboard(), collapse = " ")) {
          clipboard(current_content)
        }
      })

      output$test <- renderText({
        clipboard()
      })

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
