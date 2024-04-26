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
        current_content <- req(clipr::read_clip())
        req(prepare_content_to_evaluate(current_content))
        clipboard(current_content)
      })

      output$test <- renderText({
        clipboard()
      })

    }
  )
}

#' Evaluate Character Vector
#'
#' Check if label was provided at the first line (or first not empty line).
#'
#' @param current_content content to evaluate.
#'
#' @return
#' Logical vector length 1.
#' @noRd
prepare_content_to_evaluate <- function(current_content) {
  current_content <- gsub(" ", "", current_content, fixed = TRUE)
  grepl("^--#", current_content[current_content != ""][[1]], perl = TRUE)
}

#' Add `reactable` Styling To Table
#'
#' @param tbl_data data to transform into styled table.
#'
#' @return
#' `reactable` object.
#' @noRd
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
