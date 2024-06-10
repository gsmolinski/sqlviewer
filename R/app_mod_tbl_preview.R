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
    reactable::reactableOutput(ns("tables"))
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
        current_content <- req(suppressWarnings(clipr::read_clip())) # suppress warning if no content in clipboard
        req(prepare_content_to_evaluate(current_content))
        clipboard(current_content)
      })

      queries <- reactive({
        req(clipboard())
        queries_names <- get_queries_names(clipboard())
        queries_tbl <- mark_separate_queries(clipboard())
        queries_tbl <- mark_nested_queries(queries_tbl, queries_names)
        queries_order <- order_connected_queries(queries_tbl)
        resolve_queries(queries_order, queries_tbl, queries_names)
      })

      output$tables <- reactable::renderReactable({
        req(queries())
        reactable::reactable(data.frame(query = names(queries())),
                             details = function(values) {
                               display_tbl(run_query(conn, queries()[[values]]))
                             },
                             compact = TRUE,
                             wrap = FALSE,
                             outlined = FALSE,
                             highlight = TRUE,
                             pagination = FALSE)
      })

    }
  )
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
