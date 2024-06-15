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
#' @param observe_clipboard reactive input TRUE/FALSE: should we observe clipboard?
#' @param color_mode light or dark mode.
#'
#' @return
#' server function
#' @noRd
tbl_preview_server <- function(id, conn, observe_clipboard, color_mode) {
  moduleServer(
    id,
    function(input, output, session) {
      clipboard <- reactiveVal()
      queries <- reactiveValues()

      observe({
        invalidateLater(1000)
        req(observe_clipboard())
        current_content <- req(suppressWarnings(clipr::read_clip())) # suppress warning if no content in clipboard
        req(prepare_content_to_evaluate(current_content))
        clipboard(current_content)
      })

      observe({
        req(clipboard)
        queries_names <- get_queries_names(clipboard())
        queries_tbl <- mark_separate_queries(clipboard())
        queries_tbl <- mark_nested_queries(queries_tbl, queries_names)
        queries_order <- order_connected_queries(queries_tbl)
        resolved_queries <- resolve_queries(queries_order, queries_tbl, queries_names)
        # insert queries into reactiveValues `queries` and make it named
        invisible(lapply(names(resolved_queries), \(e) `<-`(queries[[e]], resolved_queries[[e]])))
      })

      output$tables <- reactable::renderReactable({
        queries_labels <- sort(names(queries))
        reactable::reactable(data.frame(query = queries_labels),
                             details = function(index) {
                               display_tbl(run_query(conn, queries[[queries_labels[[index]]]]),
                                           color_theme = add_reactable_theme(color_mode()))
                             },
                             columns = list(
                               query = reactable::colDef(name = "")
                             ),
                             theme = add_reactable_theme(color_mode()),
                             compact = TRUE,
                             wrap = FALSE,
                             outlined = FALSE,
                             highlight = TRUE,
                             pagination = FALSE,
                             borderless = TRUE,
                             language = reactable::reactableLang(
                               noData = ""
                             ))
      })

    }
  )
}

#' Add `reactable` Styling To Table
#'
#' @param tbl_data data to transform into styled table.
#' @param color_theme theme from `add_reactable_theme` function.
#'
#' @return
#' `reactable` object.
#' @noRd
display_tbl <- function(tbl_data, color_theme) {
  reactable::reactable(tbl_data,
                       theme = color_theme,
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

#' Set Theme For `reactable` Table
#'
#' @param color_mode light or dark mode.
#'
#' @return
#' reactable object to set theme for table.
#' @noRd
add_reactable_theme <- function(color_mode) {
  backgroundColor <- NULL
  color <- NULL
  borderColor <- NULL
  highlightColor <- NULL
  inputStyle <- NULL
  pageButtonHoverStyle <- NULL
  pageButtonActiveStyle <- NULL

  if (color_mode == "dark") {
    backgroundColor <- "#1D1F21"
    color <- "hsl(233, 9%, 87%)"
    borderColor <- "hsl(233, 9%, 22%)"
    highlightColor <- "hsl(233, 12%, 24%)"
    inputStyle <- list(backgroundColor = "hsl(233, 9%, 25%)")
    pageButtonHoverStyle <- list(backgroundColor = "hsl(233, 9%, 25%)")
    pageButtonActiveStyle <- list(backgroundColor = "hsl(233, 9%, 28%)")
  }

  reactable::reactableTheme(
    color = color,
    backgroundColor = backgroundColor,
    borderColor = borderColor,
    highlightColor = highlightColor,
    inputStyle = inputStyle,
    pageButtonHoverStyle = pageButtonHoverStyle,
    pageButtonActiveStyle = pageButtonActiveStyle,
    headerStyle = list(
      borderWidth = "1px"
    )
  )
}
