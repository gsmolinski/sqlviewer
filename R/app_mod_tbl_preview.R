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
    tags$div(id = ns("preview_sql"), class = "sqlviewer_header")
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
        queries_names <- get_queries_names(clipboard())
        queries_tbl <- mark_separate_queries(clipboard())
        queries_tbl <- mark_nested_queries(queries_tbl, queries_names)
        queries_order <- order_connected_queries(queries_tbl)
        resolved_queries <- resolve_queries(queries_order, queries_tbl, queries_names)
        # remove from ui, output and reactive `queries` everything from clipboard (because user decided to re-run this)
        invisible(lapply(names(resolved_queries), rm_ui_output_reactive, queries = queries, session = session, output = output))
        # insert queries into reactiveValues `queries` and make it named
        invisible(lapply(sort(names(resolved_queries)), \(e) `<-`(queries[[e]][["query"]], resolved_queries[[e]])))
        # insert UI and output only when not already inserted
        invisible(lapply(names(queries), insert_ui_output, queries = queries, session = session, conn = conn, output = output, color_mode = color_mode))
      }) |>
        bindEvent(clipboard())

    }
  )
}

#' Insert UI, `output` Element
#'
#' @param queries_name names of queries.
#' @param queries queries content (list).
#' @param session shiny session object.
#' @param conn connection to db.
#' @param output shiny output object.
#' @param color_mode app color (reactive).
#'
#' @return
#' Side effect - inserts ui, inserts render function and
#' marks reactive element as inserted.
#' @details
#' We need to dynamically insert UI and output, because
#' there is no better solution to keep one query opened (we don't)
#' want to rerender everything, because opening query is the same as
#' computing query, so we want to keep the current state if user do not
#' change anything. Only if user rerun query of the same name (name which
#' already exists) or adding new query, we just want to rerender this.
#'
#' @noRd
insert_ui_output <- function(queries_name, queries, session, conn, output, color_mode) {
  if (is.null(queries[[queries_name]][["inserted"]]) || !queries[[queries_name]][["inserted"]]) {
    #TODO put them alphabetically!
    #TODO make sure queries names can't be duplicated! (this has to be validated in different place, not here)
    insertUI(".sqlviewer_header", "beforeEnd",
             ui = reactable::reactableOutput(session$ns(stringi::stri_c("tbl_", queries_name))))

    output[[stringi::stri_c("tbl_", queries_name)]] <- reactable::renderReactable({
      reactable::reactable(data.frame(query = queries_name),
                           details = function(index) {
                             display_tbl(run_query(conn, queries[[queries_name]][["query"]]),
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

    queries[[queries_name]][["inserted"]] <- TRUE
  }
}

#' Remove Element From reactiveValues `queries`, From UI And `output`
#'
#' @param queries_name names of queries.
#' @param queries queries.
#' @param session shiny session object.
#' @param output shiny output object.
#'
#' @return
#' Side effect - removes UI, output and element from queries.
#' @noRd
rm_ui_output_reactive <- function(queries_name, queries, session, output) {
  removeUI(stringi::stri_c("#", session$ns(stringi::stri_c("tbl_", queries_name))))
  output[[stringi::stri_c("tbl_", queries_name)]] <- NULL
  queries[[queries_name]] <- NULL
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
