#' Module UI
#'
#' @param id module id.
#'
#' @return
#' mod UI.
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
#' @param clipboard_mode should we use clipr::write_clipboard or JS to write to clipboard?
#' @param observe_clipboard reactive input TRUE/FALSE: should we observe clipboard?
#' @param rerun_btn button to rerun previous selection.
#' @param js_clipboard clipboard content read by JS function.
#' @param copy_query input from JS - query name copied by user.
#' @param remove_query input from JS - query name to remove chosen by user.
#' @param show_result input from JS - query name for which to show result.
#' @param hide_result input from JS - query name for which to hide result.
#'
#' @return
#' server function.
#' @noRd
tbl_preview_server <- function(id, conn, clipboard_mode, observe_clipboard, rerun_btn, js_clipboard, copy_query, remove_query, show_result, hide_result) {
  moduleServer(
    id,
    function(input, output, session) {

      clipboard <- reactiveVal()
      queries_tbl <- reactiveVal(data.table(query = NA_character_,
                                            group = NA_integer_,
                                            nested_query = NA_integer_))
      queries <- reactiveValues()

      observe({
        invalidateLater(500)
        req(observe_clipboard())
        if (clipboard_mode == "web") {
          session$sendCustomMessage("read_from_clipboard", "placeholder")
          current_content <- unlist(isolate(js_clipboard()))
          current_content <- stringi::stri_replace_all_fixed(current_content, "\r", "")
          current_content <- stringi::stri_split_fixed(current_content, "\n")
        } else if (clipboard_mode == "local") {
          current_content <- suppressWarnings(clipr::read_clip(allow_non_interactive = TRUE)) # suppress warning if no content in clipboard
        }
        current_content <- unlist(current_content)
        req(current_content)
        req(prepare_content_to_evaluate(current_content))
        clipboard(current_content)
      })

      observe({
        new_queries_names <- get_queries_names(clipboard())
        req(check_no_duplicated_names(new_queries_names))
        clipboard_content <- data.table(query = clipboard(),
                                        group = NA_integer_,
                                        nested_query = NA_integer_)
        queries_tbl(remove_chosen_existing_queries(new_queries_names, queries_tbl()))
        queries_tbl(rbindlist(list(queries_tbl(), clipboard_content), use.names = TRUE, fill = TRUE))
        queries_tbl(queries_tbl()[, `:=`(group = NA_integer_, nested_query = NA_integer_)])
        queries_tbl(mark_separate_queries(queries_tbl()))
        queries_names <- get_queries_names(queries_tbl()$query)
        queries_tbl(mark_nested_queries(queries_tbl(), queries_names))
        queries_order <- order_connected_queries(queries_tbl())
        resolved_queries <- resolve_queries(queries_order, queries_tbl(), queries_names)
        # remove from ui, output and reactive `queries` everything from clipboard (because user decided to re-run this)
        invisible(lapply(sort(new_queries_names), rm_ui_output_reactive, queries = queries, session = session, output = output))
        invisible(lapply(new_queries_names, remove_extended_task_fun, main_mod_envir = main_mod_envir))
        invisible(lapply(new_queries_names, \(e) {
          assign(stringi::stri_c("ext_task_", e), ExtendedTask$new(\(conn, query) {
            run_query_fun <- get("run_query", envir = parent.env(parent.env(environment(main_mod_envir))))
            promises::future_promise(run_query_fun(conn, query), seed = TRUE)
          }), envir = environment(main_mod_envir))
        }))
        # insert queries into reactiveValues `queries` and make it named
        invisible(lapply(sort(new_queries_names), \(e) `<-`(queries[["elements"]][[e]][["query"]], resolved_queries[[e]])))
        # insert UI and output only if not already inserted
        invisible(lapply(sort(names(queries[["elements"]])), insert_ui_output, queries = queries, session = session, conn = conn, input = input, output = output, main_mod_envir = main_mod_envir))
      }) |>
        bindEvent(clipboard(), rerun_btn())

      observe({
        req(hide_result())
        isolate({
          if (eval(parse(text = stringi::stri_c("ext_task_", hide_result(), "$status()"))) != "running") {
            session$sendCustomMessage("hide_result", session$ns(stringi::stri_c("tbl_", hide_result(), "_result")))
          }
        })
      })

      main_mod_envir <- function() {} # we use this just to reference to *this* environment

      observe({
        if (eval(parse(text = stringi::stri_c("ext_task_", show_result(), "$status()"))) != "running") {
          session$sendCustomMessage("show_result", session$ns(stringi::stri_c("tbl_", show_result(), "_result")))
          session$sendCustomMessage("add_running_class", session$ns(stringi::stri_c("tbl_", show_result())))
          eval(parse(text = stringi::stri_c("ext_task_", show_result(), "$invoke(conn, queries[['elements']][['", show_result(), "']][['query']])")))
        }
      }) |>
        bindEvent(show_result())

      observe({
        if (clipboard_mode == "web") {
          session$sendCustomMessage("copy_to_clipboard",
                                    stringi::stri_replace_all_regex(queries[["elements"]][[copy_query()]]$query, "^--", "-- |"))
        } else if (clipboard_mode == "local") {
          clipr::write_clip(stringi::stri_replace_all_regex(queries[["elements"]][[copy_query()]]$query, "^--", "-- |"),
                            allow_non_interactive = TRUE)
        }
      }) |>
        bindEvent(copy_query())

      observe({
        # to be honest, this is necessary, because if we simply do clipboard(NULL), then everything will be re-run
        # so if user has already copied to clipboard something which is displayed, then this will be re-run
        # and this is not something user is expecting - so we really need to write something to clipboard
        # what will be not correct sql statement accepted by sqlviewer - and let's say that we can "sell"
        # this as a feature - user removes query and as a backup we write to the clipboard this query.
        if (clipboard_mode == "web") {
          session$sendCustomMessage("copy_to_clipboard",
                                    stringi::stri_replace_all_regex(queries[["elements"]][[remove_query()]]$query, "^--", "-- |"))
        } else if (clipboard_mode == "local") {
          clipr::write_clip(stringi::stri_replace_all_regex(queries[["elements"]][[remove_query()]]$query, "^--", "-- |"),
                            allow_non_interactive = TRUE)
        }
        remove_extended_task_fun(remove_query(), main_mod_envir)
        queries_tbl(remove_chosen_existing_queries(remove_query(), queries_tbl()))
        rm_ui_output_reactive(remove_query(), queries, session, output) # now remove as user wants
      }) |>
        bindEvent(remove_query())

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
#' @param input shiny input object.
#' @param main_mod_envir function to retrieve main module environment.
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
insert_ui_output <- function(queries_name, queries, session, conn, input, output, main_mod_envir) {
  if (is.null(queries[["elements"]][[queries_name]][["inserted"]])) {
    selector <- determine_selector(queries_name, queries, session)
    tbl_query_name_id <- session$ns(stringi::stri_c("tbl_", queries_name))
    insertUI(selector, "afterEnd",
             ui = reactable::reactableOutput(tbl_query_name_id))

    output[[stringi::stri_c("tbl_", queries_name)]] <- reactable::renderReactable({
      reactable::reactable(data.frame(query = queries_name,
                                      copy = NA,
                                      remove = NA),
                           columns = list(
                             query = reactable::colDef(name = "", align = "left",
                                                       vAlign = "center",
                                                       style = "font-weight: 500"),
                             .selection = reactable::colDef(show = FALSE),
                             copy = reactable::colDef(name = "",
                                                      cell = \() actionButton(session$ns(stringi::stri_c(queries_name, "_copy_btn")), label = NULL, icon = icon("copy"), class = "btn-sm query_name_btn"),
                                                      align = "right",
                                                      maxWidth = 50),
                             remove = reactable::colDef(name = "",
                                                        cell = \() actionButton(session$ns(stringi::stri_c(queries_name, "_rm_btn")), label = NULL, icon = icon("trash"), class = "btn-sm query_name_btn"),
                                                        align = "left",
                                                        maxWidth = 50)
                           ),
                           theme = add_reactable_theme(),
                           compact = TRUE,
                           wrap = FALSE,
                           outlined = FALSE,
                           highlight = TRUE,
                           pagination = FALSE,
                           borderless = TRUE,
                           onClick = htmlwidgets::JS("
                                                     function(rowInfo, column) {
                                                      if (column.id === 'copy') {
                                                        Shiny.setInputValue('copy_query', rowInfo.values['query'], {priority: 'event'})
                                                      } else if (column.id === 'remove'){
                                                        Shiny.setInputValue('remove_query', rowInfo.values['query'], {priority: 'event'})
                                                      } else {
                                                        if (!rowInfo.isSelected) {
                                                          if (!document.getElementById('tbl_preview-tbl_' + rowInfo.values['query']).classList.contains('sqlviewer_running')) {
                                                            Shiny.setInputValue('show_result', rowInfo.values['query'], {priority: 'event'});
                                                            rowInfo.toggleRowSelected();
                                                          }
                                                        } else {
                                                          if (!document.getElementById('tbl_preview-tbl_' + rowInfo.values['query']).classList.contains('sqlviewer_running')) {
                                                            Shiny.setInputValue('hide_result', rowInfo.values['query'], {priority: 'event'});
                                                            rowInfo.toggleRowSelected();
                                                          }
                                                        }
                                                      }
                                                     }
                                                     "),
                           selection = "single",
                           sortable = FALSE,
                           )
    })

    insertUI(stringi::stri_c("#", tbl_query_name_id), "afterEnd",
             ui = reactable::reactableOutput(session$ns(stringi::stri_c("tbl_", queries_name, "_result")))
             )

    output[[stringi::stri_c("tbl_", queries_name, "_result")]] <- reactable::renderReactable({
        if (eval(parse(text = stringi::stri_c("ext_task_", queries_name, "$status()")), envir = environment(main_mod_envir)) == "initial") {
          session$sendCustomMessage("hide_result", session$ns(stringi::stri_c("tbl_", queries_name, "_result")))
        } else {
          if (eval(parse(text = stringi::stri_c("ext_task_", queries_name, "$status()")), envir = environment(main_mod_envir)) != "running") {
            session$sendCustomMessage("rm_running_class", session$ns(stringi::stri_c("tbl_", queries_name)))
          }
          display_tbl(eval(parse(text = stringi::stri_c("ext_task_", queries_name, "$result()")), envir = environment(main_mod_envir)),
                      color_theme = add_reactable_theme())
        }
    })

    queries[["elements"]][[queries_name]][["inserted"]] <- TRUE
  }
}

#' Determine Selector Where To Put UI
#'
#' @param queries_name name of actual query.
#' @param queries queries reactive.
#' @param session shiny session object.
#'
#' @return
#' Character vector length 1 - where to put UI to keep
#' alphabetical order of queries.
#' @noRd
determine_selector <- function(queries_name, queries, session) {
  ind <- which(queries_name == sort(names(queries[["elements"]])))
  if (ind == 1) {
    selector <- ".sqlviewer_header"
  } else {
    # "_result" at the end, because it should be inserted after the render responsible to display query result, not query name
    selector <- stringi::stri_c("#", session$ns(stringi::stri_c("tbl_", sort(names(queries[["elements"]]))[[ind - 1]], "_result")))
  }
  selector
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
  removeUI(stringi::stri_c("#", session$ns(stringi::stri_c("tbl_", queries_name, "_result"))))
  output[[stringi::stri_c("tbl_", queries_name, "_result")]] <- NULL
  queries[["elements"]][[queries_name]] <- NULL
}

#' Remove Extended Task Object
#'
#' @param query_name chosen query name.
#' @param main_mod_envir function to retrieve main mod environment.
#'
#' @return
#' Side effect - removes extended task object.
#' @noRd
remove_extended_task_fun <- function(query_name, main_mod_envir) {
  if (exists(stringi::stri_c("ext_task_", query_name), envir = environment(main_mod_envir))) {
    rm(list = stringi::stri_c("ext_task_", query_name), envir = environment(main_mod_envir))
  }
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
                       showPageInfo = FALSE,
                       language = reactable::reactableLang(
                         pagePrevious = "\u276e",
                         pageNext = "\u276f",
                         pagePreviousLabel = "Previous page",
                         pageNextLabel = "Next page"
                       ))
}

#' Set Theme For `reactable` Table
#'
#' @return
#' reactable object to set theme for table.
#' @noRd
add_reactable_theme <- function() {
  reactable::reactableTheme(
    style = list("html[data-bs-theme='dark'] &" = list(color = "hsl(233, 9%, 87%)",
                                                       backgroundColor = "#1D1F21",
                                                       borderColor = "hsl(233, 9%, 22%)")),
    headerStyle = list("html[data-bs-theme='dark'] &" = list(borderColor = "hsl(233, 9%, 22%)"),
                       borderWidth = "1px"),
    inputStyle = list("html[data-bs-theme='dark'] &" = list(backgroundColor = "hsl(233, 9%, 25%)",
                                                            color = "hsl(233, 9%, 87%)")),
    rowHighlightStyle = list("html[data-bs-theme='dark'] &" = list(backgroundColor = "hsl(233, 12%, 24%)")),
    paginationStyle = list("html[data-bs-theme='dark'] &" = list(borderColor = "hsl(233, 9%, 22%)",
                                                                 color = "hsl(233, 9%, 87%)")),
    pageButtonHoverStyle = list("html[data-bs-theme='dark'] &" = list(backgroundColor = "hsl(233, 9%, 25%)")),
    pageButtonActiveStyle = list("html[data-bs-theme='dark'] &" = list(backgroundColor = "hsl(233, 9%, 28%)")),
    rowSelectedStyle = list(boxShadow = "inset 2px 0 0 0 #007BC2")
  )
}
