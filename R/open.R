#' Run `sqlviewer` App
#'
#' Runs `shiny` application with the functionality provided by `sqlviewer`:
#' preview SQL queries and construct complex queries using solution inspired by pipe operator.
#'
#' @param drv database driver passed to `[DBI::dbConnect()]`.
#' @param ... other database driver arguments passed to `[DBI::dbConnect()]`. See that function for details.
#' @param clipboard_mode `"web"` if `sqlviewer` will be used in web browser or `"local"` otherwise. Defaults to `"web"`. See *Details* section for details.
#' @param app_host IPv4 address (character vector length 1) on which application should listen on. Defaults to `"127.0.0.1"` (localhost). Argument passed to `[shiny::shinyApp()]`.
#' @param app_port TCP port (integer vector length 1) on which application should listen on. Defaults to `49152`. Argument passed to `[shiny::shinyApp()]`.
#' @param launch_browser launch browser when app starts? Must be logical length 1. Defaults to `FALSE`. Argument passed to `[shiny::shinyApp()]`. Use `.rs.invokeShinyPaneViewer`
#' object as an argument to open in RStudio Viewer.
#'
#' @return
#' Used for side effect - runs app.
#'
#' @details
#' `sqlviewer` can be currently used only to retrieve data from DB, i.e. only `SELECT` queries are allowed (SQL code to run is passed to
#' `[DBI::dbGetQuery()]`).
#'
#' To run app in parallel mode (each query will be run in separate process), call `[future::plan()]` with chosen strategy before
#' calling `sqlviewer::open()`. See *Examples* and *App Functionality* sections for more details.
#'
#' `sqlviewer` can be start in two modes: using `clipr::read_clip()` and `clipr::write_clip()` to read from and write content back to clipboard or using JavaScript functions
#' to read from and write to clipboard. JavaScript should be used if user opens `sqlviewer` in a web browser (even if this is local web browser and even
#' without working Internet connection) - then `"web"` argument should be passed to `clipboard_mode` parameter.
#' If `sqlviewer` will be used in different environment where JavaScript do not work (e.g. RStudio Viewer), `"local"` argument should be passed to `clipboard_mode` parameter.
#'
#' @section App Functionality:
#' To insert query into app, copy-paste it to clipboard (it is possible to copy more than one query at a time, then more than one table will be displayed).
#' App will insert queries only if clipboard content changed from
#' previous insertion (e.g. if the same content is copied twice in a row, query won't be re-inserted). Each query must be named
#' and **if some names of queries are duplicated, then app won't run**.
#'
#' Switch button is used to indicate if clipboard should be observed. If set to off, then
#' no new queries will be created. To see result for chosen query, click on its name.
#' To copy query - click copy button - and to remove query, click remove button.
#' When query is removed, it is also copied to clipboard.
#'
#' As said above, app will insert queries only if content of clipboard changed from previous insertion, but
#' if one would like to re-run the same selection as before, then on top of the app is a button for this.
#'
#' Each query is run in separate process using `[shiny::ExtendedTask]` and when the query is running, app will not
#' allow to re-run the same query with the one exception - if the query was re-inserted to the app (copy-pasted again
#' to clipboard), then it will be possible to run this query - it will run in the new process, which means that previously
#' started query is still running. Unfortunately, it is impossible to kill process started by `[shiny::ExtendedTask]` and if
#' one would need to do this, the only solution is to restart main R session.
#'
#' `sqlviewer` displays only first 1000 rows of table.
#'
#' @section Labeling:
#' Each SQL query needs to have label (name). Label **has to be in its own line** (i.e. nothing more should exist in the same line except of intendation)
#' and have following format:
#' \preformatted{
#' -- #label
#' }
#' where instead of `label` should be unique query name (see *Example* section below or *Piping* section). **To be valid, label must be
#' constructed using only: letters, numbers and underscores.**
#' Label can be used later for piping and will be used when displaying results of queries in the app.
#' @section Piping:
#' SQL queries can be very complex, especially when using nested queries. `sqlviewer` comes with the functionality to pipe one
#' labelled query into another query using `|>` operator. Below is an example of how to pipe one query into another query:
#' \preformatted{
#' -- #all_species
#' SELECT i.Species
#' FROM iris i;
#'
#' -- #filtered_data
#' SELECT *
#' FROM iris i
#' WHERE i.Species IN (
#'    -- |> all_species
#'    );
#' }
#' Pipe operator **has to be in its own line** (i.e. nothing more should exist in the same line except indentation) and can be read as
#' "here put *this* query". `sqlviewer` will analyze the code and insert labelled queries *as-is* (query is not computed, just inserted)
#' in the line where pipe operator was used. It is not necessary to write queries from top to bottom, i.e. nested labelled query
#' can be below query into which this nested query will be piped.
#'
#' @export
#' @import shiny
#'
#' @examples
#'
#' \dontrun{
#' temp_db <- fs::file_temp("sqlviewerDB_example", ext = ".db")
#' conn <- DBI::dbConnect(RSQLite::SQLite(), dbname = temp_db)
#' DBI::dbWriteTable(conn, "iris", iris)
#' DBI::dbDisconnect(conn)
#' future::plan("multisession") # run app in parallel mode
#' sqlviewer::open(RSQLite::SQLite(), dbname = temp_db)
#' # Now, copy SQL statement to clipboard (without comment signs!) and include label (-- #all_data)
#' #
#' # -- #all_data
#' # SELECT * FROM iris;
#' #
#' # and see result.
#' file.remove(temp_db)
#' }
open <- function(drv, ...,
                 clipboard_mode = c("web", "local"),
                 launch_browser = FALSE,
                 app_host = "127.0.0.1",
                 app_port = 49152) {

  clipboard_mode <- match.arg(clipboard_mode)
  if (clipboard_mode == "local") {
    if (!clipr::clipr_available()) {
      stop(clipr::dr_clipr(), call. = FALSE)
    }
  }
  dot_args <- list(...)
  # connection has to be evaluate in child process that's why we use expression instead of connection object
  conn <- parse(text = stringi::stri_c("connection <- DBI::dbConnect(drv = ",
                                       stringi::stri_replace_all_fixed(deparse(substitute(drv)), '"', ""),
                                       ", ",
                                       stringi::stri_c(vapply(seq_len(length(dot_args)),
                                                              resolve_dot_dot_dot,
                                                              character(1),
                                                              dot_args), collapse = ", "),
                                       ")"))
  shinyApp(set_ui(),
           set_server(conn, clipboard_mode),
           options = list(launch.browser = launch_browser,
                          host = app_host,
                          port = app_port))
}

#' Prepare dot-dot-dot Arguments
#'
#' @param idx index.
#' @param dot_args arguments from dot-dot-dot.
#'
#' @return
#' character vector of length the same as input vectors.
#' @noRd
resolve_dot_dot_dot <- function(idx, dot_args) {
  if (is.null(names(dot_args)[[idx]]) || is.na(names(dot_args)[[idx]]) || names(dot_args)[[idx]] == "") {
    deparse(unclass(dot_args[[idx]]))
  } else {
    stringi::stri_c(names(dot_args)[[idx]], " = ", deparse(unclass(dot_args[[idx]])))
  }
}
