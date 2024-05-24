#' Run Query Against Database
#'
#' @param conn connection to db.
#' @param query query to run
#'
#' @return
#' data.frame
#' @details
#' Currently only returning data from db is supported, i.e. only SELECT queries,
#' that's why we simple use `DBI::dbGetQuery`.
#' @noRd
run_query <- function(conn, query) {
  tryCatch(DBI::dbGetQuery(conn, query),
           error = \(e) data.frame(validation = as.character(e)))
}
