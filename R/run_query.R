#' Run Query Against Database
#'
#' @param conn expression to eval - it defines object "connection" as a DBI connection.
#' Expression is from script prepared to run as a background job.
#' @param query query to run
#'
#' @return
#' data.frame
#' @details
#' Currently only returning data from db is supported, i.e. only SELECT queries,
#' that's why we simply use `DBI::dbGetQuery`.
#' @noRd
run_query <- function(conn, query) {
  eval(conn)
  query <- stringi::stri_c("SELECT * FROM ( ", stringi::stri_replace_all_fixed(query, ";", ""), " ) AS t__sqlviewer__t LIMIT 1000;")
  result <- tryCatch(DBI::dbGetQuery(connection, query),
                     error = \(e) data.frame(Validation = as.character(e)))
  invisible(DBI::dbDisconnect(connection))
  result
}
