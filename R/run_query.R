#' Run Query Against Database
#'
#' @param conn connection to db.
#' @param query query to run
#'
#' @return
#' data.frame
#' @details
#' Currently only returning data from db is supported, i.e. only SELECT queries,
#' that's why we simply use `DBI::dbGetQuery`.
#' @noRd
run_query <- function(conn, query) {
  query <- stringi::stri_c("SELECT * FROM ( ", stringi::stri_replace_all_fixed(query, ";", ""), " ) AS t__sqlviewer__t LIMIT 1000;")
  tryCatch(DBI::dbGetQuery(conn, query),
           error = \(e) data.frame(validation = as.character(e)))
}
