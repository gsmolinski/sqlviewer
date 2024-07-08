#' Run Query Against Database
#'
#' @param conn connection to db.
#' @param query query to run.
#'
#' @return
#' data.frame
#' @details
#' Currently only returning data from db is supported, i.e. only SELECT queries,
#' that's why we simply use `DBI::dbGetQuery`.
#' @noRd
run_query <- function(conn, query) {
  tryCatch(DBI::dbGetQuery(conn, query),
           error = \(e) data.frame(validation = as.character(e)))
}

#' Count Number Of Rows On User Query
#'
#' @param conn connection to db.
#' @param query query to run.
#'
#' @return
#' 0 if error (syntax SQL error), otherwise integer vector length 1.
#' @noRd
count_rows <- function(conn, query) {
  query <- stringi::stri_c("SELECT COUNT(*) FROM ( ", stringi::stri_replace_all_fixed(query, ";", ""), ") AS t__sqlviewer__t;")
  tryCatch(DBI::dbGetQuery(conn, query),
           error = \(e) 0)[[1]]
}

#' Pick Random OFFSET And Construct Query
#'
#' @param query query constructed by user.
#' @param total_rows COUNT(*) of table.
#'
#' @return
#' Character vector length 1 - query to run.
#' @noRd
sample_rows <- function(query, total_rows) {
  if (total_rows <= 1000) {
    query
  } else {
    offset <- sample(tryCatch(seq_len(total_rows - 1001), error = \(e) 0), 1)
    stringi::stri_c("SELECT * FROM ( ", stringi::stri_replace_all_fixed(query, ";", ""), " ) AS t__sqlviewer__t OFFSET ", offset, " LIMIT 100;")
  }
}
