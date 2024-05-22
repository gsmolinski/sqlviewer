#' Evaluate Character Vector
#'
#' Check if label was provided at the first line (or first not empty line).
#'
#' @param current_content content to evaluate (character vector length >= 1).
#'
#' @return
#' Logical vector length 1.
#' @noRd
prepare_content_to_evaluate <- function(current_content) {
  current_content <- gsub(" ", "", current_content, fixed = TRUE)
  grepl("^--#\\S", current_content[current_content != ""][[1]], perl = TRUE)
}

#' Divide Queries Into Separate Named Queries In List
#'
#' @param content from clipboard (character vector - each separate line will be separate vector element).
#'
#' @return
#' Named list with two elements:
#' (1) named list with prepared queries without piped queries
#' (2) data.table with raw queries with column: query and group - each query line in separate row.
#' @import data.table
divide_into_named_queries <- function(content) {
  group <- NULL
  queries <- data.table(query = content,
                        group = NA_integer_)
  queries[grepl("^\\s*--\\s*#\\s*\\S", query, perl = TRUE),
          group := .I]
  queries_names <- get_queries_names(queries$query[!is.na(queries$group)])
  setnafill(queries, cols = "group", type = "locf")
  named_queries <- setNames(split(queries, by = "group", keep.by = FALSE), queries_names)
  named_queries <- lapply(named_queries, `[[`, 1) |>
    lapply(collapse_query)
  list(named_queries = named_queries,
       raw_queries = queries)
}

#' Insert Queries As Subqueries
#'
#' @param queries list containing named_queries and raw_queries from `divide_into_named_queries` fun.
#'
#' @return
#' Named list.
#' @noRd
insert_piped_queries <- function(queries) {

}

#' Retrieve Names For Queries
#'
#' @param queries character vector of queries returned by `clipr` package, so each line is as a separate vector element.
#'
#' @return
#' Character vector
#' @noRd
get_queries_names <- function(queries) {
  queries_names <- stringi::stri_extract_all_regex(queries, "^\\s*--\\s*#\\s*\\S+", omit_no_match = TRUE) |>
    unlist(use.names = FALSE)
  queries_names <- stringi::stri_replace_all_regex(queries_names, "^--|\\s+|#", "")
  queries_names
}


#' Collapse Query Into Vector Length 1
#'
#' @param query query to collapse
#'
#' @return
#' Character vector length 1.
#' @details
#' It is safe to collapse query using `\n`, because
#' `DBI` just ignores new line.
#' @noRd
collapse_query <- function(query) {
  paste0(query, collapse = "\n")
}


