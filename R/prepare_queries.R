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
#' @param content from clipboard.
#'
#' @return
#' Named list.
#' @import data.table
divide_into_named_queries <- function(content) {
  queries <- data.table(query = content,
                        group = NA_integer_)
  queries[grepl("^\\s*--\\s*#\\s*\\S", query, perl = TRUE),
          group := .I]
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
