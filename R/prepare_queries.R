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
  current_content <- stringi::stri_replace_all_regex(current_content, "^\\s+$", "")
  stringi::stri_detect_regex(current_content[current_content != ""][[1]], "^\\s*--\\s*#\\s*[a-zA-Z0-9_]+\\s*$")
}

#' Put Queries Into data.frame And Mark Where Query Starts
#'
#' @param queries data.table.
#'
#' @return
#' data.table with two columns: part of sql query and group
#' @details
#' For groups only one row per each group will be filled in by integer, so
#' it is necessary later to call e.g. `data.table::setnafill`.
#' This function also removes completely empty lines.
#' @noRd
mark_separate_queries <- function(queries) {
  group <- query <- NULL

  queries[stringi::stri_detect_regex(query, "^\\s*--\\s*#\\s*[a-zA-Z0-9_]+\\s*$"),
          group := .I]
  queries[query != ""]
}

#' Retrieve Names For Queries
#'
#' @param queries character vector of queries returned by `clipr` package, so each line is as a separate vector element.
#'
#' @return
#' Character vector
#' @details
#' We want to pass here only lines where is '--#label' and to do this we can
#' just get non missing rows from `mark_separate_queries`, i.e. before using `data.table::setnafill`.
#' @noRd
get_queries_names <- function(queries) {
  queries |>
    stringi::stri_extract_all_regex("^\\s*--\\s*#\\s*[a-zA-Z0-9_]+\\s*$", omit_no_match = TRUE) |>
    unlist(use.names = FALSE) |>
    stringi::stri_replace_all_regex("-|\\s|#", "")
}

#' Check If Queries' Names Are Duplicated
#'
#' @param queries_names character vector of queries names.
#'
#' @return
#' Logical vector length 1. Returns TRUE if no duplicated names.
#' Otherwise - FALSE.
#' @details
#' We don't want duplicated names, because code is not executed
#' from top to bottom, so we can't just overwrite one name by another.
#' @noRd
check_no_duplicated_names <- function(queries_names) {
  !any((duplicated(queries_names)))
}

#' Mark What Query Group Is Nested Within Query In Given Line (Row)
#'
#' @param queries_df data.table returned by `mark_separate_queries`.
#' @param queries_names character vector with queries labels (names), object returned by `get_queries_names`.
#'
#' @details
#' The idea is to find if in given line is nested query, i.e. line with code
#' `--|>label` (eventually with spaces) and what nested query it is (we are looking
#' for the group, not the label itself).
#' @return
#' data.table with three columns:
#' - query
#' - group to which content belongs (and without NAs!)
#' - nested query, i.e. what query group is nested in query in given line
#' @import data.table
#' @noRd
mark_nested_queries <- function(queries_df, queries_names) {
  nested_query <- group <- query <- nested_tmp_query <- NULL
  pattern <- stringi::stri_c(stringi::stri_c("^\\s*--\\s*\\|>\\s*#?\\s*", queries_names, "\\s*$", collapse = "|"), collapse = "|")
  queries_df[is.na(group),
             nested_tmp_query := fifelse(stringi::stri_detect_regex(query, pattern),
                                     stringi::stri_replace_all_regex(query, "-|>|#|\\s|\\|", ""),
                                     NA_character_)][
                                       !is.na(nested_tmp_query),
                                       nested_tmp_query := vapply(nested_tmp_query, \(e) as.character(which(queries_names == e)), FUN.VALUE = character(1))
                                     ][,
                                       nested_query := as.integer(nested_tmp_query)
                                     ][,
                                       nested_tmp_query := NULL
                                       ]
  setnafill(queries_df, "locf", cols = "group")
}

#' Check If There Is Nested Query Inside The Same Query
#'
#' @param queries data.table with query, group and nested query columns
#'
#' @details
#' We cannot proceed if there is self nested query, so this function
#' is intended to be used inside `shiny::req()` to stop the computation
#' if there is nested query inside the same query.
#' Self nested query can be simple: the same query nested in the same query;
#' or can be recursive - we need to find both cases.
#' To find self nested query we can switch the column order and compare with
#' table without switched column order - if we would find duplicated, it means
#' some queries are self nested.
#' @return
#' Logical vector length 1: TRUE if there is nested query FALSE otherwise.
#' @noRd
check_no_self_nested <- function(queries) {
  nested_query <- NULL

  queries <- queries[, c("group", "nested_query")]
  queries <- unique(queries[!is.na(nested_query)])
  if (queries[, .N] > 0) {
    queries_reverse <- queries[, c("nested_query", "group")] # switch columns
    names(queries_reverse) <- names(queries) # switch columns
    !any(duplicated(rbind(queries, queries_reverse))) # FALSE if at least one self nested
  } else {
    TRUE
  }
}

#' Order Queries From Most Inner One To Most Outer One
#'
#' @param queries data.frame with queries, groups and nested queries
#'
#' @details
#' queries data needs to be modified - we need different order of columns,
#' because only this way `igraph` will construct correct graph. We also need
#' only non missing rows for `nested_query` column, because missing rows to not
#' construct any connected vertexes. In case there is no nested queries, we want
#' to simply return all queries (as integers).
#' @return
#' Integer vector - order matters if there were nested queries, because
#' it shows in which order we need to resolve code at first to correctly
#' insert named sql queries.
#' @noRd
order_connected_queries <- function(queries) {
  nested_query <- NULL

  queries <- queries[, c("nested_query", "group")] # this order is needed for `igraph`
  queries <- unique(queries) # leave only unique rows
  queries_no_na <- queries[!is.na(nested_query)] # no need to consider queries without nested queries
  if (queries_no_na[, .N] > 0) {
    queries_graph <- igraph::graph_from_data_frame(queries_no_na)
    roots <- as.integer(igraph::V(queries_graph)[igraph::degree(queries_graph, mode = "in") == 0])
    connected_queries <- lapply(roots, order_connected_queries_helper, queries_graph = queries_graph) |>
      unlist(use.names = FALSE)
    unique(c(connected_queries, queries$group)) # add queries to tail which have no nested queries
  } else {
    unique(queries$group)
  }
}

#' Find Longest Path Starting From The Root
#'
#' @param root from which root start from?
#' @param queries_graph graph from queries data.frame
#'
#' @return
#' Integer vector.
#' @noRd
order_connected_queries_helper <- function(root, queries_graph) {
  igraph::ego(queries_graph, order = igraph::diameter(queries_graph), nodes = root, mode = "out")[[1]] |>
    names() |>
    as.integer()
}

#' Insert Nested Queries Into Queries And Return Full Query
#'
#' @param queries_order the order of queries to stick with (where to start, where to finish).
#' @param queries queries data.table.
#' @param queries_names vector of queries names.
#'
#' @details
#' The idea is to start from the right query group and then step by step resolve nested queries.
#' Because we need to have an access to what was resolved in previous step, we use data.table
#' passing by reference (`set` function in this case).
#' We also want to preserve user format (i.e. number of `\t` or `\s` and because of that
#' we need nested loop).
#' @return
#' Named list with full queries.
#' @noRd
resolve_queries <- function(queries_order, queries, queries_names) {
  all_resolved_queries <- vector("list", length(queries_order))
  for (i in queries_order) {
    nested_query_rows <- which(queries[["nested_query"]] == i)
    if (length(nested_query_rows) > 0) {
      for (nested_i in nested_query_rows) {
        # keep user format (number of \t or spaces)
        user_format <- stringi::stri_replace_all_regex(queries[["query"]][[nested_i]], "--.+$", "")
        prepared_query <- stringi::stri_c(user_format, queries[queries$group == i][["query"]])
        # now, resolve nested query and go to the next query group
        set(queries, nested_i, 1L, collapse_query(stringi::stri_replace_all_fixed(prepared_query, ";", ""))) # remove semicolon, collapse and insert
      }
    }
    prepared_query <- collapse_query(queries[queries$group == i][["query"]])
    all_resolved_queries[[i]] <- prepared_query
  }
  names(all_resolved_queries) <- queries_names
  all_resolved_queries
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
  stringi::stri_c(query, collapse = "\n")
}

#' Remove Queries Which User Rerun
#'
#' @param new_queries_names new queries names.
#' @param queries_tbl data.table with all data to resolve queries.
#'
#' @return
#' data.table.
#' @noRd
remove_chosen_existing_queries <- function(new_queries_names, queries_tbl) {
  group <- query <- NULL

  queries_tbl_names <- queries_tbl[, .SD[1], by = group][, query := stringi::stri_replace_all_regex(query, "-|\\s|#", "")]
  queries_to_rm <- unlist(lapply(new_queries_names, \(e) queries_tbl_names[query == e][["group"]]), use.names = FALSE)
  if (length(queries_to_rm) > 0) {
    queries_tbl[!group %in% queries_to_rm]
  } else {
    queries_tbl
  }
}
