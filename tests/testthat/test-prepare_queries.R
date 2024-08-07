test_that("prepare_content_to_evaluate recognizes correctly label in query", {
  expect_true(prepare_content_to_evaluate(c("", "--# fgh")))
  expect_true(prepare_content_to_evaluate(c("     ", "--# fgh")))
  expect_false(prepare_content_to_evaluate(c("  ", "   --  ##### fgh")))
  expect_false(prepare_content_to_evaluate(c("", "--# 123][x./fgh")))
  expect_false(prepare_content_to_evaluate(c("fgh", "--# 123][x./fgh")))
  expect_false(prepare_content_to_evaluate(c("      sfd", "--# 123][x./fgh")))
  expect_false(prepare_content_to_evaluate(c("      ", "--# 123][x./fgh")))
})

test_that("get_queries_names returns names of queries correctly", {
  sql_no_pipe <- c("--# get_all", "SELECT *", "FROM iris;", "", "--#table1", "SELECT Species, Species FROM iris WHERE Species LIKE '%setosa%';",
                   "", "-- # table2", "", "SELECT Species", "FROM iris i", "WHERE i.Species = 'setosa' OR ",
                   "\ti.Species IN ('versicolor');")
  sql_with_pipe <- c("-- #get_all", "SELECT *", "FROM iris;", "", "--#all_species",
                     "SELECT i.Species", "FROM iris i;", "", "--#filtered_data", "SELECT *",
                     "FROM iris i", "WHERE i.Species IN (", "\t\t\t\t\t-- |> all_species",
                     "\t\t\t\t\t);", "", "-- # table1", "SELECT *", "FROM (", "\t\t-- |> all_species",
                     ")", "", "--#another_filtered_data ",
                     "SELECT *", "FROM iris i", "WHERE i.Species IN (", "\t\t\t\t\t-- |> table1",
                     "\t\t\t\t\t);")

  expect_equal(get_queries_names(sql_no_pipe), c("get_all", "table1", "table2"))
  expect_equal(get_queries_names(sql_with_pipe), c("get_all", "all_species", "filtered_data", "table1", "another_filtered_data"))
})

test_that("mark_separate_queries works with or without piping", {
  sql_no_pipe <- c("--# get_all", "SELECT *", "FROM iris;", "", "--#table1", "SELECT Species, Species FROM iris WHERE Species LIKE '%setosa%';",
                   "", "-- # table2", "", "SELECT Species", "FROM iris i", "WHERE i.Species = 'setosa' OR ",
                   "\ti.Species IN ('versicolor');")
  sql_with_pipe <- c("-- #get_all", "SELECT *", "FROM iris;", "", "--#all_species",
                     "SELECT i.Species", "FROM iris i;", "", "--#filtered_data", "SELECT *",
                     "FROM iris i", "WHERE i.Species IN (", "\t\t\t\t\t-- |> all_species",
                     "\t\t\t\t\t);", "", "-- # table1", "SELECT *", "FROM (", "\t\t-- |> all_species",
                     ")", "", "--#another_filtered_data ",
                     "SELECT *", "FROM iris i", "WHERE i.Species IN (", "\t\t\t\t\t-- |> table1",
                     "\t\t\t\t\t);")
  sql_no_pipe <- data.table(query = sql_no_pipe,
                            group = NA_integer_)
  sql_with_pipe <- data.table(query = sql_with_pipe,
                              group = NA_integer_)
  result_no_pipe <- mark_separate_queries(sql_no_pipe)
  result_with_pipe <- mark_separate_queries(sql_with_pipe)

  expect_equal(result_no_pipe,
               structure(list(query = c("--# get_all", "SELECT *", "FROM iris;",
                                        "--#table1", "SELECT Species, Species FROM iris WHERE Species LIKE '%setosa%';",
                                        "-- # table2", "SELECT Species", "FROM iris i", "WHERE i.Species = 'setosa' OR ",
                                        "\ti.Species IN ('versicolor');"), group = c(1L, NA, NA, 2L,
                                                                                     NA, 3L, NA, NA, NA, NA)), row.names = c(NA, -10L), class = c("data.table",
                                                                                                                                                  "data.frame")
                         ))
  expect_equal(result_with_pipe,
               structure(list(query = c("-- #get_all", "SELECT *", "FROM iris;",
                                        "--#all_species", "SELECT i.Species", "FROM iris i;", "--#filtered_data",
                                        "SELECT *", "FROM iris i", "WHERE i.Species IN (", "\t\t\t\t\t-- |> all_species",
                                        "\t\t\t\t\t);", "-- # table1", "SELECT *", "FROM (", "\t\t-- |> all_species",
                                        ")", "--#another_filtered_data ",
                                        "SELECT *", "FROM iris i", "WHERE i.Species IN (", "\t\t\t\t\t-- |> table1",
                                        "\t\t\t\t\t);"), group = c(1L, NA, NA, 2L, NA, NA, 3L, NA, NA,
                                                                   NA, NA, NA, 4L, NA, NA, NA, NA, 5L, NA, NA, NA, NA, NA)), row.names = c(NA,
                                                                                                                                           -23L), class = c("data.table", "data.frame")
                         ))
})

test_that("mark_nested_queries correctly finds nested queries", {
  sql_with_pipe <- c("-- #get_all", "SELECT *", "FROM iris;", "", "--#all_species",
                     "SELECT i.Species", "FROM iris i;", "", "--#filtered_data", "SELECT *",
                     "FROM iris i", "WHERE i.Species IN (", "\t\t\t\t\t-- |> all_species",
                     "\t\t\t\t\t);", "", "-- # table1", "SELECT *", "FROM (", "\t\t-- |> all_species",
                     ")", "", "--#another_filtered_data ",
                     "SELECT *", "FROM iris i", "WHERE i.Species IN (", "\t\t\t\t\t-- |> table1",
                     "\t\t\t\t\t);")
  queries_names <- get_queries_names(sql_with_pipe)
  sql_with_pipe <- data.table(query = sql_with_pipe,
                              group = NA_integer_)
  queries_df <- mark_separate_queries(sql_with_pipe)
  result <- mark_nested_queries(queries_df, queries_names)
  expect_length(result, 3)
  expect_equal(result$nested_query, c(rep(NA_integer_, 10), 2, rep(NA_integer_, 4), 2, rep(NA_integer_, 5), 4, NA_integer_))
})

test_that("check_no_self_nested correctly finds nested queries", {
  queries <- data.table(group = c(1, 2, 3),
                        nested_query = c(NA, NA, 3))
  expect_false(check_no_self_nested(queries))

  queries <- data.table(nested_query = c(1, 2, 3),
                          group = c(2, 1, 4))
  expect_false(check_no_self_nested(queries))

  queries <- data.table(nested_query = c(1, 2, 3),
                          group = c(3, 3, 1))
  expect_false(check_no_self_nested(queries))

  queries <- data.table(group = c(1, 2, 3),
                        nested_query = c(NA, NA, NA))
  expect_true(check_no_self_nested(queries))
})

test_that("order_connected_queries finds connected graph and orders them from most inner to most outer", {
  test_df_1 <- data.table(nested_query = c(2, 2, 4),
                          group = c(3, 4, 5))
  test_df_2 <- data.table(nested_query = c(4, 2, 2),
                          group = c(5, 4, 3))
  test_df_3 <- data.table(nested_query = c(1, 3, 2, 6, 5),
                          group = c(3, 4, 1, 7, 6))
  test_df_4 <- data.table(nested_query = c(NA, NA, NA),
                          group = c(1, 2, 3))
  test_df_5 <- data.table(nested_query = c(2, 2, NA, NA),
                          group = c(3, 4, 2, 5))

  expect_equal(order_connected_queries(test_df_1), c(2, 4, 3, 5))
  expect_equal(order_connected_queries(test_df_2), c(2, 4, 3, 5))
  expect_equal(order_connected_queries(test_df_3), c(2, 1, 3, 4, 5, 6, 7))
  expect_equal(order_connected_queries(test_df_4), c(1, 2, 3))
  expect_equal(order_connected_queries(test_df_5), c(2, 3, 4, 5))
})

test_that("resolve_queries inserts correct queries into nested objects and returns named list with full queries", {
  sql_with_pipe <- c("-- #get_all", "SELECT *", "FROM iris;", "", "--#all_species",
                     "SELECT i.Species", "FROM iris i;", "", "--#filtered_data", "SELECT *",
                     "FROM iris i", "WHERE i.Species IN (", "\t\t\t\t\t-- |> all_species",
                     "\t\t\t\t\t);", "", "-- # table1", "SELECT *", "FROM (", "\t\t-- |> all_species",
                     ")", "", "--#another_filtered_data ",
                     "SELECT *", "FROM iris i", "WHERE i.Species IN (", "\t\t\t\t\t-- |> table1",
                     "\t\t\t\t\t);")
  queries_names <- get_queries_names(sql_with_pipe)
  sql_with_pipe <- data.table(query = sql_with_pipe,
                              group = NA_integer_,
                              nested_query = NA_integer_)
  queries_df <- mark_separate_queries(sql_with_pipe)
  queries_df <- mark_nested_queries(queries_df, queries_names)
  queries_order <- order_connected_queries(queries_df)
  result <- resolve_queries(queries_order, queries_df, queries_names)

  expect_equal(result,
               list(get_all = "-- #get_all\nSELECT *\nFROM iris;", all_species = "--#all_species\nSELECT i.Species\nFROM iris i;",
                    filtered_data = "--#filtered_data\nSELECT *\nFROM iris i\nWHERE i.Species IN (\n\t\t\t\t\t--#all_species\n\t\t\t\t\tSELECT i.Species\n\t\t\t\t\tFROM iris i\n\t\t\t\t\t);",
                    table1 = "-- # table1\nSELECT *\nFROM (\n\t\t--#all_species\n\t\tSELECT i.Species\n\t\tFROM iris i\n)",
                    another_filtered_data = "--#another_filtered_data \nSELECT *\nFROM iris i\nWHERE i.Species IN (\n\t\t\t\t\t-- # table1\n\t\t\t\t\tSELECT *\n\t\t\t\t\tFROM (\n\t\t\t\t\t\t\t--#all_species\n\t\tSELECT i.Species\n\t\tFROM iris i\n\t\t\t\t\t)\n\t\t\t\t\t);"))
})

test_that("remove_chosen_existing_queries removes query from dt", {
  sql_with_pipe <- c("-- #get_all", "SELECT *", "FROM iris;", "", "--#all_species",
                     "SELECT i.Species", "FROM iris i;", "", "--#filtered_data", "SELECT *",
                     "FROM iris i", "WHERE i.Species IN (", "\t\t\t\t\t-- |> all_species",
                     "\t\t\t\t\t);", "", "-- # table1", "SELECT *", "FROM (", "\t\t-- |> all_species",
                     ")", "", "--#another_filtered_data ",
                     "SELECT *", "FROM iris i", "WHERE i.Species IN (", "\t\t\t\t\t-- |> table1",
                     "\t\t\t\t\t);")
  sql_with_pipe <- data.table(query = sql_with_pipe,
                              group = NA_integer_,
                              nested_query = NA_integer_)
  queries_df <- mark_separate_queries(sql_with_pipe)
  queries_df <- mark_nested_queries(queries_df, get_queries_names(sql_with_pipe$query))
  queries_df <- remove_chosen_existing_queries(c("get_all", "table1"), queries_df)
  expect_equal(queries_df,
               structure(list(query = c("--#all_species", "SELECT i.Species",
                                        "FROM iris i;", "--#filtered_data", "SELECT *", "FROM iris i",
                                        "WHERE i.Species IN (", "\t\t\t\t\t-- |> all_species", "\t\t\t\t\t);",
                                        "--#another_filtered_data ", "SELECT *", "FROM iris i", "WHERE i.Species IN (",
                                        "\t\t\t\t\t-- |> table1", "\t\t\t\t\t);"),
                              group = c(2L, 2L, 2L, 3L, 3L, 3L, 3L, 3L, 3L, 5L, 5L, 5L, 5L, 5L, 5L),
                              nested_query = c(NA, NA, NA, NA, NA, NA, NA, 2L, NA, NA, NA, NA, NA, 4L, NA)),
                         row.names = c(NA, -15L), class = c("data.table", "data.frame")))
})
