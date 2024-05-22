test_that("prepare_content_to_evaluate recognizes correctly label in query", {
  expect_true(prepare_content_to_evaluate(c("", "--# fgh")))
  expect_true(prepare_content_to_evaluate(c("", "--# 123][x./fgh")))
  expect_false(prepare_content_to_evaluate(c("fgh", "--# 123][x./fgh")))
  expect_false(prepare_content_to_evaluate(c("      sfd", "--# 123][x./fgh")))
  expect_true(prepare_content_to_evaluate(c("      ", "--# 123][x./fgh")))
})

test_that("get_queries_names returns names of queries correctly", {
  sql_queries <- c("--# get_all", "SELECT *", "FROM iris;", "", "--#table1", "SELECT Species, Species FROM iris WHERE Species LIKE '%setosa%';",
                   "", "-- # table2", "", "SELECT Species", "FROM iris i", "WHERE i.Species = 'setosa' OR ",
                   "\ti.Species IN ('versicolor');")
  get_queries_names(sql_queries)
  expect_equal(get_queries_names(sql_queries), c("get_all", "table1", "table2"))
})

test_that("divide_into_named_queries correctly separates queries without pipes", {
  sql_queries <- c("--# get_all", "SELECT *", "FROM iris;", "", "--#table1", "SELECT Species, Species FROM iris WHERE Species LIKE '%setosa%';",
                   "", "-- # table2", "", "SELECT Species", "FROM iris i", "WHERE i.Species = 'setosa' OR ",
                   "\ti.Species IN ('versicolor');")
  result <- divide_into_named_queries(sql_queries)$named_queries
  expect_equal(result,
               list(get_all = "--# get_all\nSELECT *\nFROM iris;\n",
                    table1 = "--#table1\nSELECT Species, Species FROM iris WHERE Species LIKE '%setosa%';\n",
                    table2 = "-- # table2\n\nSELECT Species\nFROM iris i\nWHERE i.Species = 'setosa' OR \n\ti.Species IN ('versicolor');")
               )
})

test_that("queries work after cleaning without pipes", {
  temp_db <- tempfile("sqlviewerDB_example", fileext = ".db")
  conn <- DBI::dbConnect(duckdb::duckdb(), dbdir = temp_db)
  DBI::dbWriteTable(conn, "iris", iris)
  sql_queries <- c("--# get_all", "SELECT *", "FROM iris;", "", "--#table1", "SELECT Species, Species FROM iris WHERE Species LIKE '%setosa%';",
                   "", "-- # table2", "", "SELECT Species", "FROM iris i", "WHERE i.Species = 'setosa' OR ",
                   "\ti.Species IN ('versicolor');")
  queries <- divide_into_named_queries(sql_queries)$named_queries
  results <- lapply(queries, \(x) DBI::dbGetQuery(conn, x))
  purrr::walk(results, ~ expect_true(nrow(.x) > 0))
})
