temp_db <- fs::file_temp("sqlviewerDB_example", ext = ".db")
conn <- DBI::dbConnect(RSQLite::SQLite(), dbname = temp_db)
DBI::dbWriteTable(conn, "iris", iris)
DBI::dbDisconnect(conn)
connection <- parse(text = paste0("connection <- DBI::dbConnect(RSQLite::SQLite(), dbname = ", "'", temp_db, "')"))
shiny_app_obj <- shinyApp(set_ui(), set_server(connection, "local"))

test_that("app displays correct output when queries are copied to clipboard", {
  skip_on_cran()
  skip_if_not(interactive())
  clipr::write_clip("
--#test2
SELECT sqlite3_sleep(10000)
--#test1
SELECT * FROM iris i CROSS JOIN iris r CROSS JOIN iris m CROSS JOIN iris h WHERE i.Species IN ('setosa', 'setosa') LIMIT 1;


--#test5
SELECT *
FROM (
-- |> test4
);

--#test4
SELECT * FROM iris;

", allow_non_interactive = TRUE)
  app <- shinytest2::AppDriver$new(shiny_app_obj, name = "test-queries-names")
  app$wait_for_value(output = "tbl_preview-tbl_test1", timeout = 10000)
  app$expect_values(output = TRUE)
  app$stop()
})

test_that("app appends new query to previous queries in right place", {
  skip_on_cran()
  skip_if_not(interactive())
  clipr::write_clip("
--#test2
SELECT sqlite3_sleep(10000)
--#test1
SELECT * FROM iris i CROSS JOIN iris r CROSS JOIN iris m CROSS JOIN iris h WHERE i.Species IN ('setosa', 'setosa') LIMIT 1;
", allow_non_interactive = TRUE)
  app <- shinytest2::AppDriver$new(shiny_app_obj, name = "test-queries-appending")
  app$wait_for_value(output = "tbl_preview-tbl_test1", timeout = 10000)

  clipr::write_clip("
--#test0
SELECT *
FROM (
-- |> test4
);
--#test4
SELECT * FROM iris;
", allow_non_interactive = TRUE)
  app$wait_for_value(output = "tbl_preview-tbl_test4", timeout = 10000)
  app$expect_values(output = TRUE)
  app$stop()
})
