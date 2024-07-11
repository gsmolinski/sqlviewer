# future::plan("multisession")
# devtools::load_all()
# rstudio_theme_mode <- "light"
# temp_db <- fs::file_temp("sqlviewerDB_example", ext = ".db")
# connection <- DBI::dbConnect(duckdb::duckdb(), dbdir = temp_db)
# DBI::dbWriteTable(connection, "iris", dplyr::bind_rows(iris, iris, iris, iris, iris, iris, iris, iris, iris, iris, iris))
# shiny_app <- shinyApp(set_ui(rstudio_theme_mode), set_server(expression(connection <- DBI::dbConnect(duckdb::duckdb(), dbdir = "~/test.db"))), options = list(launch.browser = FALSE, port = 4877))
# rstudioapi::viewer("http://127.0.0.1:4877")
# runApp(shiny_app)

# DBI::dbDisconnect(connection)
