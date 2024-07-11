# future::plan("multisession")
# devtools::load_all()
# rstudio_theme_mode <- "light"
# temp_db <- fs::file_temp("sqlviewerDB_example", ext = ".db")
# connection <- DBI::dbConnect(duckdb::duckdb(), dbdir = temp_db)
# DBI::dbWriteTable(connection, "iris", dplyr::bind_rows(iris, iris, iris, iris, iris, iris, iris, iris, iris, iris, iris))
# shiny_app <- shinyApp(set_ui(rstudio_theme_mode), set_server(connection), options = list(launch.browser = FALSE, port = 4877))
# rstudioapi::viewer("http://127.0.0.1:4877")
# runApp(shiny_app)

# DBI::dbDisconnect(connection)

# rstudio_theme_mode <- "light"
# connection <- DBI::dbConnect(drv = duckdb::duckdb(), dbdir = "~/test22.db")
# future::plan("multisession")
# shiny::onStop(function() {
#   DBI::dbDisconnect(connection)
# })
#
# app_to_run <- shiny::shinyApp(sqlviewer:::set_ui(rstudio_theme_mode),
#                               sqlviewer:::set_server(connection),
#                               options = list(launch.browser = FALSE,
#                                              host = "127.0.0.1",
#                                              port = 49152))
#
# shiny::runApp(app_to_run)
