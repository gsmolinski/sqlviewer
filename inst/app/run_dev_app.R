devtools::load_all()
rstudio_theme_mode <- "dark"
temp_db <- tempfile("sqlviewerDB_example", fileext = ".db")
connection <- DBI::dbConnect(duckdb::duckdb(), dbdir = temp_db)
DBI::dbWriteTable(connection, "iris", iris)
shiny_app <- shinyApp(sqlviewer:::set_ui("dark"), sqlviewer:::set_server(connection), options = list(launch.browser = FALSE, port = 4877))
rstudioapi::viewer("http://127.0.0.1:4877")
runApp(shiny_app)

