# devtools::load_all()
# devtools::install()
# remove.packages("sqlviewer")
# sqlviewer:::open("RPostgres::Postgres", dbname = "dvdrental", host = "localhost", port = 5432, user = "postgres", password = "")

# devtools::load_all()
# rstudio_theme_mode <- "dark"
# connection <- DBI::dbConnect(RPostgres::Postgres(), dbname = "dvdrental", host = "localhost", port = 5432, user = "postgres", password = "")
#
# shiny_app <- shinyApp(sqlviewer:::set_ui("dark"), sqlviewer:::set_server(connection), options = list(launch.browser = FALSE, port = 4877))
# rstudioapi::viewer("http://127.0.0.1:4877")
# runApp(shiny_app)

