rstudio_theme_mode <- "light"
connection <- DBI::dbConnect(drv = , )
future::plan("multisession")
shiny::onStop(function() {
  DBI::dbDisconnect(connection)
})

app_to_run <- shiny::shinyApp(sqlviewer:::set_ui(rstudio_theme_mode),
                              sqlviewer:::set_server(connection),
                              options = list(launch.browser = FALSE,
                                             host = ,
                                             port = ))

shiny::runApp(app_to_run)
