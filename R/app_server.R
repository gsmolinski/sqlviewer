#' Create 'server' Function for App
#'
#' @param conn
#'
#' @return
#' `shiny` server function.
#' @details
#' To use server object in `shiny::shinyApp` we need
#' to return function.
#' @import shiny
#' @noRd
set_server <- function(conn) {
  function(input, output, session) {
    tbl_preview_server("tbl_preview", conn,
                       observe_clipboard = reactive({input$observe_clipboard}))
    onStop(function() {
      DBI::dbDisconnect(conn)
    })
  }
}
