#' Create 'server' Function for App
#'
#' @param conn connection to database
#' @param clipboard_mode should we use clipr::write_clipboard or JS to write to clipboard?
#'
#' @return
#' `shiny` server function.
#' @details
#' To use server object in `shiny::shinyApp` we need
#' to return function.
#' @import shiny
#' @noRd
set_server <- function(conn, clipboard_mode) {
  function(input, output, session) {
    tbl_preview_server("tbl_preview", conn, clipboard_mode,
                       observe_clipboard = reactive({input$observe_clipboard}),
                       js_clipboard = reactive({input$js_clipboard}),
                       copy_query = reactive({input$copy_query}),
                       remove_query = reactive({input$remove_query}),
                       show_result = reactive({input$show_result}),
                       hide_result = reactive({input$hide_result}))
  }
}
