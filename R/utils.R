#' Add Dependency to App
#'
#' @return
#' Side effect: adds dependency code to app.
#' @noRd
include_dependency <- function() {
  htmltools::htmlDependency(
    name = "sqlviewer-dependency",
    version = utils::packageVersion("sqlviewer"),
    package = "sqlviewer",
    src = c(file = "app/www"),
    stylesheet = "sqlviewer.css",
    script = c("show_result.js", "hide_result.js",
               "add_running_class.js", "rm_running_class.js",
               "copy_to_clipboard.js", "read_from_clipboard.js")
  )
}
