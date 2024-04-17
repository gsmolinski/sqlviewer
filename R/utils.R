#' Add CSS Dependency to App
#'
#' @return
#' Side effect: adds css code to app.
#' @noRd
include_css_dependency <- function() {
  htmltools::htmlDependency(
    name = "sqlviewer-css",
    version = packageVersion("sqlviewer"),
    package = "sqlviewer",
    src = "app/www",
    stylesheet = "sqlviewer.css"
  )
}

#' Include JS Script To Read From Clipboard
#'
#' @return
#' Side effect: include JavaScript script.
#' @noRd
include_js_read_clipboard <- function() {
  htmltools::htmlDependency(
    name = "sqlviewer-read-clipboard",
    version = packageVersion("sqlviewer"),
    package = "sqlviewer",
    src = "app/www",
    script = "read_clipboard.js"
  )
}

#' Include JS Script to Write Into Clipboard
#'
#' @return
#' Side effect: include JavaScript script.
#' @noRd
include_js_write_clipboard <- function() {
  htmltools::htmlDependency(
    name = "sqlviewer-write_clipboard",
    version = packageVersion("sqlviewer"),
    package = "sqlviewer",
    src = "app/www",
    script = "write_clipboard.js"
  )
}
