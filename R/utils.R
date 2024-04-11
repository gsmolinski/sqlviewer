include_css_dependency <- function() {
  htmltools::htmlDependency(
    name = "sqlviewer-css",
    version = packageVersion("sqlviewer"),
    package = "sqlviewer",
    src = "app/www",
    stylesheet = "sqlviewer.css"
  )
}
