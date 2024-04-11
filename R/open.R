#' Run 'sqlviewer' App
#'
#' Runs `shiny` application as a background job with the functionality provided by `sqlviewer`:
#' preview SQL queries and construct complex queries using solution inspired by pipe operator.
#'
#' @param drv database driver name *with* package name (character vector length 1), e.g. `"RPostgres::Postgres`. See Details section.
#' @param ... other database driver arguments passed to `[DBI::dbConnect()]`. See that function for details.
#' @param app_host IPv4 address on which application should listen on. Defaults to `"127.0.0.1"`, i.e., localhost. Argument passed to `[shiny::shinyApp()]`.
#' @param app_port TCP port on which application should listen on. Default to `49152`. Argument passed to `[shiny::shinyApp()]`.
#'
#' @return
#' Used for side effect: to run app as a background job.
#' @details
#' To establish connection with database using `DBI::dbConnect()`, it is necessary to provide
#' database driver (as well as other arguments which are needed by specific driver). However,
#' implementation of `sqlviewer` expects that driver will be provided as a character vector length 1, not
#' a function call itself. Moreover, it is also necessary to provide package name along with the driver.
#' As an example, if one would like to connect with the PostgreSQL database and use package `RPostgres` for this,
#' then it would be necessary to pass an argument `"RPostgres::Postgres"` (notice quotation mark and lack of parenthesis).
#' Even that user should be provide character vector, it is expected that database package from which driver is used,
#' will be installed. In other words, if using `RPostgres` package, this package must be installed.
#' @section Running SQL Queries:
#' To run SQL query, simply copy statements to clipboard and `sqlviewer` will run
#' the code and display result as a table. You can copy more than one query at a time,
#' then more than one table will be displayed. If you use labeling (see Piping section below),
#' tables will be titled according to the label.
#' @section Piping:
#'
#' @export
#' @examples
#' \dontrun{
#' temp_file <- tempfile("sqlviewerDB_example", fileext = ".db")
#' conn <- DBI::dbConnect(duckdb::duckdb(), dbdir = temp_file)
#' DBI::dbWriteTable(conn, "iris", iris)
#' sqlviewer::open("duckdb::duckdb", dbdir = temp_file)
#' # Now, copy SQL statement to clipboard: SELECT * FROM iris;
#' # and see result in Viewer.
#' # To finish, press 'STOP' button in Background Jobs pane.
#' DBI::dbDisconnect(conn)
#' file.remove(temp_file)
#' }
open <- function(drv, ..., app_host = "127.0.0.1", app_port = 49152) {
  check_requirements(drv, app_host, app_port)
  temp_file <- tempfile("sqlviewer", fileext = ".R")
  pass_args_to_script(drv, ..., temp_file = temp_file, app_host = app_host, app_port = app_port, rstudio_dark_theme = rstudioapi::getThemeInfo()$dark)
  job_id <- run_bg_job(temp_file, app_host, app_port)
  Sys.sleep(1) # otherwise everything runs too quickly to go back to console
  file.remove(temp_file)
  rstudioapi::executeCommand("activateConsole")
  message_opening(app_host, app_port)
  rstudioapi::viewer(paste0("http://", app_host, ":", app_port))
  message_closing(job_id, temp_file)
}

#' Check Arguments Passed To Function.
#'
#' @param drv database driver name with package name.
#' @param app_host host on which app will be running.
#' @param app_port port on which app will be running.
#'
#' @return
#' Side effect: error or nothing.
#' @noRd
check_requirements <- function(drv, app_host, app_port) {
  if (!rstudioapi::isAvailable()) {
    stop("'sqlviewer' must be used inside RStudio IDE", call. = FALSE)
  }

  if (typeof(app_host) != "character") {
    stop("Argument passed to 'app_host' parameter must be of type character", call. = FALSE)
  }

  if (!(typeof(app_port) == "integer" | typeof(app_port) == "double")) {
    stop("Argument passed to 'app_port' parameter must be an integer", call. = FALSE)
  }

  if (app_port %% 1 != 0) {
    stop("Argument passed to 'app_port' parameter must be an integer", call. = FALSE)
  }

  if (typeof(drv) != "character") {
    stop("Argument passed to 'drv' parameter must be of type character. Did you accidentally call database driver function? Pass driver as a character with package name, e.g. 'RPostgres::Postgres' instead of RPostgres::Postgres() or Postgres()", call. = FALSE)
  }

  if (!grepl("::", drv, fixed = TRUE)) {
    stop("Argument passed to 'drv' parameter must include package name, e.g. use 'RPostgres::Postgres' instead of 'Postgres'. Make sure this package is installed on your machine.", call. = FALSE)
  }
}

#' Insert Arguments Into R Script and Write R Script To Temporary File.
#'
#' @param drv database driver name with package name.
#' @param ... arguments passed to `DBI::dbConnect()`.
#' @param temp_file path to temporary file.
#' @param app_host host on which app will be running.
#' @param app_port port on which app will be running.
#' @param rstudio_dark_theme does user uses RStudio with dark theme?
#'
#' @return
#' Side effect: writes temporary R script to be used (run)
#' by `rstudioapi::jobRunScript`.
#' @noRd
pass_args_to_script <- function(drv, ..., temp_file, app_host, app_port, rstudio_dark_theme) {
  rest_args <- list(...)
  rest_args <- paste0(unlist(mapply(function(x, y) paste0(x, " = ", '"', y, '"'),
                                    names(rest_args),
                                    rest_args),
                             use.names = FALSE),
                      collapse = ", ")
  script_code <- readLines(system.file(package = "sqlviewer", "app", "run_app_template.R"))
  script_code[[2]] <- sub("drv = , ", paste0("drv = , ", rest_args), script_code[[2]], fixed = TRUE)
  script_code[[2]] <- sub("drv = ", paste0("drv = ", gsub('"|\\\'|\\(|\\)', "", drv, perl = TRUE), "()"), script_code[[2]], fixed = TRUE)
  script_code[[11]] <- sub("host = ", paste0("host = ", '"', app_host, '"'), script_code[[11]], fixed = TRUE)
  script_code[[12]] <- sub("port = ", paste0("port = ", app_port), script_code[[12]], fixed = TRUE)
  if (rstudio_dark_theme) {
    script_code[[1]] <- sub('rstudio_theme_mode <- "light"', 'rstudio_theme_mode <- "dark"', script_code[[1]], fixed = TRUE)
  }
  writeLines(script_code, temp_file)
}

run_bg_job <- function(temp_file, app_host, app_port) {
  rstudioapi::jobRunScript(temp_file, importEnv = FALSE)
}

#' Display Message About Opening 'sqlviewer'.
#'
#' @param app_host host on which app is running.
#' @param app_port port on which app is running.
#'
#' @return
#' Side effect: displays message.
#' @noRd
message_opening <- function(app_host, app_port) {
  message("Opening: sqlviewer is running as a background job on host: ", app_host, " and port: ", app_port, ". App is displayed in Viewer pane. Visit website: http://", app_host, ":", app_port, " if you prefer web browser.\n")
}

#' Display Message About Closing 'sqlviewer'
#'
#' @param job_id job id returned by `rstudioapi::jobRunScript`.
#' @param temp_file path to temporary file.
#'
#' @return
#' Side effect: displays message.
#' @details
#' Currently it is not possible to
#' programatically close background job. There is
#' function to remove job, but it just removes
#' job from the background pane and do not close it.
#' There is also a function to change the state of the
#' job, but it also do not really close the job.
#' @noRd
message_closing <- function(job_id, temp_file) {
  message("Closing: to close sqlviewer, open 'Background Jobs' pane and push the 'STOP' red button (job id: ", job_id,  ", file name: ", basename(temp_file),") or close the main R session, so the background job will be closed as well.")
}
