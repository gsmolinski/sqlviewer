#' Run `sqlviewer` App
#'
#' Runs `shiny` application as a background job with the functionality provided by `sqlviewer`:
#' preview SQL queries and construct complex queries using solution inspired by pipe operator.
#'
#' @param drv database driver name *with* package name (character vector length 1), e.g. `"RPostgres::Postgres"`. See *Details* section.
#' @param ... other database driver arguments passed to `[DBI::dbConnect()]`. See that function for details.
#' @param app_host IPv4 address (character vector length 1) on which application should listen on. Defaults to `"127.0.0.1"` (localhost). Argument passed to `[shiny::shinyApp()]`.
#' @param app_port TCP port (integer vector length 1) on which application should listen on. Defaults to `49152`. Argument passed to `[shiny::shinyApp()]`.
#' @param save_temp_path_to where to save *path* to temporary file? Defaults to `""`, meaning no saving. Argument passed to [base::cat()] to `file` parameter. See *Security* section for details.
#'
#' @return
#' Used for side effect: to run app as a background job.
#' @details
#' To establish connection with database using `[DBI::dbConnect()]`, it is necessary to provide
#' database driver (as well as other arguments which are needed by specific driver). However,
#' implementation of `sqlviewer` expects that driver will be provided as a character vector length 1, not
#' a function call itself. Moreover, it is also necessary to provide package name along with the driver.
#' As an example, if one would like to connect with the PostgreSQL database and use package `RPostgres` for that,
#' then it would be necessary to pass an argument: `"RPostgres::Postgres"` (notice quotation mark indicating character vector and lack of parenthesis).
#' Even that user should provide character vector, it is expected that database package from which driver is used,
#' will be installed. In other words, if using `RPostgres` package (or any other database specific package), this package must be installed on machine.
#'
#' Currently, it is not possible to construct function which will close running background job. To close the app, one
#' must go to 'Background Jobs' pane and press 'STOP' button or close the main R session, so all child R sessions
#' (including background jobs) will be closed as well.
#' @section Running SQL Queries:
#' To run SQL query, simply copy statements with labels (see *Labeling* section) to clipboard (ensure switch input to observe clipboard is on) and `sqlviewer` will run
#' the code and display result as a table. You can copy more than one query at a time, then more than one table will be displayed.
#' @section Labeling:
#' Each SQL query needs to have label (name). Label **has to be in its own line** (i.e. nothing more should exist in the same line except of intendation)
#' and have following format:
#' \preformatted{
#' -- #label
#' }
#' where instead of `label` should be unique query name (see *Example* section below or *Piping* section). **To be valid, label must be
#' constructed using only: letters, numbers and underscores.**
#' Label can be used later for piping and will be used when displaying results of queries in the app.
#' @section Piping:
#' SQL queries can be very complex, especially when using nested queries. `sqlviewer` comes with the functionality to pipe one
#' labelled query into another query using `|>` operator. Below is an example of how to pipe one query into another query:
#' \preformatted{
#' -- #all_species
#' SELECT i.Species
#' FROM iris i;
#'
#' -- #filtered_data
#' SELECT *
#' FROM iris i
#' WHERE i.Species IN (
#'    -- |> all_species
#'    );
#' }
#' Pipe operator **has to be in its own line** (i.e. nothing more should exist in the same line except indentation) and can be read as
#' "here put *this* query". `sqlviewer` will analyze the code and insert labelled queries *as-is* (query is not computed, just inserted)
#' in the line where pipe operator was used. It is not necessary to write queries from top to bottom, i.e. nested labelled query
#' can be below query into which this nested query will be piped.
#' @section App Functionality:
#' Switch button is used to indicate if clipboard should be observed. If set to off, then all
#' existing queries will be removed and no new queries will be created. To see result for chosen query,
#' click on its name. To copy query - click copy button - and to remove query, click remove button. When
#' query is removed, it is also copied to clipboard.
#'
#' `sqlviewer` displays only first 1000 rows of table. If some names of queries are duplicated, then app won't run.
#' @section Security:
#' User should be aware that password to database passed as an argument is stored
#' in temporary file (R script) as a plain text, i.e. even if password was passed as a variable, in
#' temporary file value of this variable will be used. This is needed to run script as
#' background job and immediately after running background job, temporary file is removed.
#' However, it may happen due to some errors during function execution (e.g. not reaching the code line which removes
#' file, because machine was shut down during code execution) that file won't be removed. To know where
#' the R script was saved (to be able to remove it manually later), one can pass character vector length 1 argument to
#' `save_temp_path_to` parameter - temporary path will be saved there.
#' @export
#' @examples
#' \dontrun{
#' temp_db <- tempfile("sqlviewerDB_example", fileext = ".db")
#' conn <- DBI::dbConnect(duckdb::duckdb(), dbdir = temp_db)
#' DBI::dbWriteTable(conn, "iris", iris)
#' DBI::dbDisconnect(conn)
#' sqlviewer::open("duckdb::duckdb", dbdir = temp_db)
#' # Now, copy SQL statement to clipboard (without comment signs!) and include label (-- #all_data)
#' #
#' # -- #all_data
#' # SELECT * FROM iris;
#' #
#' # and see result in Viewer.
#' # To finish, press 'STOP' button in Background Jobs pane.
#' file.remove(temp_db)
#' }
open <- function(drv, ..., app_host = "127.0.0.1", app_port = 49152, save_temp_path_to = "") {
  check_requirements(drv, app_host, app_port)
  temp_file <- tempfile("sqlviewer", fileext = ".R")
  save_temporary_path(temp_file, save_temp_path_to)
  pass_args_to_script(drv, ..., temp_file = temp_file, app_host = app_host, app_port = app_port, rstudio_dark_theme = rstudioapi::getThemeInfo()$dark)
  job_id <- run_bg_job(temp_file, app_host, app_port)
  file.remove(temp_file)
  Sys.sleep(1) # otherwise everything runs too quickly to go back to console
  rstudioapi::executeCommand("activateConsole")
  rstudioapi::viewer(paste0("http://", app_host, ":", app_port))
  message_opening(app_host, app_port)
  message_closing(job_id, temp_file)
}

#' Check Arguments Passed To Function.
#'
#' @param drv database driver name with package name.
#' @param app_host host on which app will be running.
#' @param app_port port on which app will be running.
#' @param save_temp_path_to path to where save path to temp file.
#'
#' @return
#' Side effect: error or nothing.
#' @noRd
check_requirements <- function(drv, app_host, app_port, save_temp_path_to) {
  if (!rstudioapi::isAvailable()) {
    stop("{sqlviewer} must be used inside RStudio IDE", call. = FALSE)
  }

  if (is.na(app_host) || typeof(app_host) != "character") {
    stop("Argument passed to 'app_host' parameter must be of type character", call. = FALSE)
  }

  if (is.na(app_host) || !(typeof(app_port) == "integer" || typeof(app_port) == "double")) {
    stop("Argument passed to 'app_port' parameter must be an integer", call. = FALSE)
  }

  if (app_port %% 1 != 0) {
    stop("Argument passed to 'app_port' parameter must be an integer", call. = FALSE)
  }

  if (is.na(drv) || typeof(drv) != "character") {
    stop("Argument passed to 'drv' parameter must be of type character. Did you accidentally call database driver function? Pass driver as a character with package name, e.g. 'RPostgres::Postgres' instead of RPostgres::Postgres() or Postgres()", call. = FALSE)
  }

  if (!grepl("::", drv, fixed = TRUE)) {
    stop("Argument passed to 'drv' parameter must include package name, e.g. use 'RPostgres::Postgres' instead of 'Postgres'. Make sure this package is installed on your machine.", call. = FALSE)
  }

  if (is.na(save_temp_path_to) || typeof(save_temp_path_to) != "character") {
    stop("Argument passed to 'save_temp_path_to' must be of type character", .call = FALSE)
  }

  if (!missing(save_temp_path_to)) {
    if (nchar(save_temp_path_to) == 0) {
      stop("Argument passed to 'save_temp_path_to' must have at lease one character", .call = FALSE)
    }
  }
}

#' Save Path To Temporary File
#'
#' @param temp_file path to created temporary file.
#' @param save_temp_path_to where to save info about path to temporary file?
#'
#' @return
#' Side effect: save text to file.
#' @noRd
save_temporary_path <- function(temp_file, save_temp_path_to) {
  if (nchar(save_temp_path_to) > 0) {
    cat(paste0(format(Sys.time(), usetz = TRUE), ". {sqlviewer} created temporary R script here: ", temp_file),
               file = save_temp_path_to, sep = "\n", append = TRUE)
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
  message("Opening: {sqlviewer} is running as a background job on host: ", app_host, " and port: ", app_port, ". App is displayed in Viewer pane. Visit website: http://", app_host, ":", app_port, " if you prefer web browser.\n")
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
  message("Closing: to close {sqlviewer}, open 'Background Jobs' pane and push the 'STOP' red button (job id: ", job_id,  ", file name: ", basename(temp_file),") or close the main R session, so the background job will be closed as well.")
}
