
<!-- README.md is generated from README.Rmd. Please edit that file -->

# sqlviewer

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/sqlviewer)](https://CRAN.R-project.org/package=sqlviewer)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

`{sqlviewer}` is an R package and `{shiny}` app developed to help
building SQL queries - it provides queries results previewer (by
displaying first 1000 rows) as well as piping-like possibility to
construct complex queries. The main three goals are:

- make working with SQL using R more comfortable;
- bring better debug experience by displaying results from temporary
  queries;
- help construct complex queries;

![](./sqlviewer.gif)

## Installation

You can install the development version of `{sqlviewer}` from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("gsmolinski/sqlviewer")
```

## Example

Let’s start with the sample SQLite database:

``` r
# install.packages("RSQLite") # run if package not installed already
# install.packages("fs") # run if package not installed already

temp_db <- fs::file_temp("sqlviewerDB_example", ext = ".db")
conn <- DBI::dbConnect(RSQLite::SQLite(), dbname = temp_db)
DBI::dbWriteTable(conn, "iris", iris)
DBI::dbDisconnect(conn)
```

After the database with `iris` dataset was saved in temporary location
(path is stored in `temp_db` variable), we need SQL queries to run.
`{sqlviewer}` expects that each SQL query will be named (have label),
names won’t be duplicated and will consist only of letters, numbers and
underscores. Additionally, each name (label) must be in its own line:

``` sql
-- #versicolor_virginica
SELECT DISTINCT i.Species
FROM iris i
WHERE i.Species IN ('versicolor', 'virginica');

-- #filtered_data
SELECT *
FROM iris i
WHERE i.Species IN (
   -- |> versicolor_virginica
   );
```

Above we shown not just named queries, but also how to use piping. Pipe
operator (`|>`) has to be in its own line and can be read as “here put
*this* query”.

Coming back to R, what’s left now is to run `{sqlviewer}` app, but
because the app was developed to enable parallelization, before running
app, we can run `{future}` plan.

``` r
future::plan("multisession") # enable parallel mode
sqlviewer::open(RSQLite::SQLite(), dbname = temp_db) # run 'sqlviewer' app against temporary database
```

App should be running now on the default host and port and in the
console full web address should be printed already - it can be copied to
web browser to see the app.

How to run SQL queries? They have to be copy to clipboard and if in the
app switch input is set to TRUE (i.e. is blue, not grey), queries’ names
will be inserted. To run a query, click on its name. Other possibilities
are: copy the full query (i.e. with all resolved piped queries) or
remove the query (which also copies full query to clipboard before
removing). Please note that currently background processes can’t be
stopped, so if one ran long-running query and want to stop it, it will
be necessary to restart main R session.

To add more queries, simply copy them to clipboard. If the copied name
already exists, will be replaced by new query.

## Running Locally or in the Web Browser

By default, `sqlviewer` do not open itself in IDE viewer or web browser.
To open web browser with the running app after calling
`sqlviewer::open()`, use parameter `launch_browser` set to `TRUE`:

``` r
sqlviewer::open(RSQLite::SQLite(), dbname = temp_db, launch_browser = TRUE)
```

What is important here is to note that in `sqlviewer` two modes to read
from or write back (queries with resolved piped queries) to clipboard
are possible, using `clipr::write_clip()` and `clipr::read_clip()` or
JavaScript functions. JavaScript should be used in any web-based
environment (like opening app in the web browser), while `clipr`
functions should be used where JavaScript do not work. One example of
not working JavaScript function is opening app in the local (desktop)
RStudio Viewer, so to use `sqlviewer` in the viewer locally, we need to
change two parameters:

``` r
sqlviewer::open(RSQLite::SQLite(), dbname = temp_db, clipboard_mode = "local", launch_browser = .rs.invokeShinyPaneViewer)
```
