future::plan("multisession")
devtools::load_all()
#mydb <- DBI::dbConnect(RSQLite::SQLite(), dbname = "~/test2.db")
#DBI::dbWriteTable(mydb, "iris", dplyr::bind_rows(iris, iris, iris, iris, iris, iris, iris, iris, iris, iris, iris))
open(RSQLite::SQLite(), "~/test2.db")
#DBI::dbDisconnect(mydb)

#TODO: Pomyśleć, czy można zrezygnować z RStudio IDE jako warunku.
