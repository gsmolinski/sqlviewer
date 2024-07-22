# future::plan("multisession")
# devtools::load_all()
# open(RSQLite::SQLite(), "~/test2.db")
#
# #mydb <- DBI::dbConnect(RSQLite::SQLite(), dbname = "~/test2.db")
# #DBI::dbWriteTable(mydb, "iris", dplyr::bind_rows(iris, iris, iris, iris, iris, iris, iris, iris, iris, iris, iris))
# #DBI::dbDisconnect(mydb)
#
# # TODO: przenieść tabelę z queries do resolved nested queries do reactive value
