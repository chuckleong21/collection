box::use(
  app/logic/api[...],
  app/logic/request[request],
  app/logic/fetch[fetch],
  app/logic/merge[merge],
  app/logic/fill[fill],
  app/logic/collection[...],
  app/logic/api_helpers[...],
  app/logic/branch[branch]
)

# from import to database:
# 1. load database
# 2. read import
# 3. calculate diff
# 4. request collection_diff ->
#  4.1 skip 4 if requests database is up to date
# 5. update requests database -> skip 5 if 4.1
#   5.1 disconnect requests database
#   5.2 update zip file
# 6. merge diff
# 7. fill diff
# 8. update main database
# 9. returns the updated database

import_to_database <- function(file, dbdir = NULL) {
  dbdir <- dbdir %||% "app/static/database.duckdb"
  has_changed <- function(x, y) {
    bool <- purrr::map2(x$data, y$data, dplyr::setequal) |>
      purrr::reduce(c)
    all(!bool)
  }
  database <- collection("database")
  message(sprintf('reading file "%s"...', file))
  imported <- suppressWarnings(collection("import", file))
  message("checking diff between import and database...")
  d <- diff(database, imported)
  rd <- request(d)
  if(nrow(rd) != 0) {
    con <- DBI::dbConnect(duckdb::duckdb("app/static/requests.duckdb"))
    requests <- con |>
      dplyr::tbl("requests") |>
      dplyr::collect()
    dplyr::rows_upsert(requests, rd, "url") |>
      DBI::dbWriteTable(con, "requests", "url", overwrite = TRUE)
    DBI::dbDisconnect(con)
    zip("requests.zip", "app/static/requests.duckdb")
  }
  
  message("merging import with database...")
  m <- merge(d, database)
  if(has_changed(database, m)) {
    message("completing collection...")
    f <- fill(m)
    write_collection(f, "database", dbdir = dbdir)
    message(sprintf("updated database %s", dbdir))
  }
  database <- collection("database")
  database
}

import_to_database("app/static/豆伴(58485907)_2.xlsx")
