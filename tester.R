box::use(
  app/logic/api[...], 
  app/logic/collection[...],
  app/logic/api_helpers[...],
)

bangumi <- api(321885, domain = "bangumi")

database <- collection("database", dbdir = "tests/testthat/database.duckdb")
imported <- collection("import", "app/static/豆伴(58485907)_2.xlsx")
d <- diff(database, imported)
merge(d, database)

# fetch_douban_movie
