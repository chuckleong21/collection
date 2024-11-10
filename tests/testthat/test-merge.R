box::use(
  app/logic/merge[merge],
  app/logic/collection[collection]
)

test_that("merge method", {
  dbdir <- "database.duckdb"
  f <- "豆伴(58485907).xlsx"
  f2 <- "豆伴(58485907)_2.xlsx"
  db <- collection(source = "database", dbdir = dbdir)
  im <- suppressWarnings(collection(source = "import", f, dbdir = dbdir))
  im2 <- suppressWarnings(collection(source = "import", f2, dbdir = dbdir))
  d <- diff(db, im)
  d2 <- diff(db, im2)
  expect_message(merge(d, db), "up to date")
  expect_message(merge(d2, db), "up to date")
})
