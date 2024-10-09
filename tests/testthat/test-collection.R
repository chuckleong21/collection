box::use(
  app/logic/collection[...]
)

dbdir <- "database.duckdb"
f <- "豆伴(58485907).xlsx"
f2 <- "豆伴(58485907)_2.xlsx"

test_that("Collection class", {
  # argument checks
  expect_error(collection(source = "import", dbdir = dbdir))
  expect_error(collection(c("import", "source"), dbdir = dbdir))
  expect_s3_class(
    suppressWarnings(
      collection(source = "database", dbdir = dbdir)
    ), "collection")
  expect_s3_class(
    suppressWarnings(collection(source = "import", file = f, dbdir = dbdir)
    ), "collection")
  expect_s3_class(
    suppressWarnings(collection(source = "import", file = f2, dbdir = dbdir)
    ), "collection")
  expect_s3_class(
    suppressWarnings(collection(source = c("import", "database"), file = f, dbdir = dbdir)
    ), "collection")
  expect_s3_class(
    suppressWarnings(collection(source = c("import", "database"), file = f2, dbdir = dbdir)
    ), "collection")
})
