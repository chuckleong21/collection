box::use(
  app/logic/collection[collection, diff.collection, write_collection],
  app/logic/merge[merge],
)

dbdir <- "database.duckdb"
f <- "豆伴(58485907).xlsx"
f2 <- "豆伴(58485907)_2.xlsx"
db <- collection(source = "database", dbdir = dbdir)
im <- suppressWarnings(collection(source = "import", f, dbdir = dbdir))
im2 <- suppressWarnings(collection(source = "import", f2, dbdir = dbdir))
d <- diff(db, im)
d2 <- diff(db, im2)

test_that("Collection class", {
  # argument checks
  expect_error(collection(source = "import", dbdir = dbdir))
  expect_error(collection(c("import", "source"), dbdir = dbdir))
  expect_s3_class(db, "collection")
  expect_s3_class(im, "collection")
  expect_s3_class(im2, "collection")
  expect_s3_class(
    suppressWarnings(collection(source = c("import", "database"), file = f, dbdir = dbdir)), "collection")
  expect_s3_class(
    suppressWarnings(collection(source = c("import", "database"), file = f2, dbdir = dbdir)), "collection")
})

test_that("diff method", {
  expect_s3_class(d, "collection_diff")
  expect_true(all(purrr::map_vec(d$diff, ~any((names(.x) %in% "branch")))))
})

test_that("write_collection", {
  on.exit({
    file.remove("collection.xlsx")
    file.remove("merge.duckdb")
  }, add = TRUE, after = FALSE)
  m <- merge(d2, db)
  expect_error(write_collection(d, c("worksheet"), file = "collection.xlsm"),
               "collection does not inherit from class collection")
  expect_error(write_collection(m, c("worksheet", "database")),
               "argument \"to\" is either \"worksheet\" or \"database\"")
  expect_error(write_collection(m, c("worksheet"), file = "collection.xlsm"),
               "not TRUE")
  expect_silent(write_collection(m, "worksheet", file = "collection.xlsx"))
  expect_silent(write_collection(m, "database", dbdir = "merge.duckdb"))
})
