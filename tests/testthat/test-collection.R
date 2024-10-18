box::use(
  app/logic/collection[...]
)

dbdir <- "database.duckdb"
f <- "豆伴(58485907).xlsx"
f2 <- "豆伴(58485907)_2.xlsx"
db <- collection(source = "database", dbdir = dbdir) 
im <- collection(source = "import", f, dbdir = dbdir)
im2 <- collection(source = "import", f2, dbdir = dbdir)

test_that("Collection class", {
  # argument checks
  expect_error(collection(source = "import", dbdir = dbdir))
  expect_error(collection(c("import", "source"), dbdir = dbdir))
  expect_s3_class(db, "collection")
  expect_s3_class(im, "collection")
  expect_s3_class(im2, "collection")
  expect_s3_class(
    collection(source = c("import", "database"), file = f, dbdir = dbdir), "collection")
  expect_s3_class(
    collection(source = c("import", "database"), file = f2, dbdir = dbdir), "collection")
})

d <- diff(db, im)
d2 <- diff(db, im2)

test_that("diff method", {
  expect_s3_class(d, "collection_diff")
  expect_true(all(purrr::map_vec(d$diff, ~any((names(.x) %in% "branch")))))
})

test_that("merge method", {
  expect_message(merge(d, db), "updated \\d+? records")
  expect_message(merge(d2, db), "updated \\d+? records")
})
