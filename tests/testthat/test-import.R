source("app/logic/read_import.R")

test_that("read_import expects a xlsx file", {
  expect_error(read_import("app/static/favicon.ico"))
})

test_that("read_import returns a tibble", {
  imports <- suppressWarnings(read_import("app/static/豆伴(58485907).xlsx"))
  expect_s3_class(imports, c("tbl_df", "data.frame"))
})

