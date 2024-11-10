box::use(
  app/logic/request[request],
  app/logic/api[api]
)

test_that("request method for API class", {
  u2 <- "https://movie.douban.com/subject/3792799/"
  u3 <- "https://www.douban.com/game/11639331/"
  expect_s3_class(request(api(u2), header_toml = "headers.toml"), "httr2_response")
  expect_s3_class(request(api(u3), header_toml = "headers.toml"), "httr2_response")
})
