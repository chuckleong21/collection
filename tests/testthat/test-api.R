box::use(
  app/logic/api[...]
)

test_that("api class", {
  u <- "https://bangumi.tv/subject/321885"
  u2 <- "https://movie.douban.com/subject/3792799/"
  u3 <- "https://www.douban.com/game/11639331/"
  expect_s3_class(api(u), "api")
  expect_s3_class(api(u), "bangumi")
  expect_s3_class(api(u2), "douban")
  expect_s3_class(api(u3), "douban")
  expect_failure(expect_s3_class(api(u), c("douban")))
  expect_error(api(3218885))
  expect_error(api(3218885, domain = "domain"))
  expect_error(api(3218885, domain = "douban"), "empty schema")
  expect_error(api(3218885, domain = "douban", schema = "schema"), "Out-of-scope schema")
  expect_s3_class(api(3218885, domain = "douban", schema = "movie"), "api")
})

test_that("request method for API class", {
  u2 <- "https://movie.douban.com/subject/3792799/"
  u3 <- "https://www.douban.com/game/11639331/"
  expect_s3_class(request(api(u2), header_toml = "headers.toml"), "httr2_response")
  expect_s3_class(request(api(u3), header_toml = "headers.toml"), "httr2_response")
})

test_that("fetch method for different schemas in douban API class", {
  xpath <- blogdown::read_toml("headers.toml")$xpaths$douban
  douban_movie <- api(35882838, domain = "douban", schema = "movie")
  douban_movie2 <- api(36152618, domain = "douban", schema = "movie")
  douban_book <- api(36879010, domain = "douban", schema = "book")
  douban_music <- api(35393951, domain = "douban", schema = "music")
  douban_game <- api(11639331, domain = "douban", schema = "game")
  expect_s3_class(fetch(douban_movie, header_toml = "headers.toml", xpath = xpath), "data.frame")
  expect_s3_class(fetch(douban_movie2, header_toml = "headers.toml", xpath = xpath), "data.frame")
  expect_s3_class(fetch(douban_book, header_toml = "headers.toml", xpath = xpath), "data.frame")
  expect_s3_class(fetch(douban_game, header_toml = "headers.toml", xpath = xpath), "data.frame")
  expect_s3_class(fetch(douban_music, header_toml = "headers.toml", xpath = xpath), "data.frame")
})
