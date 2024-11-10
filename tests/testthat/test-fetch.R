box::use(
  app/logic/fetch[fetch],
  app/logic/api[api]
)

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
