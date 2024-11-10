# box::use(
#   app/logic/api[api], 
#   app/logic/request[request], 
#   app/logic/fetch[fetch]
# )
# 
# test_that("api class", {
#   u <- "https://bangumi.tv/subject/321885"
#   u2 <- "https://movie.douban.com/subject/3792799/"
#   u3 <- "https://www.douban.com/game/11639331/"
#   expect_s3_class(api(u), "api")
#   expect_s3_class(api(u), "bangumi")
#   expect_s3_class(api(u2), "douban")
#   expect_s3_class(api(u3), "douban")
#   expect_failure(expect_s3_class(api(u), c("douban")))
#   expect_error(api(3218885))
#   expect_error(api(3218885, domain = "domain"))
#   expect_error(api(3218885, domain = "douban"), "empty schema")
#   expect_error(api(3218885, domain = "douban", schema = "schema"), "Out-of-scope schema")
#   expect_s3_class(api(3218885, domain = "douban", schema = "movie"), "api")
# })
