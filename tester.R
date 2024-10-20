box::use(
  app/logic/api[...], 
  app/logic/collection[...],
  app/logic/api_helpers[...],
)

douban_movie <- api(35882838, domain = "douban", schema = "movie")
douban_book <- api(36879010, domain = "douban", schema = "book")
douban_music <- api(35393951, domain = "douban", schema = "music")
douban_game <- api(11639331, domain = "douban", schema = "game")
bangumi <- api(321885, domain = "bangumi")

database <- collection("database")
imported <- collection("import", "app/static/豆伴(58485907)_2.xlsx")
r <- request(douban_book)
h <- httr2::resp_body_html(r)
  
res <- xpaths$douban$book |> 
  purrr::map(~fetch_douban_book(h, .x)) |>
  purrr::list_flatten(name_spec = "{inner}")
res$id <- NA_integer_
res$subject_id <- as.character(douban_book$id)
res$type <- douban_book$schema
res$status <- NA_character_
res$my_rating <- NA_real_
res$url <- r$url
res$created_at <- lubridate::force_tz(lubridate::now(), "UTC")
dplyr::as_tibble(res) |>
  dplyr::select(id, subject_id, type, title, cover, year, author,
                publisher, status, rating, my_rating,url, created_at)

# fetch_douban_movie
