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

fetch(douban_movie)
fetch(douban_book)
fetch(douban_music)
fetch(douban_game)

database <- collection("database")
imported <- collection("import", "app/static/豆伴(58485907)_2.xlsx")


# fetch_douban_movie
