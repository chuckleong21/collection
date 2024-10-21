box::use(
  stringr[str_detect, str_split, str_which, str_replace,
          str_extract, str_remove_all, str_c],
  rvest[html_element, html_attr, html_text2], 
  purrr[reduce],
  utils[head],
  blogdown[read_toml]
)

box::use(
  app/logic/import_helpers[...]
)

#' @export
xpaths <- list(
  douban = list(
    movie = c(
      cover  = '//*[@id="mainpic"]/a/img', 
      title  = '//*[@id="content"]/h1/span[1]', 
      year   = '//*[@id="content"]/h1/span[2]',
      info   = '//*[@id="info"]', 
      rating = '//*[@id="interest_sectl"]/div[1]/div[2]/strong'
    ),
    book = c(
      cover  = '//*[@id="mainpic"]/a/img', 
      title  = '//*[@id="wrapper"]/h1/span', 
      info   = '//*[@id="info"]', 
      rating = '//*[@id="interest_sectl"]/div[1]/div[2]/strong'
    ),
    music = c(
      cover = '//*[@id="mainpic"]/span/a/img', 
      title = '//*[@id="wrapper"]/h1/span', 
      info = '//*[@id="info"]', 
      rating = '//*[@id="interest_sectl"]/div[1]/div[2]/strong'
    ),
    game = c(
      cover  = '//*[@class="pic"]/a/img', 
      title  = '//*[@id="content"]/h1',
      info   = '//*[@class="thing-attr"]',
      rating = '//*[@id="interest_sectl"]/div/div[2]/strong'
    )
  )
)

#' @export
headers <- function(toml = NULL) {
  toml <- toml %||% "app/static/headers.toml"
  headers <- blogdown::read_toml(toml)$headers
  purrr::map(headers$Cookie, ~list(
    Accept = headers$Accept, 
    `User-Agent` = headers$`User-Agent`,
    Cookie = .x
  ))
}

#' @export
fetch_douban_movie <- function(x, xpath) {
  if(str_detect(xpath, "img")) {
    return(html_element(x, xpath = xpath) |> 
             html_attr("src"))
  }
  if(str_detect(xpath, "info")) {
    info <- html_element(x, xpath = xpath) |> 
      html_text2() |>
      str_split("\\n", simplify = TRUE) |>
      as.vector()
    region <- info[str_which(info, "^制片国家/地区:\\s(.+)")] |>
      str_extract("^制片国家/地区:\\s?(.+)", group = 1) |>
      str_remove_all("/ ")
    genre <- info[str_which(info, "^类型:\\s(.+)")] |>
      str_extract("^类型:\\s?(.+)", group = 1) |>
      str_remove_all("/ ")
    director <- info[str_which(info, "^导演:\\s(.+)")] |>
      str_extract("^导演:\\s?(.+)", group = 1) |>
      str_remove_all("/ ")
    starring <- info[str_which(info, "^主演:\\s(.+)")] |>
      str_extract("^主演:\\s?(.+)", group = 1) |>
      str_remove_all("/ ") |> 
      str_split("\\s") |>
      reduce(c) |>
      head(2) |>
      str_c(collapse = " ")
    return(list(region = region, genre = genre, 
                director = director, starring = starring))
  }
  if(str_detect(xpath, "span\\[2\\]")) {
    return(html_element(x, xpath = xpath) |> 
             html_text2() |> 
             str_extract("\\d+"))
  }
  html_element(x, xpath = xpath) |> 
    html_text2()
}

#' @export
fetch_douban_book <- function(x, xpath) {
  if(str_detect(xpath, "img")) {
    return(html_element(x, xpath = xpath) |> 
             html_attr("src"))
  }
  if(str_detect(xpath, "info")) {
    info <- html_element(x, xpath = xpath) |>
      html_text2() |>
      str_split("\\n", simplify = TRUE) |>
      as.vector()
    year <- info[str_which(info, "^出版年:\\s(.+)")] |>
      str_extract("^出版年:\\s(\\d{4})", group = 1)
    author <- info[str_which(info, "^作者:\\s(.+)")] |>
      str_remove_all("/ ") |> 
      str_replace(".*(?:作者:\\s)", "")
    publisher <- info[str_which(info, "^出版社?:\\s(.+)")] |>
      str_remove_all("/ ") |> 
      str_replace(".*(?:出版社?:\\s)", "")
    return(list(year = year, author = author, publisher = publisher))
  }
  html_element(x, xpath = xpath) |>
    html_text2()
}

#' @export
fetch_douban_music <- function(x, xpath) {
  if(stringr::str_detect(xpath, "img")) {
    return(rvest::html_element(x, xpath = xpath) |> 
             rvest::html_attr("src"))
  }
  if(stringr::str_detect(xpath, "info")) {
    info <- html_element(x, xpath = xpath) |> 
      html_text2() |>
      str_split("\\n", simplify = TRUE) |>
      as.vector()
    performer <- info[stringr::str_which(info, "^表演者:\\s(.+)")] |>
      stringr::str_remove_all("/ ") |>
      stringr::str_replace(".*(?:表演者: )", "")
    year <- info[stringr::str_which(info, "^发行时间:\\s(.+)")] |>
      stringr::str_replace(".*(?:发行时间:\\s)", "") |>
      lubridate::as_date() |> 
      lubridate::year() |>
      as.character()
    return(list(performer = performer, year = year))
  }
  rvest::html_element(x, xpath = xpath) |> 
    rvest::html_text2()
}

#' @export
fetch_douban_game <- function(x, xpath) {
  if(stringr::str_detect(xpath, "img")) {
    return(rvest::html_element(x, xpath = xpath) |> 
             rvest::html_attr("src"))
  }
  if(stringr::str_detect(xpath, "thing")) {
    info <- rvest::html_element(x, xpath = xpath) |>
      rvest::html_text2() |>
      stringr::str_split("\\n", simplify = TRUE) |>
      as.vector()
    category <- info[stringr::str_which(info, "类型") + 1] |>
      stringr::str_extract_all(games_regex("type")) |>
      purrr::map_chr(~paste0(.x, collapse = " "))
    developer <- info[stringr::str_which(info, "开发商") + 1]
    release <- info[stringr::str_which(info, "发行日期") + 1]
    return(list(category = category, developer = developer, 
                release = release))
  }
  rvest::html_element(x, xpath = xpath) |>
    rvest::html_text2()
}