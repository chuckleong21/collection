box::use(
  stringr[str_detect, str_split, str_which, 
          str_extract, str_remove_all, str_c],
  rvest[html_element, html_attr, html_text2], 
  purrr[reduce],
  utils[head]
)

#' @export
xpaths <- list(
  douban = list(
    movie = c(
      cover = '//*[@id="mainpic"]/a/img', 
      title = '//*[@id="content"]/h1/span[1]', 
      year = '//*[@id="content"]/h1/span[2]',
      info = '//*[@id="info"]', 
      rating = '//*[@id="interest_sectl"]/div[1]/div[2]/strong'
    )
  )
)

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


           