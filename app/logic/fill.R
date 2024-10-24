box::use(
  furrr[future_map, future_pmap_chr, furrr_options],
  stringr[str_detect, str_split, str_replace, str_which],
  rvest[read_html, html_element, html_text2, html_attr],
  blogdown[read_toml], 
  DBI[dbConnect, dbDisconnect], 
  duckdb[duckdb],
  assertthat[assert_that], 
  dplyr[tbl, select, mutate, collect, left_join]
)

#' @export
fill <- function(x, ...) {
  UseMethod("fill")
}

fill_site <- function(site_url, api_url, html_string = NULL) {
  if(str_detect(api_url, "movie")) {
    xpath <- read_toml("app/static/headers.toml")$xpath$douban$movie$info
    info <- read_html(html_string) |>
      html_element(xpath = xpath) |> 
      html_text2() |>
      str_split("\\n", simplify = TRUE)
    site <- info[str_which(info, "^官方网站:\\s(.+)")] |>
      str_replace("^.+:\\s", "")
    site <- ifelse(length(site) != 0, site, NA_character_)
  }
  site
}

fill_cover <- function(cover_url, api_url, html_string = NULL) {
  xpaths <- read_toml("app/static/headers.toml")$xpaths$douban
  if(str_detect(api_url, "movie")) {
    xpath <- xpaths$movie$cover
  } else if(str_detect(api_url, "book")) {
    xpath <- xpaths$book$cover
  } else if(str_detect(api_url, "music")) {
    xpath <- xpaths$music$cover
  } else if(str_detect(api_url, "game")) {
    xpath <- xpaths$game$cover
  } else {
    stop("invalid xpath", call. = FALSE)
  }
  
  if(is.na(cover_url) && !is.null(html_string)) {
    read_html(html_string) |>
      html_element(xpath = xpath) |> 
      html_attr("src")
  } else cover_url
}

fill_collection_tbl <- function(x, y) {
  out <- left_join(x, y, by = "url")
  if("site" %in% colnames(x)) {
    out |>
      mutate(cover = mapply(fill_cover, cover_url = cover, api_url = url, html_string = html),
                    site = mapply(fill_site, site_url = site, api_url = url, html_string = html)) |>
      select(-html)
  } else {
    out |>
      mutate(cover = mapply(fill_cover, cover_url = cover, api_url = url, html_string = html)) |>
      select(-html)
  }
}


fill.collection <- function(collection, from = NULL, workers = 2) {
  on.exit({dbDisconnect(con)}, add = TRUE)
  dbpath <- "app/static/requests.duckdb"
  if(is.null(from)) {
    assert_that("requests.duckdb" %in% list.files("app/static"))
  }
  con <- dbConnect(duckdb(dbpath))
  from <- from %||% con |>
    tbl("requests") |>
    select(-subject_id) |>
    collect()
  assert_that(inherits(from, "data.frame"), "html" %in% colnames(from))
  
  out <- lapply(collection$data, fill_collection_tbl, y = from)
  structure(list(source = collection$source, data = out), class = "collection")
}

e <- new.env()
local({
  fill.collection
  .S3method("fill", "collection")
})