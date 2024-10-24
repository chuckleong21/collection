#' @export
fill <- function(x, ...) {
  UseMethod("fill")
}

fill_site <- function(site_url, api_url, html_string = NULL) {
  if(stringr::str_detect(api_url, "movie")) {
    xpath <- blogdown::read_toml("app/static/headers.toml")$xpath$douban$movie$info
    info <- rvest::read_html(html_string) |>
      rvest::html_element(xpath = xpath) |> 
      rvest::html_text2() |>
      stringr::str_split("\\n", simplify = TRUE)
    site <- info[stringr::str_which(info, "^官方网站:\\s(.+)")] |>
      stringr::str_replace("^.+:\\s", "")
    site <- ifelse(length(site) != 0, site, NA_character_)
  }
  site
}

fill_cover <- function(cover_url, api_url, html_string = NULL) {
  xpaths <- blogdown::read_toml("app/static/headers.toml")$xpaths$douban
  if(stringr::str_detect(api_url, "movie")) {
    xpath <- xpaths$movie$cover
  } else if(stringr::str_detect(api_url, "book")) {
    xpath <- xpaths$book$cover
  } else if(stringr::str_detect(api_url, "music")) {
    xpath <- xpaths$music$cover
  } else if(stringr::str_detect(api_url, "game")) {
    xpath <- xpaths$game$cover
  } else {
    stop("invalid xpath", call. = FALSE)
  }
  
  if(is.na(cover_url) && !is.null(html_string)) {
    rvest::read_html(html_string) |>
      rvest::html_element(xpath = xpath) |> 
      rvest::html_attr("src")
  } else cover_url
}

fill.collection <- function(collection, from = NULL) {
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  dbpath <- "app/static/requests.duckdb"
  if(is.null(from)) {
    assertthat::assert_that("requests.duckdb" %in% list.files("app/static"))
  }
  con <- DBI::dbConnect(duckdb::duckdb(dbpath))
  from <- from %||% con |>
    dplyr::tbl("requests") |>
    dplyr::collect() |>
    dplyr::select(-subject_id)
  assertthat::assert_that(inherits(from, "data.frame"), "html" %in% colnames(from))
  
  out <- purrr::map(collection$data, \(x) {
    d <- dplyr::left_join(x, from, by = "url")
    if("site" %in% colnames(x)) {
      dplyr::mutate(d, cover = purrr::pmap_vec(list(cover, url, html), fill_cover), 
                    site = purrr::pmap_vec(list(site, url, html), fill_site)) |>
        dplyr::select(-html)
    } else {
      dplyr::mutate(d, cover = purrr::pmap_vec(list(cover, url, html), fill_cover)) |>
        dplyr::select(-html)
    }
  })
  structure(out, class = "collection")
}

e <- new.env()
local({
  fill.collection
  .S3method("fill", "collection")
})