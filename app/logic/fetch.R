box::use(
  blogdown[read_toml], 
  httr2[resp_body_html],
  purrr[map, map_vec, list_flatten, list_rbind],
  dplyr[as_tibble, select],
  lubridate[force_tz, now]
)

box::use(
  app/logic/api[api],
  app/logic/api_helpers[...],
  app/logic/request[request]
)

# fetch method ------------------------------------------------------------

#' @export
fetch <- function(x, ...) {
  UseMethod("fetch")
}
fetch.api <- function(api) {
  if(inherits(api, "douban")) {
    fetch.douban(api)
  } else if(inherits(api, "bangumi")) {
    fetch.bangumi(api)
  } else {
    stop(paste0("No fetch method for ", head(class(api), 1), " API class"), 
         call. = FALSE)
  }
}
fetch.douban <- function(x, header_toml = NULL, xpath = NULL) {
  header_toml <- header_toml %||% "app/static/headers.toml"
  xpath <- xpath %||% read_toml(header_toml)$xpaths$douban
  r <- request(x, header_toml = header_toml)
  h <- resp_body_html(r)
  
  if(identical(x$schema, "movie")) {
    res <- xpath$movie |> 
      map(~fetch_douban_movie(h, .x)) |>
      list_flatten(name_spec = "{inner}")
  }
  if(identical(x$schema, "book")) {
    res <- xpath$book |> 
      map(~fetch_douban_book(h, .x)) |>
      list_flatten(name_spec = "{inner}")
  }
  if(identical(x$schema, "music")) {
    res <- xpath$music |> 
      map(~fetch_douban_music(h, .x)) |>
      list_flatten(name_spec = "{inner}")
  }
  if(identical(x$schema, "game")) {
    res <- xpath$game |>
      map(~fetch_douban_game(h, .x)) |>
      list_flatten(name_spec = "{inner}")
  }
  res$id <- NA_integer_
  res$subject_id <- as.character(x$id)
  res$api <- x$domain
  res$type <- x$schema
  res$status <- NA_character_
  res$my_rating <- NA_real_
  res$url <- r$url
  res$created_at <- force_tz(now(), "UTC")
  
  if(identical(x$schema, "movie")) {
    return(as_tibble(res) |>
             select(id, subject_id, api, type, title, cover, year, region,
                    genre, director, starring, status, rating, my_rating,
                    url, created_at))
  }
  if(identical(x$schema, "book")) {
    return(as_tibble(res) |>
             select(id, subject_id, api, type, title, cover, year, author,
                    publisher, status, rating, my_rating, url, created_at))
  }
  if(identical(x$schema, "music")) {
    return(as_tibble(res) |>
             select(id, subject_id, api, type, title, cover, year, performer,
                    status, rating, my_rating, url, created_at))
  }
  if(identical(x$schema, "game")) {
    return(as_tibble(res) |>
             select(id, subject_id, api, type, title, cover, category, developer, 
                    release, status, rating, my_rating, url, created_at))
  }
}



e <- new.env()
local(envir = e, {
  fetch.api
  .S3method("fetch", "api")
})