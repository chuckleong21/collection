box::use(
  httr2[req_perform, request, req_headers, req_error, resp_body_html], 
  rvest[html_element, html_elements, html_attr, html_text2], 
  blogdown[read_toml, write_toml],
  cli[combine_ansi_styles],
  stringr[str_detect, str_extract],
  glue[glue], 
  assertthat[assert_that],
  utils[head],
  purrr[map, list_flatten],
  lubridate[force_tz, now],
  dplyr[as_tibble, select]
)

box::use(
  app/logic/api_helpers[...]
)


# api class --------------------------------------------------------------


#' @export
api <- function(x, ...) {
  UseMethod("api")
}
api.character <- function(url) {
  detect_regex <- "https://(bangumi\\.tv|(movie|book|music)\\.douban\\.com|www\\.douban\\.com/game)(/subject)?/(\\d+)/?"
  if(!str_detect(url, detect_regex)) {
    stop('Invalid "url" argument: not a URL or a corrupted URL', call. = FALSE)
  }
  if(str_detect(url, "douban")) {
    domain <- "douban"
    if(str_detect(url, "(movie|music|book)")) {
      schema <- str_extract(url, detect_regex, group = 2)
      id <- str_extract(url, detect_regex, group = 4)
    } else {
      schema <- "game"
      id <- str_extract(url, detect_regex, group = 4)
    }
    structure(list(domain = domain, schema = schema, id = id), 
              class = c("douban", "api"))
  } else if(str_detect(url, "bangumi")) {
    domain <- "bangumi"
    id <- str_extract(url, detect_regex, group = 4)
    structure(list(domain = domain, id = id), class = c("bangumi", "api"))
  }
}

api.numeric <- function(id, domain, schema = NULL) {
  avail_domain <- c("douban", "bangumi", "imdb")
  if(!domain %in% avail_domain) {
    stop('"unknown "domain" argument', call. = FALSE)
  }
  if(domain == "douban") {
    if(is.null(schema)) {
      stop(call. = FALSE, 
           sprintf('empty schema for %s API', deparse(substitute(domain))))
    }
    if(!schema %in% c("movie", "book", "music", "game")) {
      stop(call. = FALSE, "Out-of-scope schema")
    }
    structure(list(domain = domain, schema = schema, id = id), 
              class = c("douban", "api"))
  } else if(domain == "bangumi") {
    structure(list(domain = domain, id = id), class = c("bangumi", "api"))
  }
}


# request method ----------------------------------------------------------


#' @export
request <- function(x, ...)  {
  UseMethod("request")
}
request.character <- function(x) {
  httr2::request(x)
}
request.api <- function(api) {
  if(inherits(api, "douban")) {
    request.douban(douban_api = api)
  } else if(inherits(api, "bangumi")) {
    request.bangumi(bangumi_api = api)
  }
}

# request method for API children classes
request.douban <- function(douban_api) {
  if(douban_api$schema == "game") {
    url_constructor <- 'https://www.douban.com/{douban_api[["schema"]]}/{douban_api[["id"]]}'
  } else {
    url_constructor <- 'https://{douban_api[["schema"]]}.{douban_api[["domain"]]}.com/subject/{douban_api[["id"]]}'
  }
  # iterate cookies until no errors are raised
  headers <- headers()
  r <- purrr::map(headers, ~try({
    glue(url_constructor) |>
      request() |>
      req_headers(!!!.x) |>
      req_error(\(resp) resp$status_code %in% c(403, 404, 500, 502)) |>
      req_perform()
  }, silent = TRUE))
  httr2_success <- purrr::map_vec(r, \(resp) inherits(resp, "httr2_response"))
  if(all(httr2_success)) {
    r <- magrittr::extract2(sample(r, 1), 1)
  } else if(any(httr2_success)){
    r <- r[[which(httr2_success)]]
  } else {
    r <- magrittr::extract2(sample(r, 1), 1)
  }
  r
}

request.bangumi <- function(bangumi_api) {
  url_constructor <- 'https://{bangumi_api[["domain"]]}.tv/subject/{bangumi_api[["id"]]}'
  glue(url_constructor) |> 
    request() |>
    req_error(\(resp) resp$status %in% c(403, 404, 500, 502)) |>
    req_perform()
}

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
fetch.douban <- function(x) {
  r <- request(x)
  h <- httr2::resp_body_html(r)
  xpath <- xpaths$douban
  
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
  res$type <- x$schema
  res$status <- NA_character_
  res$my_rating <- NA_real_
  res$url <- r$url
  res$created_at <- force_tz(now(), "UTC")
  
  if(identical(x$schema, "movie")) {
    return(as_tibble(res) |>
             select(id, subject_id, type, title, cover, year, region,
                    genre, director, starring, status, rating, my_rating,
                    url, created_at))
  }
  if(identical(x$schema, "book")) {
    return(as_tibble(res) |>
             select(id, subject_id, type, title, cover, year, author,
                    publisher, status, rating, my_rating, url, created_at))
  }
  if(identical(x$schema, "music")) {
    return(as_tibble(res) |>
             select(id, subject_id, type, title, cover, year, performer,
                    status, rating, my_rating, url, created_at))
  }
  if(identical(x$schema, "game")) {
    return(as_tibble(res) |>
             select(id, subject_id, type, title, cover, category, developer, 
                    release, status, rating, my_rating, url, created_at))
  }
  stop("Out-of-scope schema", call. = FALSE)
}


# Register S3 Method ------------------------------------------------------


e <- new.env()
local(envir = e, {
  api.character
  .S3method("api", "character")
})
local(envir = e, {
  api.numeric
  .S3method("api", "numeric")
})
local(envir = e, {
  request.character
  .S3method("request", "character")
})
local(envir = e, {
  request.api
  .S3method("request", "api")
})
local(envir = e, {
  fetch.api
  .S3method("fetch", "api")
})
local(envir = e, {
  fetch.douban
  .S3method("fetch", "douban")
})