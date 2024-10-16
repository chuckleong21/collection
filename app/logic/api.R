box::use(
  httr2[req_perform, request, req_headers, req_error], 
  rvest[html_element, html_elements, html_attr, html_text2], 
  blogdown[read_toml, write_toml],
  cli[combine_ansi_styles],
  stringr[str_detect, str_extract],
  glue[glue], 
  assertthat[assert_that]
)

# box::use(
#   app/logic/collection[...]
# )


# api class --------------------------------------------------------------


#' @export
api <- function(x, ...) {
  UseMethod("api")
}
api.character <- function(url) {
  detect_regex <- "https://(bangumi\\.tv|(movie|book|music)\\.douban\\.com|www\\.douban\\.com/game)(/subject)?/(\\d+)/?"
  if(!stringr::str_detect(url, detect_regex)) {
    stop('Invalid "url" argument: not a URL or a corrupted URL', call. = FALSE)
  }
  if(stringr::str_detect(url, "douban")) {
    domain <- "douban"
    if(stringr::str_detect(url, "(movie|music|book)")) {
      schema <- stringr::str_extract(url, detect_regex, group = 2)
      id <- stringr::str_extract(url, detect_regex, group = 4)
    } else {
      schema <- "game"
      id <- stringr::str_extract(url, detect_regex, group = 4)
    }
    structure(list(domain = domain, schema = schema, id = id), 
              class = c("douban", "api"))
  } else if(stringr::str_detect(url, "bangumi")) {
    domain <- "bangumi"
    id <- stringr::str_extract(url, detect_regex, group = 4)
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
request.api <- function(api, headers = NULL) {
  if(inherits(api, "douban")) {
    request.douban(douban_api = api, headers = headers)
  } else if(inherits(api, "bangumi")) {
    request.bangumi(bangumi_api = api)
  }
}

# request method for API children classes
request.douban <- function(douban_api, headers = NULL) {
  headers <- headers %||% read_toml("app/static/headers.toml")$headers
  if(douban_api$schema == "game") {
    url_constructor <- 'https://www.douban.com/{douban_api[["schema"]]}/{douban_api[["id"]]}'
  } else {
    url_constructor <- 'https://{douban_api[["schema"]]}.{douban_api[["domain"]]}.com/subject/{douban_api[["id"]]}'
  }
    glue(url_constructor) |>
      request() |>
      req_headers(!!!headers) |>
      req_error(\(resp) resp$status %in% c(403, 404, 500, 502)) |>
      req_perform()
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
fetch <- function(api) {
  UseMethod("fetch")
}
fetch.api <- function(api) {
  if(inherits(api, "douban")) {
    fetch.douban(api)
  } else if(inherits(api, "bangumi")) {
    fetch.bangumi(api)
  }
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
