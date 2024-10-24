box::use(
  stringr[str_detect, str_extract]
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

