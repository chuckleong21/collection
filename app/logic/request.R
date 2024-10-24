box::use(
  glue[glue],
  httr2[req_headers, req_error, req_perform],
  magrittr[extract2],
  purrr[map, map_vec],
  stats[runif],
  stringr[str_detect], 
  tibble[tibble],
  dplyr[mutate]
)

box::use(
  app/logic/api_helpers[headers],
  app/logic/branch[branch]
)


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
request.douban <- function(douban_api, header_toml = NULL) {
  on.exit(Sys.sleep(runif(1, 5, 10)), add = TRUE)
  if(douban_api$schema == "game") {
    url_constructor <- 'https://www.douban.com/{douban_api[["schema"]]}/{douban_api[["id"]]}'
  } else {
    url_constructor <- 'https://{douban_api[["schema"]]}.{douban_api[["domain"]]}.com/subject/{douban_api[["id"]]}'
  }
  # iterate cookies until no errors are raised
  header_toml <- header_toml %||% "app/static/headers.toml"
  headers <- headers(header_toml)
  r <- map(headers, ~try({
    glue(url_constructor) |>
      request() |>
      req_headers(!!!.x) |>
      req_error(\(resp) resp$status_code %in% c(403, 404, 500, 502) | str_detect(resp$url, "sorry")) |>
      req_perform()
  }, silent = TRUE))
  httr2_success <- map_vec(r, \(resp) inherits(resp, "httr2_response"))
  if(all(httr2_success)) {
    r <- extract2(sample(r, 1), 1)
  } else if(any(httr2_success)){
    r <- r[[which(httr2_success)]]
  } else {
    r <- extract2(sample(r, 1), 1)
  }
  r
}

request.bangumi <- function(bangumi_api) {
  url_constructor <- 'https://{bangumi_api[["domain"]]}.tv/subject/{bangumi_api[["id"]]}'
  glue(url_constructor) |> 
    request() |>
    req_error(\(resp) resp$status_code %in% c(403, 404, 500, 502)) |>
    req_perform()
}

request.collection <- function(collection) {
  requests <- map(collection$data, 
                 ~ tibble(subject_id = .x$subject_id, 
                          url = .x$url)) |>
    list_rbind()
  out <- requests |>
    mutate(html = map(url, \(x) api(x) |>
                        request() |>
                        extract2("body"), .progress = TRUE), 
           html = map_vec(html, \(h) rawToChar(h[!h == '00'])))
  attributes(out) <- append(attributes(out), list(tableType = "requests"))
  out
}

request.collection_diff <- function(x) {
  u <- purrr::map(branch(d), 
                  ~ tibble::tibble(subject_id = .x$subject_id, 
                                   url = .x$url)) |>
    purrr::list_rbind() |>
    dplyr::select(-subject_id)
  if(nrow(u) == 0) return(tibble::tibble(url = character(), html = character()))
  dplyr::mutate(u, html = purrr::map(url, \(x) {
    api(x) |>
      request() |>
      magrittr::extract2("body")
  }, .progress = TRUE), 
  html = purrr::map_vec(html, \(h) rawToChar(h[!h == '00'])))
}


e <- new.env()
local(envir = e, {
  request.character
  .S3method("request", "character")
})
local(envir = e, {
  request.api
  .S3method("request", "api")
})
local(envir = e, {
  request.collection
  .S3method("request", "collection")
})
local(envir = e, {
  request.collection_diff
  .S3method("request", "collection_diff")
})