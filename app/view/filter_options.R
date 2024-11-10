box::use(
  utils[read.csv], 
  dplyr[as_tibble, group_by, mutate, cur_group_id, pull, group_split],
  purrr[map_vec, map, map2, list_flatten, pluck_depth],
  shiny.react[JS]
)

DropdownMenuItemType <- function(type) {
  JS(paste0("jsmodule['@fluentui/react'].DropdownMenuItemType.", type))
}
dropdown_header <- function(ns, inputId, label) {
  list(key = ns(inputId), 
       text = label, 
       itemType = DropdownMenuItemType("Header"))
} 
dropdown_divider <- function(ns, n) {
  list(key = ns(paste("divider", n, sep = "_")), 
       text = "-", 
       itemType = DropdownMenuItemType("Divider"))
}
dropdown_options <- function(ns, inputId, label) {
  list(key = ns(inputId), text = label)
}

# filter by genre / region / rating
# region filters
#' @export
country_options <- function(x, ns) {
  
  country_filter <- read.csv("app/static/country_id.csv", stringsAsFactors = F) |>
    as_tibble() |>
    group_by(continent) |>
    mutate(divider_id = cur_group_id()) 
  
  read_countries <- pull(country_filter, country)
  if(!all(x %in% read_countries)) {
    stop(sprintf("%s\n%s%s","one or more countries are absent with ids",
                 "Consider inserting the following entries in 'app/static/country_id.csv': ",
                 paste0(countries[which(!countries %in% read_countries)], collapse = ", ")), call. = FALSE)
  } else if(!all(read_countries %in% x)) {
    stop(sprintf("%s\n%s%s", "one or more countries are superfluous",
                 "Consider removing the following entries in 'app/static/country_id.csv': ", 
                 paste0(read_countries[which(!read_countries %in% countries)], collapse = ", ")), call. = FALSE)
  }
  
  country_filter_list <- country_filter |> group_split()
  max_divider_id <- country_filter_list |>
    map_vec(\(d) unique(d$divider_id)) |> max()
  
  
  out <- country_filter_list |>  
    map(\(d) {
      header_id <- unique(d$continent_id)
      divider_id <- unique(d$divider_id)
      continent <- unique(d$continent)
      list(
        dropdown_header(ns = ns, header_id, continent),
        map2(d$country, d$country, dropdown_options, ns = ns),
        if(divider_id != max_divider_id) {
          dropdown_divider(ns = ns, divider_id)
        }
      )
    }) |>
    list_flatten() |>
    map(\(x) {
      if(pluck_depth(x) == 2) return(list(x))
      return(x)
    }) |>
    list_flatten()
  
  out[map_vec(out, \(x) !is.null(x))]
}
