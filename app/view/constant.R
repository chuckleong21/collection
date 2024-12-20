box::use(
  app/logic/collection[collection]
)

box::use(
  stringr[str_split],
  purrr[reduce, set_names],
  stats[na.omit],
  dplyr[slice_sample, pull, left_join, transmute, group_by, mutate, cur_group_id],
  assertthat[`on_failure<-`, assert_that],
  tibble[tibble],
  countrycode[countrycode]
)

#' Generate random R colors
#' 
#' @section Details:
#' The function returns color hex codes by scraping 
#' \href{https://r-charts.com/colors/} {R-colors}. The website groups
#' the color tone nicely and each group has 5 color accents.
#' 
#' @param x An integer. The number of colors to generate,\cr A character vector.
#' @param accent An integer. The color accent to use
#' @param ... other arguments
#'
#' @return A character vector of hex color codes
#' @export
#' 
#' @examples
#' random_r_colors(3, accent = 3)
#' random_r_colors(letters[1:5], accent = 2)
#' 
random_r_colors <- function(x, accent, include.na) {
  UseMethod("random_r_colors")
}

random_r_colors.default <- function(x, accent, include.na = TRUE) {
  colors_tbl <- readRDS("app/static/color_dict.rds")
  
  is_scalar_numeric <- function(x) is.numeric(x) & length(x) == 1
  on_failure(is_scalar_numeric) <- function(call, env) paste0(deparse(call$x), " is not a scalar")
  assert_that(is_scalar_numeric(accent), accent %in% 1:5)
  
  if(inherits(x, "numeric")) {
    assert_that(is_scalar_numeric(x), !is.na(x))
  } else if(inherits(x, "character")) {
    if(any(is.na(x))) {
      if(!include.na) {
        warning(sprintf("remove %g NA value(s)", length(which(is.na(x)))))
        x <- na.omit(x)
      } else x
    }
    x <- length(x)
  } else {
    x <- as.character(x)
    if(any(is.na(x))) {
      if(!include.na) {
        warning(sprintf("remove %g NA value(s)", length(which(is.na(x)))))
        x <- na.omit(x)
      } else x
    }
    x <- length(x)
  }
  colors_tbl |>
    slice_sample(n = x) |>
    pull(code)
}

genre_color_generator <- function(accent, seed) {
  set.seed(seed)
  genre_all <- collection("database")$data$movie$genre |> 
    str_split("\\s") |>
    reduce(c) |>
    unique()
  genre_all |> 
    set_names(random_r_colors(genre_all, accent = accent)) |>
    na.omit()
}

category_color_generator <- function(accent, seed) {
  set.seed(seed)
  category_all <- collection("database")$data$game$category |> 
    str_split("\\s") |>
    reduce(c) |>
    unique() |>
    na.omit()
  category_all |> 
    set_names(random_r_colors(category_all, accent = accent))
}

region_tbl <- function() {
  avail_regions <- collection("database")$data$movie$region |> 
    str_split("\\s") |> 
    reduce(c) |> 
    unique() |> 
    na.omit()
  country_dict <- readRDS("app/static/country_dict.rds")
  
  tibble(
    country.name.cn = avail_regions,
  ) |>
    left_join(
      country_dict, by = "country.name.cn"
    ) |> 
    transmute(
      continent_id = tolower(continent),
      country_id = tolower(countrycode(country.name.en, "country.name.en", "iso2c")),
      country = country.name.cn,
      continent = continent.cn
    ) |>
    group_by(continent) |>
    mutate(divider_id = cur_group_id())
}

#' @export
genre_colors <- genre_color_generator(3, 100)

#' @export
category_colors <- category_color_generator(3, 200)

#' @export
regions <- region_tbl()