box::use(
  grDevices[colorRampPalette], 
  shiny[tags],
  stringr[str_replace, str_c, str_split],
  xfun[download_file],
  lubridate[as_date]
)

#' Generate random R colors
#' 
#' @section Details:
#' The function returns color hex codes from \href{} 
#' 
#' 
#' @param ... dots argument reserved for classes
#' @inheritParams random_r_colors.numeric
#' @inheritParams random_r_colors.character 
#'
#' @return A character vector of hex color codes
#' @export
#' 
#' @seealso [random_r_colors.numeric()], [random_r_colors.character()]
#'
#' @examples
#' random_r_colors(3, accent = 3)
#' random_r_colors(letters[1:5], accent = 2)
random_r_colors <- function(...) {
  UseMethod("random_r_colors")
}

#' Generate Random R Colors
#'
#' @param n An integer. The number of colors to generate
#' @param accent An integer. The color accent to use
#'
#' @return A character vector of hex color codes
#' @export
random_r_colors.numeric <- function(n, accent) {
  is_scalar_integer <- function(x) is.integer(x) & length(x) == 1
  assertthat::on_failure(is_scalar_integer) <- function(call, env) paste0(deparse(call$x), " is not a scalar")
  assertthat::assert_that(is_scalar_integer(n), !is.na(n))
  assertthat::assert_that(is_scalar_integer(accent), accent %in% 1:5)
  response <- httr2::request("https://r-charts.com/colors/") |> 
    httr2::req_perform() |>
    httr2::resp_body_html()
  
  colors_tbl <- response |> 
    rvest::html_elements(xpath = '//*[@id="top"]/section[4]/div/div[3]/div') |>
    purrr::map(\(x) {
      e <- rvest::html_elements(x, "#clipboardItem p") |>
        rvest::html_text2()
      stringr::str_c(e[seq(1, length(e), 2)], e[seq(2, length(e), 2)], sep = ":")
    }) |>
    tibble::enframe(name = "id") |>
    tidyr::unnest(value) |>
    tidyr::separate(value, c("name", "code"), ":")
  
  colors_tbl |>
    dplyr::group_by(id) |>
    dplyr::slice(accent) |>
    dplyr::ungroup() |>
    dplyr::slice_sample(n = n) |>
    dplyr::pull(code)
} 
#' Generate Random R Colors
#'
#' @param x A character vector
#' @param accent An integer. The color accent to use
#' @param include.na Should a color hex code be assigned to NA values?
#'
#' @return A character vector of hex color codes
#' @export
random_r_colors.character <- function(x, accent, include.na) {
  if(any(is.na(x))) {
    if(!include.na) {
      warning(sprintf("remove %g NA value(s)", length(which(is.na(x)))))
      x <- na.omit(x)
    } else x
  }
  n <- length(x)
  response <- httr2::request("https://r-charts.com/colors/") |> 
    httr2::req_perform() |>
    httr2::resp_body_html()
  
  colors_tbl <- response |> 
    rvest::html_elements(xpath = '//*[@id="top"]/section[4]/div/div[3]/div') |>
    purrr::map(\(x) {
      e <- rvest::html_elements(x, "#clipboardItem p") |>
        rvest::html_text2()
      stringr::str_c(e[seq(1, length(e), 2)], e[seq(2, length(e), 2)], sep = ":")
    }) |>
    tibble::enframe(name = "id") |>
    tidyr::unnest(value) |>
    tidyr::separate(value, c("name", "code"), ":")
  
  colors_tbl |>
    dplyr::group_by(id) |>
    dplyr::slice(accent) |>
    dplyr::ungroup() |>
    dplyr::slice_sample(n = n) |>
    dplyr::pull(code)
}
random_r_colors.default <- function(x, ...) {
  x <- as.character(x)
  random_dark_colors(x, ...)
}

#' Draw Gauges
#'
#' @description
#' The function returns a <svg> \code{shiny.tag} object given a rating.
#' The dots argument is essentially a single row of a \code{data.frame} or
#' a \code{tibble}. It requires \emph{type}, \emph{subject_id}, 
#' \emph{rating} for the backend:
#' \describe{
#' \item[type] {Corresponds to four data frames resides in the database: movie, music, book, game}
#' \item[subject_id] {The unique id for path in keyframe CSS}
#' \item[rating] {The rating}
#' }
#' 
#' @param ... A named list
#' @param width A numeric. svg element width
#' @param height A numeric. svg element height
#' @param stroke A numeric. stroke width
#' @param animate Should the gauge be static or animated? Default is TRUE
#'
#' @return shiny.tag object
#' @export
#' 
#' @examples
#' \dontrun {
#' d <- dplyr::slice_sample(collection("database")$data$movie, n = 1)
#' gauge_path(d) |> htmltools::browsable()
#' gauge_path(d, animate = FALSE) |> htmltools::browsable()
#' }
#' 
gauge_path <- function(..., width = 150, height = 150, stroke = 6, animate = TRUE) {
  list2env(..., environment())
  is_scalar_numeric <- function(x) is.numeric(x) & length(x) == 1
  assertthat::on_failure(is_scalar_numeric) <- function(call, env) paste0(deparse(call$x), " is not a scalar numeric")
  assertthat::assert_that(is_scalar_numeric(stroke), is_scalar_numeric(width), is_scalar_numeric(height)) 
  if(identical(type, "movie")) {
    cols <- colorRampPalette(c("#fdffff", "#0ea5b5"))(100)
  } else if(identical(type, "book")) {
    cols <- colorRampPalette(c("#fdffff", "#327f3d"))(100)
  } else if(identical(type, "music")) {
    cols <- colorRampPalette(c("#fdffff", "#e84d18"))(100)
  } else {
    cols <- colorRampPalette(c("#fdffff", "#f89f19"))(100)
  }
  
  class <- sprintf("path-%s", subject_id)
  dash <- sprintf("dash-%s", subject_id)
  radius <- width / 2 - 5; circumference <- 2 * 3.14 * radius
  center_x <- center_y <- width / 2
  dasharray <- (width / 2 - 5) * 2 * 3.14
  dashoffset <- ifelse(animate, dasharray, (1-rating/10) * circumference) 
  tags$svg(
    width = as.character(width), height = as.character(height), 
    if(animate) {
      tags$style(
        sprintf(
          '.%s {animation: 2s %s linear forwards}
        @keyframes %s {to {stroke-dashoffset: %g;}}',
          class, dash, dash, (1 - rating/10) * circumference
        )
      )
    },
    tags$circle(r = as.character(radius), 
                cx = as.character(center_x), 
                cy = as.character(center_y), 
                class = class, 
                style = sprintf("fill:none; stroke:%s; stroke-width:%g; stroke-dasharray:%g; stroke-dashoffset:%g;",
                                cols[rating * 10], stroke, dasharray, dashoffset)
    ),
    tags$text(x = "40", y = "90", style = "font-size:xxx-large;", sprintf("%.1f", rating))
  )
}

#' Show Cover Image
#'
#' @description
#' The function download the image from a URL before returning the <img> \code{shiny.tag} object. 
#' The dots argument is essentially a row of a \code{data.frame} or 
#' a \code{tibble}. It requires \emph{type}, \emph{subject_id}, \emph{cover}
#' for the backend:
#' \describe{
#' \item[type] {Corresponds to four data frames resides in the database: movie, music, book, game, 
#' a dependent paramter for the static file path}
#' \item[subject_id] {The static file name. A dependent parameter for the static file path}
#' \item[cover] {The static file URL}
#' }
#' 
#' @param ... A named list
#'
#' @return A shiny.tag object
#' @export
#' 
#' @examples
#' \dontrun {
#' d <- dplyr::slice_sample(collection("database")$data$movie, n = 1)
#' img_cover(d) |> htmltools::browsable()
#' }
#' 
img_cover <- function(...) {
  list2env(..., envir = environment())
  out <- sprintf("static/cover/%s/%s", type, str_replace(basename(cover), "^.+\\.", paste0(subject_id, ".")))
  if(!str_replace(basename(cover), "^.+\\.", paste0(subject_id, ".")) %in% list.files(sprintf("app/static/cover/%s", type))) {
    download_file(url = cover, mode = "wb", output = out)
  }
  tags$img(src = out, alt = title)
}

#' Status Badge
#'
#' @description
#' The function returns a <span> \code{shiny.tag} object given a status.
#' The dots argument is essentially a row of a \code{data.frame} or 
#' a \code{tibble}. It requires \emph{status} for the backend.
#'
#' @param ... A named list
#'
#' @return A shiny.tag object
#' @export
#'
#' @examples
#' \dontrun{
#' d <- dplyr::slice_sample(collection("database")$data$book, n = 1)
#' span_badge(d) |> htmltools::browsable()
#' }
span_status <- function(...) {
  list2env(..., environment())
  badge_col <- switch(
    as.character(status), 
    "想看" = "#90edf6",
    "在看" = "#23dbee", 
    "看过" = "#0ea5b5",
    "想读" = "#8fd399", 
    "在读" = "#65c272", 
    "读过" = "#327f3d", 
    "想听" = "#f19171", 
    "在听" = "#ed764d", 
    "听过" = "#e84d18", 
    "想玩" = "#fbcf8b", 
    "在玩" = "#f9af3f", 
    "玩过" = "#f89f19", 
  )
  badge_style <- sprintf("background-color:%s;padding:4px 8px;list-style:outside none none;text-align:center;border-radius:5px;",
                         badge_col)
  tags$span(style = badge_style, status)
}
span_region <- function(..., flag_size = c("small", "big")) {
  list2env(..., environment())
  if(!exists("region_tbl", .GlobalEnv)) {
    region_tbl <<- tibble::tibble(
      chinese = new_collection$data$movie$region |> stringr::str_split("\\s") |> purrr::reduce(c) |> unique() |> na.omit(),
      english = purrr::map_vec(chinese, \(x) polyglotr::google_translate(x, target_language = "en", source_language = "zh-CN"))
    ) |>
      dplyr::inner_join(
        countrycode::codelist |>
          dplyr::transmute(country.name.en, iso2c = tolower(iso2c)) |>
          dplyr::mutate(country.name.en = dplyr::case_match(country.name.en, 
                                                            "United Kingdom" ~ "U.K.",
                                                            "United States" ~ "USA", 
                                                            "Philippines" ~ "the Philippines",
                                                            "Hong Kong SAR China" ~ "Hongkong", 
                                                            "Czechia" ~ "Czech Republic",
                                                            .default = country.name.en)),
        by = c("english" = "country.name.en")
      )
  }
  flag_size <- match.arg(flag_size)
  flag_size <- switch(flag_size, "small" = "s", "big" = "b")
  region <- stringr::str_split(region, "\\s") |> purrr::reduce(c)
  shiny::tagList(
    purrr::map(region, \(r) {
      if(!is.na(r)) {
        shiny::tags$span(class = sprintf("fi fi-%s fi%s", 
                                         region_tbl$iso2c[charmatch(r, region_tbl$chinese)], 
                                         flag_size))
      }
    })
  ) 
}

li_category <- function(..., class = "category") {
  list2env(..., environment())
  if(!exists("category_colors", envir = .GlobalEnv)) {
    category_colors <- collection("database")$data$game$category |> 
      stringr::str_split("\\s") |>
      purrr::reduce(c) |>
      unique() |>
      na.omit()
    category_colors <<- category_colors |> 
      purrr::set_names(random_r_colors(category_colors, accent = 3))
  }
  category <- stringr::str_split(category, "\\s") |> purrr::reduce(c)
  shiny::tags$li(
    purrr::map(category, \(cat) {
      if(!is.na(cat)) {
        style <- sprintf('background-color:%s;color:#000;padding:4px 4px;text-align:center;border-radius:6px;', 
                         names(category_colors[stringr::str_which(category_colors, cat)]))
        shiny::tags$span(style = style, cat)
      }
    })
  )
}
li_info <- function(..., class = "info") {
  list2env(..., environment())
  if(identical(type, "movie")) {
    shiny::tagList(
      if(!is.na(director)) {
        shiny::tags$li(class = class, sprintf("导演: %s", stringr::str_replace_all(director, "\\s", " / ")))
      },
      if(!is.na(starring)) {
        shiny::tags$li(class = class, sprintf("主演: %s", stringr::str_replace_all(starring, "\\s", " / ")))
      }
    )
  } else if(identical(type, "music")) {
    shiny::tagList(
      if(!is.na(performer)) {
        shiny::tags$li(class = class, sprintf("演奏者: %s", performer))
      },
      if(!is.na(year)) {
        shiny::tags$li(class = class, sprintf("年份: %s", year))
      }
    )
  } else if(identical(type, "book")) {
    shiny::tagList(
      if(!is.na(author)) {
        shiny::tags$li(class = class, sprintf("作者: %s", author))
      }, 
      if(!is.na(publisher)) {
        shiny::tags$li(class = class, sprintf("出版社: %s", publisher))
      }
    )
  } else if(identical(type, "game")) {
    shiny::tagList(
      if(!is.na(developer)) {
        shiny::tags$li(class = class, sprintf("开发者: %s", developer))
      },
      if(!is.na(release)) {
        shiny::tags$li(class = class, sprintf("发行日期: %s", release))
      }
    )
  } else stop("invalid type")
}
li_genre <- function(..., class = "info") {
  list2env(..., environment())
  if(!exists("genre_colors", envir = .GlobalEnv)) {
    genre_colors <- collection("database")$data$movie$genre |> 
      stringr::str_split("\\s") |>
      purrr::reduce(c) |>
      unique() |>
      na.omit()
    genre_colors <<- genre_colors |> 
      purrr::set_names(random_r_colors(genre_colors, accent = 5))
  }
  genre <- stringr::str_split(genre, "\\s") |> purrr::reduce(c)
  shiny::tags$li(
    purrr::map(genre, \(g) {
      if(!is.na(g)) {
        style <- sprintf('background-color:%s;color:#fff;padding:4px 4px;text-align:center;border-radius:6px;', 
                         names(genre_colors[stringr::str_which(genre_colors, g)]))
        shiny::tags$span(style = style, g)
      }
    })
  )
}

divs <- function(..., class) {
  list2env(..., environment())
  switch(
    class, 
    "title" = shiny::div(
      class = class,
      span_status(...),
      if(type %in% c("movie", "book")) {
        title_year <- ifelse(is.na(year), title, paste(title, sprintf("(%g)", year)))
        shiny::tags$a(href = url, title_year)
      } else {
        shiny::tags$a(href = url, title)
      },
      if(identical(type, "movie")) {
        span_region(...)
      }
    ),
    "meta" = shiny::div(
      class = class,
      img_cover(...),
      tags$ul(
        if(identical(type, "movie")) {
          lis(..., class = "genre")
        } else if(identical(type, "game")) {
          lis(..., class = "category")
        },
        lis(..., class = "info"),
        tags$li(class = "date", paste0("记录于", lubridate::as_date(created_at)))
      )
    )
  )
}
lis <- function(..., class) {
  switch(
    class, 
    "genre" = li_genre(...),
    "category" = li_category(...),
    "info" = li_info(...)
  )
}



# test functions ----------------------------------------------------------

div_item <- function(..., class = "item") {
  shiny::div(
    class = class, 
    gauge_path(...),
    div(
      class = "info",
      divs(..., class = "title"),
      divs(..., class = "meta")
    )
  )
}
