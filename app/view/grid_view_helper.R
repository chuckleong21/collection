box::use(
  assertthat[assert_that, `on_failure<-`], 
  grDevices[colorRampPalette], 
  lubridate[as_date],
  purrr[map, reduce],
  shiny[tags, tagList, div],
  shiny.fluent[ActionButton.shinyInput],
  stringr[str_replace, str_c, str_split, str_which, str_replace_all],
  tidyr[unnest, separate],
  xfun[download_file],
)

box::use(app/logic/collection[collection],
         app/view/constant[genre_colors, category_colors, regions])

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
  on_failure(is_scalar_numeric) <- function(call, env) paste0(deparse(call$x), " is not a scalar numeric")
  assert_that(is_scalar_numeric(stroke), is_scalar_numeric(width), is_scalar_numeric(height)) 
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
  rating <- ifelse(is.na(rating), 0, rating)
  radius <- width / 2 - 5; circumference <- 2 * 3.14 * radius
  center_x <- center_y <- width / 2
  dasharray <- ifelse(rating != 0, (width / 2 - 5) * 2 * 3.14, 0)
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
                                ifelse(rating != 0, cols[rating * 10], "#bbb"), stroke, dasharray, dashoffset)
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
    "想看" = "#90edf6", "在看" = "#23dbee", "看过" = "#0ea5b5",
    "想读" = "#8fd399", "在读" = "#65c272", "读过" = "#327f3d", 
    "想听" = "#f19171", "在听" = "#ed764d", "听过" = "#e84d18", 
    "想玩" = "#fbcf8b", "在玩" = "#f9af3f", "玩过" = "#f89f19"
  )
  badge_style <- sprintf("background-color:%s;padding:4px 8px;list-style:outside none none;text-align:center;border-radius:5px;",
                         badge_col)
  tags$span(style = badge_style, status)
}

span_region <- function(..., flag_size = c("small", "big")) {
  list2env(..., environment())
  flag_size <- match.arg(flag_size)
  flag_size <- switch(flag_size, "small" = "s", "big" = "b")
  region <- str_split(region, "\\s") |> reduce(c)
  tagList(
    map(region, \(r) {
      if(!is.na(r)) {
        tags$span(class = sprintf("fi fi-%s fi%s", 
                                         regions$country_id[charmatch(r, regions$country)], 
                                         flag_size))
      }
    })
  ) 
}
li_category <- function(...) {
  list2env(..., environment())
  category <- str_split(category, "\\s") |> reduce(c)
  tags$li(
    class = "info",
    map(category, \(cat) {
      if(!is.na(cat)) {
        style <- sprintf('background-color:%s;color:#000;padding:4px 4px;text-align:center;border-radius:6px;', 
                         names(category_colors[str_which(category_colors, cat)]))
        tags$span(style = style, cat)
      }
    })
  )
}
li_info <- function(..., class = "info") {
  list2env(..., environment())
  if(identical(type, "movie")) {
    tagList(
      if(!is.na(director)) {
        tags$li(class = class, sprintf("导演: %s", str_replace_all(director, "\\s", " / ")))
      },
      if(!is.na(starring)) {
        tags$li(class = class, sprintf("主演: %s", str_replace_all(starring, "\\s", " / ")))
      }
    )
  } else if(identical(type, "music")) {
    tagList(
      if(!is.na(performer)) {
        tags$li(class = class, sprintf("演奏者: %s", performer))
      },
      if(!is.na(year)) {
        tags$li(class = class, sprintf("年份: %s", year))
      }
    )
  } else if(identical(type, "book")) {
    tagList(
      if(!is.na(author)) {
        tags$li(class = class, sprintf("作者: %s", author))
      }, 
      if(!is.na(publisher)) {
        tags$li(class = class, sprintf("出版社: %s", publisher))
      }
    )
  } else if(identical(type, "game")) {
    tagList(
      if(!is.na(developer)) {
        tags$li(class = class, sprintf("开发者: %s", developer))
      },
      if(!is.na(release)) {
        tags$li(class = class, sprintf("发行日期: %s", release))
      }
    )
  } else stop("invalid type")
}

li_genre <- function(...) {
  list2env(..., environment())
  genre <- str_split(genre, "\\s") |> reduce(c)
  tags$li(
    class = "info",
    map(genre, \(g) {
      if(!is.na(g)) {
        style <- sprintf('background-color:%s;color:#fff;padding:4px 4px;text-align:center;border-radius:6px;', 
                         names(genre_colors[str_which(genre_colors, g)]))
        tags$span(style = style, g)
      }
    })
  )
}

#' @export
divs <- function(..., class, flag_size = "big", ns) {
  list2env(..., environment())
  switch(
    class, 
    "title" = div(
      class = class,
      span_status(...),
      if(type %in% c("movie", "book")) {
        title_year <- ifelse(is.na(year), title, paste(title, sprintf("(%g)", year)))
        tags$a(href = url, title_year)
      } else {
        tags$a(href = url, title)
      },
      if(identical(type, "movie")) {
        span_region(..., flag_size = flag_size)
      }
    ),
    "meta" = div(
      class = class,
      img_cover(...),
      tags$ul(
        if(identical(type, "movie")) {
          lis(..., class = "genre")
        } else if(identical(type, "game")) {
          lis(..., class = "category")
        },
        lis(..., class = "info"),
        tags$li(class = "date", paste0("记录于", as_date(created_at)))
      )
    ),
    "edit" = div(
      class = class, 
      ActionButton.shinyInput(inputId = ns(sprintf("edit-%s", subject_id)), 
                                            iconProps = list(iconName = "EditSolid12"), text = ""),
      ActionButton.shinyInput(inputId = ns(sprintf("delete-%s", subject_id)),
                              iconProps = list(iconName = "Delete"))
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


