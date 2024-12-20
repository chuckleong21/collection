box::use(
  magrittr[`%>%`],
  purrr[map, list_rbind, pmap, map2, map2_vec, pmap_vec],
  dplyr[transmute, count, group_by, mutate, row_number, right_join,
        arrange, ungroup, group_split, case_when, between], 
  lubridate[as_date, today, year, month], 
  tibble[tibble],
  stringr[str_c], 
  shiny[tags, tagList, div]
)

#' @export
heatmap <- function(x, ...) {
  UseMethod("heatmap")
}

heatmap.collection <- function(x, width = 700, height = 110) {
  # heatmap data
  heatmap_data <- map(x$data, \(d) transmute(d, type, created_at = as_date(created_at))) |>
    list_rbind() |> 
    count(type, created_at)
  # yearly data
  end_date <- today(); start_date <- end_date - 5 - 51*7 + 1
  week_number <- rep(1:52, each = 7)[seq_along(seq(start_date, end_date, 1))]
  yearly_data <- tibble(created_at = seq(start_date, end_date, 1),
                           week_number = week_number) |>
    group_by(week_number) |>
    mutate(day_number = row_number(),
                  y = 13 * (day_number - 1),
                  width = 11, height = 11)
  # join yearly data 
  heatmap_data <- heatmap_data |> 
    right_join(yearly_data) |>
    arrange(created_at) |>
    mutate(fill = color_accent(type, n)) |>
    group_by(created_at) |>
    mutate(item_group = row_number(),
                  item_group_sum = sum(item_group),
                  type = str_c(type, collapse = ", ")) |>
    ungroup() |> 
    mutate(points = polygon_points(item_group_sum, item_group, y)) |>
    group_split(week_number)
  
  # html structure
  heatmap_svg <- heatmap_data |>
    map(\(d) {
      pmap(d, \(item_group, width, height, y, points, fill, type, created_at, ...) {
        if(item_group == 1) {
          tags$rect(
            class = "day", rx = "2", ry = "2",
            width = as.character(width), 
            height = as.character(height), 
            y = as.character(y), 
            fill = fill, 
            `data-items` = ifelse(is.na(type), NA_character_, type), 
            `data-legend` = NA_character_, 
            `data-date` = created_at
          )
        } else {
          tags$polygon(
            points = points, 
            fill = fill, 
            `data-items` = ifelse(is.na(type), NA_character_, type), 
            `data-legend` = NA_character_, 
            `data-date` = created_at
          )
        }
      })
    })
  
  out <- map(c(seq_along(heatmap_svg), length(heatmap_svg) + 1), \(i) {
    if(i != 53) {
      tags$g(transform = sprintf("translate(%g, 0)", 13*(i-1)), 
             tagList(heatmap_svg[[i]]))
    } else {
      tagList(
        tags$g(transform = sprintf("translate(%g, 0)", 13*(i-1))),
        heatmap_label(end_date)
      )
    }
  }) |> 
    heatmap_container(width = width, height = height)
  out
}

heatmap_container <- function(..., width = 700, height = 110) {
  div(
    class = "calendar_view cards", 
    tags$svg(width = as.character(width), height = as.character(height), 
             tags$g(transform = "translate(0, 20)", ...))
  )
}
heatmap_label <- function(date) {
  a <- map(seq_len(12), \(i) {
    if(i != 1) {
      seq_len(12)[-seq_len(i-1)]
    } else {
      seq_len(12)
    }
  }) |>
    map(\(x) {
      if(length(x) != 12) {
        xx <- month.abb[c(x, seq_len(12 - length(x)))]
        xx[charmatch("Jan", xx)] <- year(date)
        xx
      } else {
        xx <- month.abb[x]
        xx[charmatch("Jan", xx)] <- year(date)
        xx
      }
    }) %>% 
    do.call(cbind, .)
  if(month(date) != 12) {
    a <- a[month(date) + 1, ]
  } else {
    a <- a[1, ]
  }
  x <- cumsum(c(3, 5, 4, 4, 5, 4, 4, 5, 4, 4, 5, 4)) * 13
  map2(x, a, \(x, y) tags$text(
    x = as.character(x), y = as.character(-5), 
    class = "month", toupper(y)
  ))
}
polygon_points <- function(item_group_sum, item_group, y, width = 11, height = 11) {
  pmap_vec(
    list(item_group_sum, item_group, y, width, height), 
    \(item_group_sum, item_group, y, width, height) {
      case_when(
        item_group_sum == 10 ~ {
          case_when(
            item_group == 4 ~ sprintf("%g,%g %g,%g %g,%g %g,%g", 
                                      0, y + height, 0, y + height - 5, 
                                      6, y + height - 5, 6, y + height),
            item_group == 3 ~ sprintf("%g,%g %g,%g %g,%g %g,%g", 
                                      0, y + height - 5, 0, y, 6, y, 6, y + height - 5),
            item_group == 2 ~ sprintf("%g,%g %g,%g %g,%g %g,%g", 
                                      6, y + height - 5, 6, y, width, y, width, y + height - 5),
            .default = NA_character_
          )
        }, 
        item_group_sum == 6 ~ {
          case_when(
            item_group == 3 ~ sprintf("%g,%g %g,%g %g,%g %g,%g", 
                                      0, y + 3/4*height, 0, y + height, 
                                      width, y + height, 1/2*width, y + 1/2*height), 
            item_group == 2 ~ sprintf("%g,%g %g,%g %g,%g, %g,%g", 
                                      width, y + height, width, y, 
                                      3/4*width, y, 1/2*width, y + 1/2*height),
            .default = NA_character_
          )
        }, 
        item_group_sum == 3 ~ {
          case_when(
            item_group == 2 ~ sprintf("%g,%g %g,%g %g,%g", 
                                      0, y + height, width, y + height, width, y),
            .default = NA_character_
          )
        }, 
        .default = NA_character_
      )
    })
}
color_accent <- function(type, n) {
  map2_vec(type, n, \(type, n) {
    switch(
      type, 
      "book" = {
        case_when(
          between(n, 1, 5) ~ "#8fd399",
          between(n, 6, 10) ~ "#65c272", 
          n > 10 ~ "#327f3d",
        )
      }, 
      "movie" = {
        case_when(
          between(n, 1, 5) ~ "#90edf6",
          between(n, 6, 10) ~ "#23dbee", 
          n > 10 ~ "#0ea5b5",
        )
      }, 
      "music" = {
        case_when(
          between(n, 1, 5) ~ "#f19171",
          between(n, 6, 10) ~ "#ed764d", 
          n > 10 ~ "#e84d18",
        )
      }, 
      "game" = {
        case_when(
          between(n, 1, 5) ~ "#fbcf8b",
          between(n, 6, 10) ~ "#f9af3f", 
          n > 10 ~ "#f89f19",
        )
      },
      "#fbfcfc"
    )
  })
}

e <- new.env()
local(envir = e, {
  heatmap.collection
  .S3method("heatmap", "collection")
})