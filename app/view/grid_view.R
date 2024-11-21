box::use(
  glue[glue], 
  tidyr[complete], 
  grDevices[colorRampPalette], 
  utils[head], 
  ggplot2[ggplot, aes, geom_rect, geom_text, 
          coord_polar, xlim, theme_void, ggsave], 
  stringr[str_replace, str_split, str_c], 
  xfun[download_file], 
  tibble[tibble],
  shiny[tags, tagList, div]
)

box::use(
  app/view/grid_view_helper
)

makeRecord <- function(x) {
  div_pic <- function(x) {
    div(
      class = "pic",
      tags$a(title = x$title, href = x$url, 
        img_cover(x)
      ),
      gauge_path(x, animate = TRUE)
    )
  }
  
  div_info <- function(x) {
    div(
      class = "info",
      div(class = "title", span_status(x$status), 
          tags$a(href = x$url, style = "padding:5px;", x$title)),
      tags$ul(
        if(identical(x$type, "movie")) {
          tagList(
            li_meta(type = x$type, year = x$year, region = x$region, genre = x$genre),
            li_director(x),
            li_starring(x)
          )
        } else if(identical(x$type, "book")) {
          li_meta(type = x$type, author = x$author, publisher = x$publisher, year = x$year)
        } else if(identical(x$type, "music")) {
          li_meta(type = x$type, performer = x$performer, year = x$year)
        } else if(identical(x$type, "game")) {
          li_meta(type = x$type, category = x$category, developer = x$developer, release = x$release)
        },
        tags$li(class = "date", paste0("记录于", lubridate::as_date(x$created_at)))
      )
    )
  }
  div(class = "item comment-item", div_pic(x), div_info(x))
}

#' @export
grid_view <- function(x) {
    tagList(
      purrr::map(seq_len(nrow(x)), \(i) makeRecord(x[i, ]))
    )
}
