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

makeRating <- function(x) {
  png_path <- glue("app/static/cover/rating/{x$type}/{x$subject_id}.png")
  if(!sprintf("%s.png", x$subject_id) %in% list.files(sprintf("app/static/cover/rating/%s", x$type))) {
    x <- complete(x, fill = list(my_rating = 0))
    if(identical(x$type, "movie")) {
      cols <- colorRampPalette(c("#fdffff", "#0ea5b5"))(100)
    } else if(identical(x$type, "book")) {
      cols <- colorRampPalette(c("#fdffff", "#327f3d"))(100)
    } else if(identical(x$type, "music")) {
      cols <- colorRampPalette(c("#fdffff", "#e84d18"))(100)
    } else {
      cols <- colorRampPalette(c("#fdffff", "#f89f19"))(100)
    }
    ring <- tibble(
      percent = c(1-x$rating/10, x$rating/10), 
      mark = x$my_rating * 2,
      ymax = cumsum(percent), 
      ymin = c(0, head(ymax, n = -1)),
      color = c(cols[1], cols[x$rating*10])
    )
    p <- ggplot(
      ring, aes(ymax = ymax, ymin = ymin, 
                xmin = 3, xmax = 4, fill = I(color))
    ) + 
      geom_rect(show.legend = FALSE) + 
      geom_text(aes(x = -5, y = 0, label = x$rating), size = 10) +
      coord_polar(theta = "y") + 
      xlim(c(-6, 4.5)) + 
      theme_void() + 
      ggsave(png_path, p, width = 197, units = "px",dpi = "print")
  }
  png_path
}
img_cover <- function(cover, alt) {
  out <- sprintf("app/static/cover/%s/%s", x$type, str_replace(basename(cover), "^.+\\.", paste0(x$subject_id, ".")))
  if(!str_replace(basename(cover), "^.+\\.", paste0(x$subject_id, ".")) %in% list.files(sprintf("app/static/cover/%s", x$type))) {
    download_file(url = cover, mode = "wb", output = out)
  }
  tags$img(src = out, alt = alt, width = "110%")
}
span_badge <- function(status) {
      badge_col <- switch(
        as.character(status), 
        "想看" = "#90edf6",
        "在看" = "#23dbee", 
        "看过" = "#0ea5b5",
        "想读" = "#8fd399", 
        "在读" = "#65c272", 
        "已读" = "#327f3d", 
        "想听" = "#f19171", 
        "在听" = "#ed764d", 
        "已听" = "#e84d18", 
        "想玩" = "#fbcf8b", 
        "在玩" = "#f9af3f", 
        "已玩" = "#f89f19", 
      )
      badge_style <- sprintf("background-color:%s;padding:4px 8px;list-style:outside none none;text-align:center;border-radius:5px;",
                             badge_col)
      tags$span(style = badge_style, status)
    }
li_meta <- function(type, ...) {
  zz <- list(...)
  if(identical(type, "movie")) {
    if(!is.na(zz$year) | !is.na(zz$region) | !is.na(zz$genre)) {
      region <- str_split(zz$region, "\\s", simplify = TRUE) |>
        str_c(collapse = " / ")
      genre <- str_split(zz$genre, "\\s", simplify = TRUE) |>
        str_c(collapse = " / ")
      tags$li(
        class = "intro",
        sprintf("%s%s%s", 
                ifelse(is.na(zz$year), "", paste0(zz$year, " / ")), 
                ifelse(is.na(zz$region), "", paste0(zz$region, " / ")), 
                ifelse(is.na(zz$genre), "", zz$genre)
        ))
    }
  } else if(identical(type, "book")) {
    if(!is.na(zz$year) | !is.na(zz$author) | !is.na(zz$publisher)) {
      tags$li(
        class = "intro", 
        sprintf("%s%s%s", 
                ifelse(is.na(zz$author), "", paste0(zz$author, " / ")), 
                ifelse(is.na(zz$publisher), "", paste0(zz$publisher, " / ")), 
                ifelse(is.na(zz$year), "", zz$year))
      )
    }
  } else if(identical(type, "music")) {
    if(!is.na(zz$performer) | !is.na(zz$year)) {
      tags$li(
        class = "intro", 
        sprintf("%s%s", 
                ifelse(is.na(zz$performer), "", paste0(zz$performer, " / ")), 
                ifelse(is.na(zz$year), "", zz$year))
      )
    }
  } else if(identical(type, "game")) {
    if(!is.na(zz$category) | !is.na(zz$developer) | !is.na(zz$release)) {
      tags$li(
        class = "intro", 
        sprintf("%s%s%s", 
                ifelse(is.na(zz$category), "", paste0(zz$category, " / ")), 
                ifelse(is.na(zz$developer), "", paste0(zz$developer, " / ")),
                ifelse(is.na(zz$release), "", lubridate::as_date(zz$release)))
      )
    }
  } else stop("invalid type", call. = FALSE)
}
li_director <- function(director) {
  if(!is.na(x$director)) {
    director <- str_split(director, pattern = "\\s", simplify = TRUE)|> 
      str_c(collapse = " / ")
    tags$li(class = "intro", sprintf("导演: %s", director))
  }
}
li_starring <- function(starring) {
  if(!is.na(x$starring)) {
    starring <- str_split(starring, pattern = "\\s", simplify = TRUE)|> 
      str_c(collapse = " / ")
    tags$li(class = "intro", sprintf("主演: %s", starring))
    
  }
}

#' @export
makeRecord <- function(x) {
  div_pic <- function(x) {
    div(
      class = "pic",
      tags$a(title = x$title, href = x$url, 
        img_cover(cover = x$cover, alt = x$title)
      ),
      tags$img(src = makeRating(x), alt="rating")
    )
  }
  
  div_info <- function(x) {
    div(
      class = "info",
      div(class = "title", span_badge(x$status), 
          tags$a(href = x$url, style = "padding:5px;", x$title)),
      tags$ul(
        if(identical(x$type, "movie")) {
          tagList(
            li_meta(type = x$type, year = x$year, region = x$region, genre = x$genre),
            li_director(x$director),
            li_starring(x$starring)
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
