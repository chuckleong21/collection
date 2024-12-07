box::use(
  shiny[div, tagList, NS],
  purrr[map, map_vec, map2, list_flatten, pluck_depth],
  shiny.fluent[Dropdown.shinyInput],
  shiny.react[JS],
  dplyr[group_by, group_split]
)

box::use(
  app/view/grid_view_helper[gauge_path, divs, region_tbl]
)

div_item <- function(..., class = "item") {
  div(
    class = class, 
    gauge_path(...),
    div(
      class = "intro",
      divs(..., class = "title"),
      divs(..., class = "meta")
    )
  )
}
#' @export
region_dropdown <- function(inputId, label, multiple, ...) {
  DropdownMenuItemType <- function(type) {
    JS(paste0("jsmodule['@fluentui/react'].DropdownMenuItemType.", type))
  }
  dropdown_header <- function(inputId, label) {
    list(key = inputId, 
         text = label, 
         itemType = DropdownMenuItemType("Header"))
  } 
  dropdown_divider <- function(n) {
    list(key = paste("divider", n, sep = "_"), 
         text = "-", 
         itemType = DropdownMenuItemType("Divider"))
  }
  dropdown_options <- function(inputId, label) {
    list(key = inputId, text = label)
  }
  if(!exists("regions", .GlobalEnv)) {
    regions <<- region_tbl()
  }
  
  regions_list <- .GlobalEnv$regions |> 
    group_by(continent) |>
    group_split()
  max_divider_id <- regions_list |>
    map_vec(\(d) unique(d$divider_id)) |> max()
  out <- regions_list |>  
    map(\(d) {
      header_id <- unique(d$continent_id)
      divider_id <- unique(d$divider_id)
      continent <- unique(d$continent)
      list(
        dropdown_header(header_id, continent),
        map2(d$country, d$country, dropdown_options),
        if(divider_id != max_divider_id) {
          dropdown_divider(divider_id)
        }
      )
    }) |>
    list_flatten() |>
    map(\(x) {
      if(pluck_depth(x) == 2) return(list(x))
      return(x)
    }) |>
    list_flatten()
  
  out <- out[map_vec(out, \(x) !is.null(x))]
  Dropdown.shinyInput(inputId = inputId, label = label, 
                      multiSelect = multiple, 
                      options = out, ...)
}

#' @export
genre_dropdown <- function(inputId, label, multiple, ...) {
  dropdown_options <- function(inputId, label) {
    list(key = inputId, text = label)
  }
  if(!exists("genre_colors", envir = .GlobalEnv)) {
    genre_all <- collection("database")$data$movie$genre |> 
      str_split("\\s") |>
      reduce(c) |>
      unique()
    genre_colors <<- genre_all |> 
      set_names(random_r_colors(genre_all, accent = 5)) |>
      na.omit()
  }
  out <- purrr::map(genre_colors, \(x) dropdown_options(x, x)) |> unname()
  Dropdown.shinyInput(inputId = inputId, label = label, multiSelect = multiple, options = out, ...)
}

#' @export
grid_view <- function(x) {
    tagList(
      map(seq_len(nrow(x)), \(i) div_item(x[i, ]))
    )
}
