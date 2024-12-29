box::use(
  shiny[div, tagList, NS, tags, moduleServer, HTML,
        span, reactiveVal, reactiveValues, insertUI,
        observeEvent],
  purrr[map, map_vec, map2, list_flatten, reduce, 
        set_names, pluck_depth],
  shiny.fluent[Dropdown.shinyInput],
  shiny.react[JS],
  stringr[str_split],
  dplyr[group_by, group_split], 
  stats[na.omit],
  assertthat[assert_that]
)

box::use(
  app/logic/collection[collection],
  app/view/grid_view_helper[gauge_path, divs],
  app/view/constant[genre_colors, category_colors, regions]
)

div_item <- function(ns, ..., class = "item") {
  div(
    class = class, 
    gauge_path(...),
    div(
      class = "intro",
      divs(..., class = "title"),
      divs(..., class = "meta"),
    ),
    divs(..., class = "edit", ns = ns)
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
  
  regions_list <- regions |> 
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
  out <- map(genre_colors, \(x) dropdown_options(x, x)) |> unname()
  Dropdown.shinyInput(inputId = inputId, label = label, multiSelect = multiple, options = out, ...)
}

#' @export
category_dropdown <- function(inputId, label, multiple, ...) {
  dropdown_options <- function(inputId, label) {
    list(key = inputId, text = label)
  }
  out <- map(category_colors, \(x) dropdown_options(x, x)) |> unname()
  Dropdown.shinyInput(inputId = inputId, label = label, multiSelect = multiple, options = out, ...)
}

#' @export
grid_view <- function(x, ns) {
  div(class = "grid-item",
      tagList(
        map(seq_len(nrow(x)), \(i) div_item(ns, x[i, ]))
      )
  )
}

#' IntersectionObserver API
#' 
#' @description 
#' Allow infinite scroll for the UI. 
#' Read more at \href{https://www.appsilon.com/post/infinite-scrolling-in-r-shiny#scrollvspagination}{Infinite Scrolling in R Shiny}
#' by Appsilon.
#'
#' @param event_id A string for Shiny inputId binding
#' @param observer_id A string for the JS query selector id
#' @param ns A function returned by \code{shiny::NS}
#'
#' @return A HTML character
#' @export
#'
observerJS <- function(event_id, observer_id, ns = NULL) {
  if(!is.null(ns)) {
    assert_that(is.function(ns))
    event_id <- ns(event_id)
    observer_id <- ns(observer_id)
  }
  HTML(sprintf(
    ' $(document).ready(function() { const observer = new IntersectionObserver(function(entries) { if (entries[0].intersectionRatio > 0) {
            Shiny.setInputValue("%s", true, { priority: "event" });
          }
        });

        observer.observe(document.querySelector("#%s"));
      })
    ', event_id, observer_id
  ))
}

#' Data Grid View UI
#' 
#' @param id A string. namespace ID
#'
#' @return A \code{shiny.tag} object
#' @export
#' 
ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$script(observerJS("list_end_reached", "end", ns = ns)),
    div(div(id = ns("end")))
  )
}

#' Data Grid View Server
#' 
#' @param id A string. namespace ID
#'
#' @param data A \code{data.frame} or a \code{tibble}
#' @param page_size How many rows of data shown in a page? \cr Default to 20
#'
#' @export
#' 
server <- function(id, data, page_size = 20) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    last_page <- ceiling(nrow(data) / page_size)
    end_collection_ui <- div(
      id = ns("endCollection"), 
      style = "background:#7d7d7d;display:flex; align-items:center; justify-content:center;",
      span("End of collection", style = "color:#e1e1e1;")
    )
    current_page <- reactiveVal(1)
    init_data_ui <- grid_view(data[1:page_size, ], ns = ns)
    insertUI(selector = sprintf("#%s", ns("end")), where = "beforeBegin", ui = init_data_ui)
    
    observeEvent(input$list_end_reached, {
      Sys.sleep(0.5)
      current_page(current_page() + 1)
      current_ind <- reactiveValues(
        start = 1 + page_size * (current_page() - 1),
        end = ifelse(page_size * current_page() > nrow(data), 
                     nrow(data), page_size * current_page())
      )
      if(current_page() <= ceiling(nrow(data) / page_size)) {
        data_ui <- grid_view(data[current_ind$start:current_ind$end, ], ns = ns)
        insertUI(selector = sprintf("#%s", ns("end")), where = "beforeBegin", ui = data_ui)
      }
      if(current_page() == ceiling(nrow(data) / page_size)) {
        insertUI(
          selector = sprintf("#%s", ns("end")),
          where = "afterEnd", 
          ui = end_collection_ui
        )
      }
    })
  })
}