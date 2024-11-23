box::use(
  shiny[div, tagList],
  purrr[map]
)

box::use(
  app/view/grid_view_helper[gauge_path, divs]
)

div_item <- function(..., class = "item") {
  div(
    class = class, 
    gauge_path(...),
    div(
      class = "info",
      divs(..., class = "title"),
      divs(..., class = "meta")
    )
  )
}

#' @export
grid_view <- function(x) {
    tagList(
      map(seq_len(nrow(x)), \(i) div_item(x[i, ]))
    )
}
