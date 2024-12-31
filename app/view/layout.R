box::use(
  app/view/search_bar
)

logo <- function() {
  tagList(
    tags$head(
      tags$link(rel = "stylesheet", href = "../static/css/app.min.css")
    ),
    tags$svg(
      width = "50", height = "50", 
      viewBox = "0 0 50 50", fill = "none",
      xmlns="http://www.w3.org/2000/svg",
      tags$rect(width = "48", height = "48", fill="none"),
      tags$path(class = "rotate-1", d = "M26 6C26 3.79086 27.7909 2 30 2H42C44.2091 2 46 3.79086 46 6V18C46 20.2091 44.2091 22 42 22H30C27.7909 22 26 20.2091 26 18V6Z"),
      tags$rect(class = "rotate-2", x = "2", y = "2", width = "20", height = "20", rx = "4"),
      tags$rect(class = "rotate-3", x = "2", y = "28", width = "20", height = "20", rx = "4"),
      tags$rect(class = "rotate-4", x = "26", y = "28", width = "20", height = "20", rx = "4")
    )
  )
}

# header ------------------------------------------------------------------

header <- div(
  class = "header", 
  tags$svg(),
  div(class = "title", Text(variant = "xLarge", title)),
  div(class = "subtitle", Text(variant = "Large", subtitle)),
  div(class = "search", search_bar$ui("search"))
)


# sidenav -----------------------------------------------------------------

sidenav <- "sidenav"


# footer ------------------------------------------------------------------

footer <- "footer"


# layout ------------------------------------------------------------------

#' @export
layout <- function(title, subtitle, mainUI) {
  tags$body(
    header, sidenav, mainUI, footer
  )
}
