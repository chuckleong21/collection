library(shiny)
library(shiny.fluent)
library(bslib)

makePage <- function(title, subtitle, main_color = "#323130", contents) {
  tagList(div(
    class = "page-title", 
    span(title, class = "ms-fontSize-32 ms-fontWeight-semibold", 
         style = sprintf("color:%s", main_color)),
    span(subtitle, class = "ms-fontSize-14 ms-fontWeight-regular", 
         style = "color:#605E5C; margin:14px")
  ),
  contents)
}

makeCard <- function(title, content, size = 12, style = "") {
  div(
    class = sprintf("card ms-depth-8 ms-sm%g ms-xl%g", size, size),
    style = style, 
    Stack(
      tokens = list(childrenGap = 5), 
      Text(variant = "large", title, block = TRUE),
      content
    )
  )
}

layout <- function(mainUI) {
  div(class = "grid-container", 
      div(class = "header", header), 
      div(class = "sidenav", nagvigation), 
      div(class = "main", mainUI), 
      div(class = "footer", footer))
}