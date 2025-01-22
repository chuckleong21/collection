box::use(
  app/view/search_bar
)

box::use(
  shiny.router[route_link]
)

logo <- div(
  class = "logo", 
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

# header ------------------------------------------------------------------

header <- function(title, search) {
  div(
    class = "header", logo, 
    Text(variant = "xLarge", title), 
    search
  )
} 

# sidenav -----------------------------------------------------------------

navItems <- list(
  list(name = "Home", url = route_link(""), key = "home", iconProps = list(iconName = "Home")),
  list(name = "Movie", url = route_link("movie"), key = "movie", iconProps = list(iconName = "MyMoviesTV")),
  list(name = "Music", url = "#!/music", key = "music", iconProps = list(iconName = "MusicInCollectionFill")),
  list(name = "Books", url = "#!/book", key = "book", iconProps = list(iconName = "Library")),
  list(name = "Games", url = "#!/game", key = "game", iconProps = list(iconName = "Game")),
  list(name = "About", url = "#!/about", key = "about", iconProps = list(iconName = "InfoSolid"))
)
navStyles <- list(root = list(
  height = "100%", 
  boxSizng = "border-box", 
  border = "1px solid #eee",
  overflowY = "auto") 
)
initialSelectedKey <- "home"
sidenav <- Nav(
  groups = list(list(links = navItems)), 
  initialSelectedKey = "home", 
  styles = navStyles
)


# footer ------------------------------------------------------------------

footer <- "footer"


# layout ------------------------------------------------------------------

#' @export
layout <- function(mainUI, ...) {
  div(
    class = "grid-container", 
    div(class = "header", header(...)), 
    div(class = "sidenav", sidenav,
        div(class = "footer", footer)), 
    div(class = "main", mainUI)
  )
}
