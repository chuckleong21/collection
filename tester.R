library(shiny)
library(shiny.fluent)
box::use(
  app/view/filter_options[country_options],
  app/logic/import[import_to_database]
)
box::use(
  stringr[str_replace]
)

newdb <- import_to_database("app/static/豆伴(58485907)_2.xlsx")
movie <- newdb$data$movie


# Rendering headers and dividers inside dropdown
countries <- stringr::str_split(movie$region, "\\s") |>
  purrr::reduce(c) |>
  na.omit() |> 
  unique()

ui <- function(id) {
  ns <- NS(id)
  fluentPage(
    Dropdown.shinyInput(
      ns("country"), value = "", label = "国家",
      multiSelect = TRUE,
      options = country_options(countries, ns)
    ), 
    textOutput(ns("selected"))
  )
}


server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # session$ns -> function
    # session$ns(""): current namespace
    output$selected <- renderText(str_replace(input$country, session$ns(""), ""))
  })
}

if (interactive()) {
  shinyApp(ui("app"), function(input, output) server("app"))
}
