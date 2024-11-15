library(shiny)
library(shiny.fluent)
box::use(
  app/view/filter_options[country_options],
  app/logic/import[import_to_database],
  app/view/grid_view[grid_view]
)

newdb <- import_to_database("app/static/豆伴(58485907)_2.xlsx")
movie <- newdb$data$movie

r <- movie[sample(seq_len(nrow(movie)), 1), ]



# Rendering headers and dividers inside dropdown
countries <- stringr::str_split(movie$region, "\\s") |>
  purrr::reduce(c) |>
  na.omit() |> 
  unique()

ui <- function(id) {
  ns <- shiny::NS(id)
  fluentPage(
    Dropdown.shinyInput(
      ns("country"), value = "", label = "国家",
      multiSelect = TRUE,
      options = country_options(countries, ns)
    ), 
    records <- grid_view(movie)
    # gt::gt_output(ns("table"))
  )
}


server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # session$ns -> function
    # session$ns(""): current namespace
    # output$selected <- renderPrint(c(str_replace(input$country, session$ns(""), "")))
    # output$table <- gt::render_gt({
    #   filtered_table <- reactive({
    #     pattern <- stringr::str_c(stringr::str_replace(input$country, session$ns(""), ""), collapse = "|")
    #     movie |>
    #       dplyr::filter(stringr::str_detect(region, pattern))
    #   })
    #   req(input$country)
    #   filtered_table() |> gt::gt()
    # })
  })
}

if (interactive()) {
  shiny::shinyApp(ui("app"), function(input, output) server("app"))
}


movie |>
  dplyr::mutate(cover = purrr::map2_vec(type, subject_id, local_cover)) |>
  gt::gt() |>
  gtExtras::gt_img_rows(cover, img_source = "local", height = 80)

local_cover <- function(type, id) {
  p <- paste0("app/static/cover/", type)
  list.files(p, full.names = T)[stringr::str_which(list.files(p), as.character(id))]
}
