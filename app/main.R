box::use(
  shiny[bootstrapPage, div, moduleServer, NS, renderUI, tags, uiOutput],
  stats[na.omit],
  shiny.fluent[fluentPage, Dropdown.shinyInput]
)
box::use(
  app/logic/import[import_to_database],
  app/view/grid_view[region_dropdown, genre_dropdown, grid_view]
)

# ui <- function(id) {
#   ns <- NS(id)
#   bootstrapPage(
#     uiOutput(ns("message"))
#   )
# }

# server <- function(id) {
#   moduleServer(id, function(input, output, session) {
#     output$message <- renderUI({
#       div(
#         style = "display: flex; justify-content: center; align-items: center; height: 100vh;",
#         tags$h1(
#           tags$a("Check out Rhino docs!", href = "https://appsilon.github.io/rhino/")
#         )
#       )
#     })
#   })
# }

newdb <- import_to_database("app/static/豆伴(58485907)_2.xlsx")
movie <- newdb$data$movie

#' @export
ui <- function(id) {
  ns <- shiny::NS(id)
  fluentPage(
    region_dropdown("region", "Region", multiple = TRUE),
    grid_view(utils::tail(movie, 20), ns = ns)
    # gt::gt_output(ns("table"))
  )
}

#' @export
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
