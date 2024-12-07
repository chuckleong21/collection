library(shiny)
library(shiny.fluent)

set.seed(100)
record <- movie |> 
  dplyr::filter(stringr::str_count(region, "\\s") >= 1) |> 
  dplyr::slice_sample(n = 1)
modal_content <- function(ns) {
  Stack(tokens = list(padding = "15px", childrenGap = "10px"),
        div(style = list(display = "flex"),
            Text("Title", variant = "large"),
            div(style = list(flexGrow = 1)),
            IconButton.shinyInput(
              ns("hideModal"),
              iconProps = list(iconName = "Cancel")
            ),
        ),
        column(
          width = 6, 
          TextField.shinyInput(inputId = "modalTitle", ariaLabel = "Title", label = "Title", 
                               borderless = TRUE, underlined = TRUE, value = record$title)
        )
  )
}

ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$head(
      tags$style(
        ".record-grid {
        display:grid;grid-template-columns:repeat(2, 1fr);grid-template-rows:repeat(8, 1fr);gap:10px;padding:10px;
        }
        .record-grid-item {padding:10px;}
        "
      )
    ),
    div(
      DefaultButton.shinyInput(ns("showDialog"), text = "Open dialog"),
      reactOutput(ns("reactDialog"))
    )
  )
}

server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    isDialogOpen <- reactiveVal(FALSE)
    output$reactDialog <- renderReact({
      dialogContentProps <- list(
        type = 0,
        title = "Missing Subject",
        closeButtonAriaLabel = "Close",
        subText = "Do you want to send this message without a subject?"
      )
      Dialog(
        minWidth = 500, maxWidth = 960,
        hidden = !isDialogOpen(),
        onDismiss = JS(paste0(
          "function() {",
          "  Shiny.setInputValue('", ns("hideDialog"),"', Math.random());",
          "}"
        )),
        dialogContentProps = dialogContentProps,
        modalProps = list(),
        div(class = "record-grid", 
            div(
              class = "record-grid-item",
              TextField.shinyInput(inputId = ns("modalSubjectID"), ariaLabel = "SubjectID", label = "ID", 
                                   borderless = TRUE, underlined = TRUE, value = record$subject_id, disabled = TRUE)
            ),
            div(
              class = "record-grid-item",
              TextField.shinyInput(inputId = ns("modalTitle"), ariaLabel = "Title", label = "Title", 
                                   borderless = TRUE, underlined = TRUE, value = record$title)
            ),
            div(
              class = "record-grid-item", 
              TextField.shinyInput(inputId = ns("modalYear"), ariaLabel = "Year", label = "Year", 
                                   borderless = TRUE, underlined = TRUE, value = record$year)
            ),
            div(
              class = "record-grid-item", 
              region_dropdown(ns("modalRegion"), label = "Region", multiple = TRUE, 
                              value = purrr::reduce(stringr::str_split(record$region, "\\s"), c))
            ),
            div(
              class = "record-grid-item", 
              genre_dropdown(ns("modalGenre"), label = "Genre", multiple = TRUE, 
                             value = purrr::reduce(stringr::str_split(record$genre, "\\s"), c))
            ),
            div(
              class = "record-grid-item", 
              TextField.shinyInput(inputId = ns("modalDirector"), ariaLabel = "Director", label = "Director",
                                   borderless = TRUE, underlined = TRUE, value = record$director)
            ),
            div(
              class = "record-grid-item", 
              TextField.shinyInput(inputId = ns("modalStarring"), ariaLabel = "Starring", label = "Starring",
                                   borderless = TRUE, underlined = TRUE, value = record$starring)
            ),
            div(
              class = "record-grid-item", 
              TextField.shinyInput(inputId = ns("modalSite"), ariaLabel = "Site", label = "Site", 
                                   borderless = TRUE, underlined = TRUE, value = record$site)
            ), 
            div(
              class = "record-grid-item", 
              Dropdown.shinyInput(inputId = ns("modalStatus"), label = "Status", 
                                 options = purrr::map(c("想看", "在看", "看过"), \(x) dropdown_options(x, x)), 
                                 value = record$status)
            ),
            div(
              class = "record-grid-item", style = "padding-top:2.5rem;",
              TextField.shinyInput(inputId = ns("modalRating"), ariaLabel = "Rating", label = "Rating", 
                                   borderless = TRUE, underlined = TRUE, value = record$rating)
            ), 
            div(
              class = "record-grid-item",
              SpinButton.shinyInput(inputId = ns("modalMyrating"), ariaLabel = "Myrating", label = "My Rating", 
                                   min = 0, max = 5, step = 1, value = record$rating)
            ),
            div(
              class = "record-grid-item",
              TextField.shinyInput(inputId = ns("modalURL"), ariaLabel = "Url", label = "URL", 
                                    borderless = TRUE, underlined = TRUE, value = record$url, disabled = TRUE)
            ),
            div(
              class = "record-grid-item", 
              TextField.shinyInput(inputId = ns("modalCreatedAt"), ariaLabel = "CreatedAt", label = "Created At", 
                                   borderless = TRUE, underlined = TRUE, value = as.character(record$created_at))
            ), 
            div(
              class = "record-grid-item", 
              div(style = list(display = "flex", gap = "0.5rem"),
                Image(src = paste0("static/cover/movie/", 
                                   stringr::str_replace(basename(record$cover), "^.+\\.", paste0(record$subject_id, "."))),
                      style = list(`max-width` = "100px")
                ),
                fileInput(inputId = ns("modalCover"), label = NULL, multiple = FALSE, accept = c(".webp", "png", "jpg"),
                          placeholder = "Upload a cover...")
              )
            )
        ),
        DialogFooter(
          PrimaryButton.shinyInput(ns("dialogSend"), text = "Send"),
          DefaultButton.shinyInput(ns("dialogDontSend"), text = "Don't send")
        )
      )
    })
    
    observeEvent(input$showDialog, isDialogOpen(TRUE))
    observeEvent(input$hideDialog, isDialogOpen(FALSE))
    observeEvent(input$dialogSend, isDialogOpen(FALSE))
    observeEvent(input$dialogDontSend, isDialogOpen(FALSE))
  })
}

if (interactive()) {
  shinyApp(ui("app"), function(input, output) server("app"))
}
