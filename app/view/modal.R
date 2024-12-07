library(shiny)
library(shiny.fluent)

set.seed(123)
record <- dplyr::slice_sample(movie, n = 1)
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
        .record-grid-item {padding:20px;border:1px solid #ccc;}"
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
        minWidth = 800, maxWidth = 1440,
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
                                   borderless = TRUE, value = record$subject_id, disabled = TRUE)
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
              region_dropdown(ns("modalRegion"), label = "Region", multiple = TRUE, borderless = TRUE)
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
