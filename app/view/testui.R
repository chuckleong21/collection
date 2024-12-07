library(shiny)
library(bslib)
library(shiny.fluent)
options(shiny.reload = TRUE)

ui <- page_fluid(
  title = "Dynamic Dropdowns",
  
  bslib::card(
    ActionButton.shinyInput("add_dropdown", text = "Add Dropdown"),
    uiOutput("dynamic_dropdowns")
  )
)

server <- function(input, output, session) {
  # Counter for unique dropdown IDs
  counter <- reactiveVal(0)
  
  # Store the active dropdown IDs and their values
  active_dropdowns <- reactiveVal(list())
  dropdown_values <- reactiveVal(list())
  
  observeEvent(input$add_dropdown, {
    current_counter <- counter()
    current_dropdowns <- active_dropdowns()
    
    # Create new dropdown ID
    new_id <- paste0("dropdown_", current_counter)
    
    # Add to active dropdowns list
    current_dropdowns[[new_id]] <- TRUE
    active_dropdowns(current_dropdowns)
    
    # Initialize values for the new dropdown
    current_values <- dropdown_values()
    current_values[[new_id]] <- NULL
    current_values[[paste0(new_id, "_sub")]] <- NULL
    dropdown_values(current_values)
    
    # Increment counter
    counter(current_counter + 1)
  })
  
  # Observer to track dropdown values
  observe({
    current_dropdowns <- active_dropdowns()
    current_values <- dropdown_values()
    
    for (id in names(current_dropdowns)) {
      if (!is.null(input[[id]])) {
        current_values[[id]] <- input[[id]]
        current_values[[paste0(id, "_sub")]] <- input[[paste0(id, "_sub")]]
      }
    }
    
    dropdown_values(current_values)
  })
  
  output$dynamic_dropdowns <- renderUI({
    current_dropdowns <- active_dropdowns()
    current_values <- dropdown_values()
    
    dropdown_list <- lapply(names(current_dropdowns), function(id) {
      if (current_dropdowns[[id]]) {
        div(
          style = "display: flex; align-items: center; gap: 10px; margin-bottom: 10px;",
          div(
            # style = "flex-grow: 1;",
            Dropdown.shinyInput(
              id,
              value = current_values[[id]],
              placeholder = "Select an option",
              options = list(
                list(key = "1", text = "Option 1"),
                list(key = "2", text = "Option 2"),
                list(key = "3", text = "Option 3")
              )
            )
          ),
          # Conditional dropdown
          div(  
            conditionalPanel(
              condition = sprintf("input['%s'] == '1' || input['%s'] == '3'", id, id),
              Dropdown.shinyInput(
                paste0(id, "_sub"),
                value = current_values[[paste0(id, "_sub")]],
                placeholder = "Select a sub-option",
                options = list(
                  list(key = "a", text = "Sub-option A"),
                  list(key = "b", text = "Sub-option B"),
                  list(key = "c", text = "Sub-option C")
                )
              )
            )
          ),
          IconButton.shinyInput(
            paste0("delete_", id),
            iconProps = list(iconName = "Delete"),
            title = "Delete"
          )
        )
      }
    })
    
    div(dropdown_list)
  })
  
  # Handle deletion of dropdowns
  observe({
    current_dropdowns <- active_dropdowns()
    
    lapply(names(current_dropdowns), function(id) {
      delete_id <- paste0("delete_", id)
      
      observeEvent(input[[delete_id]], {
        # Remove from active dropdowns
        current_dropdowns <- active_dropdowns()
        current_dropdowns[[id]] <- NULL
        active_dropdowns(current_dropdowns)
        
        # Remove values
        current_values <- dropdown_values()
        current_values[[id]] <- NULL
        current_values[[paste0(id, "_sub")]] <- NULL
        dropdown_values(current_values)
      })
    })
  })
}

shinyApp(ui, server)
