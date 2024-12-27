box::use(app/logic/api[api], app/logic/fetch[fetch])
library(future)
plan(multisession)

search_field_ui <- function(id) {
  style <- list(
    fieldGroup = list(
      # borderRadius = 10,
      border = "0px solid transparent", 
      background = "#f3f2f1"
    ), 
    field = list(color = "#ff0000")
  )
  ns <- NS(id)
  TextField.shinyInput(ns("searchField"), 
                       styles = style)
}

search_field_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    reactive(input$searchField)
  })
}

search_button_ui <- function(id) {
  ns <- NS(id)
  ActionButton.shinyInput(
    inputId = ns("searchButton"), 
    iconProps = list(iconName = "Search"))
}

search_button_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    reactive(input$searchButton)
  })
}

ui <- function(id) {
  ns <- NS(id)
  tags$head(
    tags$link(rel = "stylesheet", href = "static/css/app.min.css")
  )
  div(
    class = "search",
    search_field_ui(ns("box")),
    search_button_ui(ns("button")),
    uiOutput(ns("out"))
  )
}

server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    searchFieldValue <- search_field_server("box")
    searchButtonValue <- search_button_server("button")
    
    # Create the task with the correct namespaced button ID

    # Create ExtendedTasks in a shiny module -------------------------------------
    # According to the documentation: 
    # 1. The new method of the ExtendedTasks R6 class accepts 
    # function object as argument;
    # 2. Pass the the reactive values / expressions to the argument
    # of the lambda function in 1;
    # 3. It is a future promise inside the lambda function;
    # 4. Place your expensive calculation inside the expression input 
    # to future function.
    # 5. Bind the task to bslib::bind_task_button() with an id. 
    # For a module, make sure provide the full id of the UI inputId
    
    fetchJob <- ExtendedTask$new(function(x) {
      future(
        {
          # Slow operation goes here
          fetch(api(x), header_toml = "../static/headers.toml")
        },
        seed = TRUE
      )
    })
    bslib::bind_task_button(fetchJob, ns("button-searchButton"))
    

    # Invoke ExtendedTasks in a shiny module ----------------------------------------------------
    # According to the documentation: 
    # 1. ExtendedTasks$invoke() is a reactive; 
    # 2. Reactive values / expressions can only be used
    # in reactive consumers such as reactive(), observe(), 
    # observeEvent(), eventReactive() or render* functions;
    # 3. Specially, reactiveVal() will throw an error.
    
    observeEvent(searchButtonValue(), {
      if(nchar(stringr::str_replace_all(searchFieldValue(), "\\s", "")) == 0) {
        output$out <- renderUI({
          textOutput(ns("nothing"))
        })
      } else {
        output$out <- renderUI({
          fetchJob$invoke(searchFieldValue())
          DT::dataTableOutput(ns("table")) |> 
            shinycssloaders::withSpinner(caption = "Fetching data from API...")
        })
        # The invoke will now be triggered by the bound button automatically
      }
    })
    
    
    output$nothing <- renderText("Can't seem to find anything")
    

    # Call result(s) of ExtendedTasks --------------------------------------------
    # According to the documentation: 
    # 1. ExtendedTasks$results is a reactive; 
    # 2. It returns one of the these values: "initial", "running",
    # "success", "error";
    # 3. Specially, DO NOT use this method with observeEvent(), 
    # eventReactive(), bindEvent(), isolate()
    
    output$table <- DT::renderDataTable({
      fetchJob$result()
    })
  })
}

shinyApp(ui("app"), function(input, output) server("app"))
