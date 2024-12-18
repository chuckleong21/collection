library(shiny)
library(shiny.fluent)

box::use(
  stringr[str_replace, str_split],
  purrr[reduce, map2],
  app/logic/collection[collection],
  app/logic/api[api],
  app/view/grid_view[genre_dropdown, region_dropdown]
)

movie <- collection("database")$data$movie
set.seed(100)
record <- movie |> 
  dplyr::filter(stringr::str_count(region, "\\s") >= 1) |> 
  dplyr::slice_sample(n = 1)

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

movie_modal <- function(data, ns, type, title = NULL, subtitle = NULL, 
                        widths, hidden = NULL, 
                        dismissId = NULL, modal_props = list(), 
                        footer_button = c("Update", "Cancel")) {
  
  # argument checks ---------------------------------------------------------
  
  is_dialog_type <- function(x) {
    is_scalar_numeric <- function(x) length(x) == 1 && is.numeric(x)
    is_scalar_numeric(x) && x %in% 0:2
  } 
  is_valid_widths <- function(x) is.numeric(x) & (length(x) >=1 & length(x) <=2)
  assertthat::on_failure(is_dialog_type) <- function(call, env) {
    sprintf("DialogType is either 0, 1, 2, not %s", deparse(call$x))
  }
  assertthat::on_failure(is_valid_widths) <- function(call, env) {
    sprintf("Maximum length of 'widths' is %g instead of 2", length(call$x))
  }
  assertthat::assert_that(is_dialog_type(type))
  assertthat::assert_that(is_valid_widths(widths))
  
  # static data -------------------------------------------------------------
  
  list2env(data, environment())
  dropdown_options <- function(inputId, label) {
    list(key = inputId, text = label)
  }
  API <- api(url)
  title <- title %||% "编辑条目"
  subtitle <- subtitle %||% sprintf("API: /%s/%s/%s", API$domain, API$schema, API$id)
  dismissId <- dismissId %||% "hideDialog"
  dialogContentProps <- list(
    type = type, 
    title = title, 
    closeButtonAriaLabel = "Close", 
    subText = subtitle
  )
  onDismiss <- JS(paste0(
        "function() {",
        "  Shiny.setInputValue('", ns(dismissId),"', Math.random());",
        "}"
      ))
  dismissId <- dismissId %||% "hideDialog"
  assertthat::assert_that(inherits(onDismiss, "JS_EVAL"), 
                          msg = "onDismiss is not a JavaScript Evaluation")
  
  Dialog(
    minWidth = min(widths), maxWidth = max(widths),
    hidden = hidden, onDismiss = onDismiss,
    dialogContentProps = dialogContentProps, 
    modalProps = modal_props,
    
    # modal contents ----------------------------------------------------------
    
    div(class = "record-grid", 
        div(
          class = "record-grid-item",
          TextField.shinyInput(inputId = ns("modalSubjectID"), ariaLabel = "SubjectID", label = "ID", 
                               borderless = TRUE, underlined = TRUE, value = subject_id, disabled = TRUE)
        ),
        div(
          class = "record-grid-item",
          TextField.shinyInput(inputId = ns("modalTitle"), ariaLabel = "Title", label = "Title", 
                               borderless = TRUE, underlined = TRUE, value = title)
        ),
        div(
          class = "record-grid-item", 
          TextField.shinyInput(inputId = ns("modalYear"), ariaLabel = "Year", label = "Year", 
                               borderless = TRUE, underlined = TRUE, value = year)
        ),
        div(
          class = "record-grid-item", 
          region_dropdown(ns("modalRegion"), label = "Region", multiple = TRUE, 
                          value = reduce(str_split(region, "\\s"), c))
        ),
        div(
          class = "record-grid-item", 
          genre_dropdown(ns("modalGenre"), label = "Genre", multiple = TRUE, 
                         value = reduce(str_split(genre, "\\s"), c))
        ),
        div(
          class = "record-grid-item", 
          TextField.shinyInput(inputId = ns("modalDirector"), ariaLabel = "Director", label = "Director",
                               borderless = TRUE, underlined = TRUE, value = director)
        ),
        div(
          class = "record-grid-item", 
          TextField.shinyInput(inputId = ns("modalStarring"), ariaLabel = "Starring", label = "Starring",
                               borderless = TRUE, underlined = TRUE, value = starring)
        ),
        div(
          class = "record-grid-item", 
          TextField.shinyInput(inputId = ns("modalSite"), ariaLabel = "Site", label = "Site", 
                               borderless = TRUE, underlined = TRUE, value = site)
        ), 
        div(
          class = "record-grid-item", 
          Dropdown.shinyInput(inputId = ns("modalStatus"), label = "Status", value = status,
                              options = map(c("想看", "在看", "看过"), \(x) dropdown_options(x, x)))
        ),
        div(
          class = "record-grid-item", style = "padding-top:2.5rem;",
          TextField.shinyInput(inputId = ns("modalRating"), ariaLabel = "Rating", label = "Rating", 
                               borderless = TRUE, underlined = TRUE, value = rating)
        ), 
        div(
          class = "record-grid-item",
          SpinButton.shinyInput(inputId = ns("modalMyrating"), ariaLabel = "Myrating", label = "My Rating", 
                                min = 0, max = 5, step = 1, value = my_rating)
        ),
        div(
          class = "record-grid-item",
          TextField.shinyInput(inputId = ns("modalURL"), ariaLabel = "Url", label = "URL", 
                               borderless = TRUE, underlined = TRUE, value = url, disabled = TRUE)
        ),
        div(
          class = "record-grid-item", 
          TextField.shinyInput(inputId = ns("modalCreatedAt"), ariaLabel = "CreatedAt", label = "Created At", 
                               borderless = TRUE, underlined = TRUE, disabled = TRUE, value = as.character(created_at))
        ), 
        div(
          class = "record-grid-item", 
          div(style = list(display = "flex", gap = "0.5rem"),
              Image(src = sprintf("static/cover/movie/%s.%s", subject_id, tools::file_ext(cover)),
                    style = list(`max-width` = "100px")
              ),
              fileInput(inputId = ns("modalCover"), label = NULL, multiple = FALSE, accept = c(".webp", "png", "jpg"),
                        placeholder = "Upload a cover...")
          )
        )
    ),
    DialogFooter(
      PrimaryButton.shinyInput(ns("dialogSend"), text = footer_button[1]),
      DefaultButton.shinyInput(ns("dialogDontSend"), text = footer_button[2])
    )  
  )
}

server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    isDialogOpen <- reactiveVal(FALSE)
    output$reactDialog <- renderReact({
      # dialogContentProps <- list(
      #   type = 0,
      #   title = "Missing Subject",
      #   closeButtonAriaLabel = "Close",
      #   subText = "Do you want to send this message without a subject?"
      # )
      # dropdown_options <- function(inputId, label) {
      #   list(key = inputId, text = label)
      # }
      movie_modal(record, 
                  ns = ns, 
                  type = 0, 
                  hidden = !isDialogOpen(), 
                  widths = c(500, 960))
      # Dialog(
      #   minWidth = 500, maxWidth = 960,
      #   hidden = !isDialogOpen(),
      #   onDismiss = JS(paste0(
      #     "function() {",
      #     "  Shiny.setInputValue('", ns("hideDialog"),"', Math.random());",
      #     "}"
      #   )),
      #   dialogContentProps = dialogContentProps,
      #   modalProps = list(),
      #   div(class = "record-grid", 
      #       div(
      #         class = "record-grid-item",
      #         TextField.shinyInput(inputId = ns("modalSubjectID"), ariaLabel = "SubjectID", label = "ID", 
      #                              borderless = TRUE, underlined = TRUE, value = record$subject_id, disabled = TRUE)
      #       ),
      #       div(
      #         class = "record-grid-item",
      #         TextField.shinyInput(inputId = ns("modalTitle"), ariaLabel = "Title", label = "Title", 
      #                              borderless = TRUE, underlined = TRUE, value = record$title)
      #       ),
      #       div(
      #         class = "record-grid-item", 
      #         TextField.shinyInput(inputId = ns("modalYear"), ariaLabel = "Year", label = "Year", 
      #                              borderless = TRUE, underlined = TRUE, value = record$year)
      #       ),
      #       div(
      #         class = "record-grid-item", 
      #         region_dropdown(ns("modalRegion"), label = "Region", multiple = TRUE, 
      #                         value = reduce(str_split(record$region, "\\s"), c))
      #       ),
      #       div(
      #         class = "record-grid-item", 
      #         genre_dropdown(ns("modalGenre"), label = "Genre", multiple = TRUE, 
      #                        value = reduce(str_split(record$genre, "\\s"), c))
      #       ),
      #       div(
      #         class = "record-grid-item", 
      #         TextField.shinyInput(inputId = ns("modalDirector"), ariaLabel = "Director", label = "Director",
      #                              borderless = TRUE, underlined = TRUE, value = record$director)
      #       ),
      #       div(
      #         class = "record-grid-item", 
      #         TextField.shinyInput(inputId = ns("modalStarring"), ariaLabel = "Starring", label = "Starring",
      #                              borderless = TRUE, underlined = TRUE, value = record$starring)
      #       ),
      #       div(
      #         class = "record-grid-item", 
      #         TextField.shinyInput(inputId = ns("modalSite"), ariaLabel = "Site", label = "Site", 
      #                              borderless = TRUE, underlined = TRUE, value = record$site)
      #       ), 
      #       div(
      #         class = "record-grid-item", 
      #         Dropdown.shinyInput(inputId = ns("modalStatus"), label = "Status", 
      #                             options = purrr::map(c("想看", "在看", "看过"), \(x) dropdown_options(x, x)), 
      #                             value = record$status)
      #       ),
      #       div(
      #         class = "record-grid-item", style = "padding-top:2.5rem;",
      #         TextField.shinyInput(inputId = ns("modalRating"), ariaLabel = "Rating", label = "Rating", 
      #                              borderless = TRUE, underlined = TRUE, value = record$rating)
      #       ), 
      #       div(
      #         class = "record-grid-item",
      #         SpinButton.shinyInput(inputId = ns("modalMyrating"), ariaLabel = "Myrating", label = "My Rating", 
      #                               min = 0, max = 5, step = 1, value = record$my_rating)
      #       ),
      #       div(
      #         class = "record-grid-item",
      #         TextField.shinyInput(inputId = ns("modalURL"), ariaLabel = "Url", label = "URL", 
      #                              borderless = TRUE, underlined = TRUE, value = record$url, disabled = TRUE)
      #       ),
      #       div(
      #         class = "record-grid-item", 
      #         TextField.shinyInput(inputId = ns("modalCreatedAt"), ariaLabel = "CreatedAt", label = "Created At", 
      #                              borderless = TRUE, underlined = TRUE, disabled = TRUE, value = as.character(record$created_at))
      #       ), 
      #       div(
      #         class = "record-grid-item", 
      #         div(style = list(display = "flex", gap = "0.5rem"),
      #             Image(src = sprintf("static/cover/movie/%s.%s", record$subject_id, tools::file_ext(record$cover)),
      #                   style = list(`max-width` = "100px")
      #             ),
      #             fileInput(inputId = ns("modalCover"), label = NULL, multiple = FALSE, accept = c(".webp", "png", "jpg"),
      #                       placeholder = "Upload a cover...")
      #         )
      #       )
      #   ),
      #   DialogFooter(
      #     PrimaryButton.shinyInput(ns("dialogSend"), text = "Send"),
      #     DefaultButton.shinyInput(ns("dialogDontSend"), text = "Don't send")
      #   )
      # )
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
