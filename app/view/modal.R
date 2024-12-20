library(shiny)
library(shiny.fluent)

box::use(
  stringr[str_replace, str_split],
  purrr[reduce, map, map2],
  app/logic/collection[collection],
  app/logic/api[api],
  app/view/grid_view[genre_dropdown, region_dropdown],
)

movie <- collection("database")$data$movie
book <- collection("database")$data$book
music <- collection("database")$data$music
game <- collection("database")$data$game
set.seed(100)
movie_record <- movie |>
  dplyr::filter(stringr::str_count(region, "\\s") >= 1) |>
  dplyr::slice_sample(n = 1)
book_record <- book |>
  dplyr::slice_sample(n = 1)
music_record <- music |>
  dplyr::slice_sample(n = 1)
game_record <- game |>
  dplyr::slice_sample(n = 1)


movie_modal <- function(data, ns, dialog_type = NULL, heading = NULL, subheading = NULL, 
                        widths = NULL, hidden = NULL, 
                        dismissId = NULL, modal_props = list(), 
                        footer_button = c("Update", "Cancel")) {
  
  # default arguments -------------------------------------------------------
  
  list2env(data, environment())
  dialog_type <- dialog_type %||% 0
  widths <- widths %||% c(500, 960)
  API <- api(url)
  heading <- heading %||% sprintf("编辑条目:%s", title)
  subheading <- subheading %||% sprintf("API: /%s/%s/%s", API$domain, API$schema, API$id)
  dismissId <- dismissId %||% "hideDialog"
  
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
  assertthat::assert_that(is_dialog_type(dialog_type))
  assertthat::assert_that(is_valid_widths(widths))
  
  # static data -------------------------------------------------------------
  
  dropdown_options <- function(inputId, label) {
    list(key = inputId, text = label)
  }
  dialogContentProps <- list(
    type = type, 
    title = heading, 
    closeButtonAriaLabel = "Close", 
    subText = subheading
  )
  onDismiss <- JS(paste0(
    "function() {",
    "  Shiny.setInputValue('", ns(dismissId),"', Math.random());",
    "}"
  ))
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
        if(type != "game") {
          div(
            class = "record-grid-item", 
            TextField.shinyInput(inputId = ns("modalYear"), ariaLabel = "Year", label = "Year", 
                                 borderless = TRUE, underlined = TRUE, value = year)
          )
        } else {
          div(
            class = "record-grid-item", 
            TextField.shinyInput(inputId = ns("modalRelease"), ariaLabel = "Release", label = "Release", 
                                 borderless = TRUE, underlined = TRUE, value = release)
          )
        },
        switch(
          type,
          "movie" = div(
            class = "record-grid-item", 
            region_dropdown(ns("modalRegion"), label = "Region", multiple = TRUE, 
                            value = reduce(str_split(region, "\\s"), c))
          ),
          "book" = div(
            class = "record-grid-item", 
            TextField.shinyInput(inputId = ns("modalAuthor"), ariaLabel = "Author", label = "Author", 
                                 borderless = TRUE, underlined = TRUE, value = author)
          ), 
          "music" = div(
            class = "record-grid-item", 
            TextField.shinyInput(inputId = ns("modalPerformer"), ariaLabel = "Performer", label = "Performer", 
                                 borderless = TRUE, underlined = TRUE, value = performer)
          ),
          "game" = div(
            class = "record-grid-item", 
            TextField.shinyInput(inputId = ns("modalCategory"), ariaLabel = "Category", label = "Category", 
                                 borderless = TRUE, underlined = TRUE, value = category)
          )
        ),
        switch(
          type, 
          "movie" = div(
            class = "record-grid-item", 
            genre_dropdown(ns("modalGenre"), label = "Genre", multiple = TRUE, 
                           value = reduce(str_split(genre, "\\s"), c))
          ), 
          "book" = div(
            class = "record-grid-item", 
            TextField.shinyInput(inputId = ns("modalPublisher"), ariaLabel = "Publisher", label = "Publisher", 
                                 borderless = TRUE, underlined = TRUE, value = publisher)
          ), 
          "game" = div(
            class = "record-grid-item", 
            TextField.shinyInput(inputId = ns("modalDeveloper"), ariaLabel = "Developer", label = "Developer", 
                                 borderless = TRUE, underlined = TRUE, value = developer)
          )
        ),
        switch(
          type, 
          "movie" = div(
            class = "record-grid-item", 
            TextField.shinyInput(inputId = ns("modalDirector"), ariaLabel = "Director", label = "Director",
                                 borderless = TRUE, underlined = TRUE, value = director)
          )
        ),
        switch(
          type, 
          "movie" = div(
            class = "record-grid-item", 
            TextField.shinyInput(inputId = ns("modalStarring"), ariaLabel = "Starring", label = "Starring",
                                 borderless = TRUE, underlined = TRUE, value = starring)
          )
        ),
        switch(
          type, 
          "movie" =  div(
            class = "record-grid-item", 
            TextField.shinyInput(inputId = ns("modalSite"), ariaLabel = "Site", label = "Site", 
                                 borderless = TRUE, underlined = TRUE, value = site)
          )
        ), 
        div(
          class = "record-grid-item", 
          Dropdown.shinyInput(inputId = ns("modalStatus"), label = "Status", value = status,
                              options = switch(type, 
                                               "movie" = map(c("想看", "在看", "看过"), \(x) dropdown_options(x, x)),
                                               "book"  = map(c("想读", "在读", "读过"), \(x) dropdown_options(x, x)),
                                               "music" = map(c("想听", "在听", "听过"), \(x) dropdown_options(x, x)),
                                               "game"  = map(c("想玩", "在玩", "玩过"), \(x) dropdown_options(x, x))))
        ),
        div(
          class = "record-grid-item", style = "padding-top:2.5rem;",
          TextField.shinyInput(inputId = ns("modalRating"), ariaLabel = "Rating", label = "Rating", 
                               borderless = TRUE, underlined = TRUE, value = rating)
        ), 
        div(
          class = "record-grid-item",
          SpinButton.shinyInput(inputId = ns("modalMyrating"), ariaLabel = "Myrating", label = "My Rating", 
                                min = 0, max = 5, step = 1, value = ifelse(is.na(my_rating), 0, my_rating))
        ),
        div(
          class = "record-grid-item",
          TextField.shinyInput(inputId = ns("modalURL"), ariaLabel = "Url", label = "URL", 
                               borderless = TRUE, underlined = TRUE, value = url, disabled = TRUE)
        ),
        div(
          class = "record-grid-item", 
          uiOutput(ns("modalTimestamp"))
        ), 
        div(
          class = "record-grid-item", 
          div(style = "display:flex;gap:1rem;",
              Text(style = list(`font-weight` = "600", `font-size` = "14px"), "Cover"),
              div(
                uiOutput(ns("modalCover")),
                ActionButton.shinyInput(inputId = ns("modalImgUpload"), iconProps = list(iconName = "Upload"), 
                                        text = "Upload Cover...")
              )
          )
        )
    ),
    DialogFooter(
      PrimaryButton.shinyInput(ns("dialogUpdate"), text = footer_button[1]),
      DefaultButton.shinyInput(ns("dialogCancel"), text = footer_button[2])
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


server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    isDialogOpen <- reactiveVal(FALSE)
    imgFile <- reactiveVal(NULL)
    output$reactDialog <- renderReact({
      movie_modal(data = data, 
                  ns = ns, 
                  dialog_type = 0, 
                  hidden = !isDialogOpen(), 
                  widths = c(500, 960))
    })
    
    observeEvent(input$showDialog, isDialogOpen(TRUE))
    observeEvent(input$hideDialog, isDialogOpen(FALSE))
    observeEvent(input$dialogUpdate, isDialogOpen(FALSE))
    observeEvent(input$dialogCancel, isDialogOpen(FALSE))
    
    output$modalCover <- renderUI({
      img_src <- ifelse(is.null(imgFile()), 
                        sprintf("static/cover/%s/%s.%s", data$type, data$subject_id, tools::file_ext(data$cover)),
                        imgFile())
      Image(src = img_src, style = list(`max-width` = "100px"), alt = data$title)
      # tags$img(src = img_src, alt = data$title, style = "max-width:100px;")
    })
    observeEvent(input$modalImgUpload, {
      f <- tryCatch(file.choose(), error = function(e) NULL)
      if(!is.null(f)) {
        f <- stringr::str_extract(f, "static.+") |> 
          stringr::str_replace_all("\\\\", "/") 
        imgFile(f)
      }
    })
    output$modalTimestamp <- renderUI({
      if(is.null(imgFile())) {
        TextField.shinyInput(inputId = ns("modalCreatedAt"), ariaLabel = "CreatedAt", label = "Created At", 
                             borderless = TRUE, underlined = TRUE, disabled = TRUE, value = as.character(data$created_at))
      } else {
        current_time <- Sys.time() |> as.character() |> stringr::str_replace("\\..+", "")
        TextField.shinyInput(inputId = ns("modalUpdatedAt"), ariaLabel = "UpdatedAt", label = "Updated At", 
                             borderless = TRUE, underlined = TRUE, disabled = TRUE, value = current_time)
      }
    })
  })
}

if (interactive()) {
  shinyApp(ui("app"), function(input, output) server("app", data = record))
}
