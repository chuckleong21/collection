box::use(
  stringr[str_replace, str_replace_all, str_extract, str_split],
  assertthat[assert_that],
  purrr[reduce, map],
  utils[head, tail], 
  tools[file_ext],
  shiny[NS, div, uiOutput, renderUI, moduleServer, 
        reactive, reactiveVal, observeEvent],
  shiny.react[JS],
  shiny.fluent[Dialog, DialogFooter, TextField.shinyInput, SpinButton.shinyInput, 
               Dropdown.shinyInput, ActionButton.shinyInput, PrimaryButton.shinyInput, 
               DefaultButton.shinyInput, Text, Image, reactOutput, renderReact],
  app/logic/api[api],
  app/logic/argscheck[...],
  app/view/grid_view[genre_dropdown, region_dropdown, category_dropdown]
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  reactOutput(ns("dialog"))
}

#' @export
server <- function(id, data, config = NULL, modalProps = list(), isOpen, coverPath) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    list2env(data, environment())
    API <- api(url)
    # dialog config -------------------------------------------------------
    config <- config %||% list(
      dialog_type = 0, 
      widths = c(500, 960), 
      heading = sprintf("编辑条目:%s", title),
      subheading = sprintf("API: /%s/%s/%s", API$domain, API$schema, API$id),
      dismissId = "hideDialog",
      footer_button = c("Update", "Cancel")
    )
    
    # static data -------------------------------------------------------------
    
    dropdown_options <- function(inputId, label) {
      list(key = inputId, text = label)
    }
    dialogContentProps <- list(
      type                 = config$dialog_type, 
      title                = config$heading, 
      subText              = config$subheading,
      closeButtonAriaLabel = "Close"
    )
    onDismiss <- JS(paste0(
      "function() {",
      "  Shiny.setInputValue('", ns(config$dismissId),"', Math.random());",
      "}"
    ))
    
    # argument checks ---------------------------------------------------------
    
    assert_that(is_valid_widths(config$widths))
    assert_that(is_dialog_type(config$dialog_type))
    assert_that(inherits(onDismiss, "JS_EVAL"), 
                msg = "onDismiss is not a JavaScript Evaluation")
    
    # server output -----------------------------------------------------------
    
    output$dialog <- renderReact({
      Dialog(
        minWidth = min(config$widths), maxWidth = max(config$widths),
        onDismiss = onDismiss, dialogContentProps = dialogContentProps,
        modalProps = modalProps,
        hidden = !isOpen(), 
        
        # dialog contents ----------------------------------------------------------
        
        div(class = "record-grid", 
            # subject_id --------------------------------------------------------------
            div(
              class = "record-grid-item",
              TextField.shinyInput(inputId = ns("modalSubjectID"), ariaLabel = "SubjectID", label = "ID", 
                                   borderless = TRUE, underlined = TRUE, value = subject_id, disabled = TRUE)
            ),
            # url ---------------------------------------------------------------------
            div(
              class = "record-grid-item",
              TextField.shinyInput(inputId = ns("modalURL"), ariaLabel = "Url", label = "URL", 
                                   borderless = TRUE, underlined = TRUE, value = url, disabled = TRUE)
            ),
            # timestamp ---------------------------------------------------------------
            div(
              class = "record-grid-item", 
              uiOutput(ns("modalTimestamp"))
            ), 
            # rating ------------------------------------------------------------------
            div(
              class = "record-grid-item", 
              # style = "padding-top:2.5rem;",
              # TextField.shinyInput(inputId = ns("modalRating"), ariaLabel = "Rating", label = "Rating", 
              #                      borderless = TRUE, underlined = TRUE, value = rating),
              SpinButton.shinyInput(inputId = ns("modalRating"), ariaLbel = "Rating", label = "Rating", 
                                    min = 0, max = 10, step = 0.1, disabled = TRUE, 
                                    value = ifelse(is.na(rating), 0, rating))
            ), 
            # title -------------------------------------------------------------------
            div(
              class = "record-grid-item",
              TextField.shinyInput(inputId = ns("modalTitle"), ariaLabel = "Title", label = "Title", 
                                   borderless = TRUE, underlined = TRUE, value = title)
            ),
            # year --------------------------------------------------------------------
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
            # region/category ---------------------------------------------------------
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
                category_dropdown(inputId = ns("modalCategory"), label = "Category", multiple = TRUE, 
                                  value = reduce(str_split(category, "\\s"), c))
              )
            ),
            # genre -------------------------------------------------------------------
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
            # director ----------------------------------------------------------------
            switch(
              type, 
              "movie" = div(
                class = "record-grid-item", 
                TextField.shinyInput(inputId = ns("modalDirector"), ariaLabel = "Director", label = "Director",
                                     borderless = TRUE, underlined = TRUE, value = director)
              )
            ),
            # starring ----------------------------------------------------------------
            switch(
              type, 
              "movie" = div(
                class = "record-grid-item", 
                TextField.shinyInput(inputId = ns("modalStarring"), ariaLabel = "Starring", label = "Starring",
                                     borderless = TRUE, underlined = TRUE, value = starring)
              )
            ),
            # site --------------------------------------------------------------------
            switch(
              type, 
              "movie" =  div(
                class = "record-grid-item", 
                TextField.shinyInput(inputId = ns("modalSite"), ariaLabel = "Site", label = "Site", 
                                     borderless = TRUE, underlined = TRUE, value = site)
              )
            ), 
            # status ------------------------------------------------------------------
            div(
              class = "record-grid-item", 
              Dropdown.shinyInput(inputId = ns("modalStatus"), label = "Status", value = status,
                                  options = switch(type, 
                                                   "movie" = map(c("想看", "在看", "看过"), \(x) dropdown_options(x, x)),
                                                   "book"  = map(c("想读", "在读", "读过"), \(x) dropdown_options(x, x)),
                                                   "music" = map(c("想听", "在听", "听过"), \(x) dropdown_options(x, x)),
                                                   "game"  = map(c("想玩", "在玩", "玩过"), \(x) dropdown_options(x, x))))
            ),
            # myrating ----------------------------------------------------------------
            div(
              class = "record-grid-item",
              SpinButton.shinyInput(inputId = ns("modalMyrating"), ariaLabel = "Myrating", label = "My Rating", 
                                    min = 0, max = 5, step = 1, value = ifelse(is.na(my_rating), 0, my_rating))
            ),
            # cover -------------------------------------------------------------------
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
        # footer ------------------------------------------------------------------
        DialogFooter(
          PrimaryButton.shinyInput(ns("dialogUpdate"), text = head(config$footer_button, 1)),
          DefaultButton.shinyInput(ns("dialogCancel"), text = tail(config$footer_button[2], 1))
        )  
      )
    })
    
    # server logic ------------------------------------------------------------
    output$modalTimestamp <- renderUI({
      if(is.null(coverPath())) {
        TextField.shinyInput(inputId = ns("modalCreatedAt"), ariaLabel = "CreatedAt", label = "Created At",
                             borderless = TRUE, underlined = TRUE, disabled = TRUE, value = as.character(created_at))
      } else {
        current_time <- Sys.time() |> as.character() |> str_replace("\\..+", "")
        TextField.shinyInput(inputId = ns("modalUpdatedAt"), ariaLabel = "UpdatedAt", label = "Updated At",
                             borderless = TRUE, underlined = TRUE, disabled = TRUE, value = current_time)
      }
    })
    
    output$modalCover <- renderUI({
      img_src <- ifelse(is.null(coverPath()),
                        sprintf("static/cover/%s/%s.%s", type, subject_id, file_ext(cover)),
                        coverPath())
      Image(src = img_src, style = list(`max-width` = "100px"), Alt = title)
    })
    observeEvent(input$modalImgUpload, {
      f <- tryCatch(file.choose(), error = function(e) NULL)
      if(!is.null(f)) {
        f <- str_extract(f, "static.+") |>
          str_replace_all("\\\\", "/")
        coverPath(f)
      }
    })
    
    # sever values ------------------------------------------------------------
    list(
      dismiss = reactive(input$hideDialog),
      update  = reactive(input$dialogUpdate), 
      cancel  = reactive(input$dialogCancel)
    )
  })
}