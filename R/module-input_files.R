input_data_files_module <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::fluidRow(
      shiny::column(12, shiny::selectInput(ns("sessions_selector"),
        "Sessions",
        choices = NULL,
        selected = NULL
      ))
    ),
    shiny::fluidRow(
      shiny::column(12, shiny::selectInput(ns("epochs_selector"),
        "Epochs",
        choices = NULL,
        selected = NULL
      ))
    )
  )
}

input_sessions_files_server <- function(id, folder_path) {
  shiny::moduleServer(id, function(input, output, session) {
    files <- shiny::reactive({
      shiny::req(folder_path())
      files <- list_files(folder_path())
    })

    shiny::observe({
      shiny::updateSelectInput(session, "sessions_selector", choices = files(), selected = "")
    })

    selected_sessions <- shiny::reactiveVal(NULL)

    # Clear selected_sessions whenever folder_path changes
    shiny::observeEvent(folder_path(), {
      selected_sessions(NULL)
      shiny::updateSelectInput(session, "sessions_selector", selected = "")
    })

    shiny::observeEvent(input$sessions_selector, {
      selected_sessions(input$sessions_selector)
    })

    selected_sessions
  })
}

input_epochs_files_server <- function(id, folder_path, sessions) {
  shiny::moduleServer(id, function(input, output, session) {
    files <- shiny::reactive({
      shiny::req(folder_path())
      list_files(folder_path())
    })

    shiny::observe({
      shiny::updateSelectInput(session, "epochs_selector", choices = files(), selected = "")
    })

    selected_epochs <- shiny::reactiveVal(NULL)

    # Clear selected_epochs whenever folder_path changes
    shiny::observeEvent(folder_path(), {
      selected_epochs(NULL)
      shiny::updateSelectInput(session, "epochs_selector", selected = "")
      shinyjs::disable("epochs_selector")
    })

    shiny::observe({
      shiny::req(sessions())
      shinyjs::enable("epochs_selector")
    })

    shiny::observeEvent(input$epochs_selector, {
      selected_epochs(input$epochs_selector)
    })

    selected_epochs
  })
}

list_files <- function(folder_path) {
  if (dir.exists(folder_path)) {
    filenames <- list.files(folder_path, full.names = FALSE, pattern = "\\.csv$")
    display_names <- gsub("\\.csv$", "", filenames)
    stats::setNames(filenames, display_names)
  } else {
    character(0)
  }
}
