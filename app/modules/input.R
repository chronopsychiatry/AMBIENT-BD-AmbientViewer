input_folder_module <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
                 shinyFiles::shinyDirButton(ns("folder"),
                                            "Select Folder",
                                            "Please select the folder containing the data files"))
}

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

input_folder_server <- function(id, session) {
  shiny::moduleServer(id, function(input, output, session) {
    volumes <- shinyFiles::getVolumes()()
    shinyFiles::shinyDirChoose(input, "folder", roots = volumes, session = session)
    folder_path <- shiny::reactiveVal("")

    shiny::observe({
      shiny::req(input$folder)
      folder_path(shinyFiles::parseDirPath(volumes, input$folder))
    })

    folder_path
  })
}

input_sessions_files_server <- function(id, folder_path) {
  shiny::moduleServer(id, function(input, output, session) {
    files <- list_files(folder_path)

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

input_epochs_files_server <- function(id, folder_path) {
  shiny::moduleServer(id, function(input, output, session) {
    files <- list_files(folder_path)

    shiny::observe({
      shiny::updateSelectInput(session, "epochs_selector", choices = files(), selected = "")
    })

    selected_epochs <- shiny::reactiveVal(NULL)

    # Clear selected_epochs whenever folder_path changes
    shiny::observeEvent(folder_path(), {
      selected_epochs(NULL)
      shiny::updateSelectInput(session, "epochs_selector", selected = "")
    })

    shiny::observeEvent(input$epochs_selector, {
      selected_epochs(input$epochs_selector)
    })

    selected_epochs
  })
}

list_files <- function(folder_path) {
  shiny::reactive({
    shiny::req(folder_path())
    if (dir.exists(folder_path())) {
      filenames <- list.files(folder_path(), full.names = FALSE, pattern = "\\.csv$")
      display_names <- gsub("\\.csv$", "", filenames)
      stats::setNames(filenames, display_names)
    } else {
      character(0)
    }
  })
}

load_sessions_module_server <- function(id, folder_path, selected_sessions) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::reactive({
      shiny::req(folder_path(), selected_sessions())
      load_sessions(file.path(folder_path(), selected_sessions()))
    })
  })
}

load_epochs_module_server <- function(id, folder_path, selected_epochs) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::reactive({
      shiny::req(folder_path(), selected_epochs())
      load_epochs(file.path(folder_path(), selected_epochs()))
    })
  })
}
