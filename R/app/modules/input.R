source("../load_data.R")

input_folder_module <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::fluidRow(
      shiny::column(4, shinyFiles::shinyDirButton(ns("folder"),
                                                  "Select Folder",
                                                  "Please select the folder containing the data files")),
      shiny::column(8, shiny::verbatimTextOutput(ns("active_folder")))
    )
  )
}

input_folder_server <- function(id, session) {
  shiny::moduleServer(id, function(input, output, session) {
    volumes <- shinyFiles::getVolumes()()
    shinyFiles::shinyDirChoose(input, "folder", roots = volumes, session = session)
    folder_path <- shiny::reactiveVal("Select data folder")

    shiny::observe({
      shiny::req(input$folder)
      folder_path(shinyFiles::parseDirPath(volumes, input$folder))
    })

    output$active_folder <- shiny::renderText({
      folder_path()
    })

    return(folder_path)
  })
}


input_data_files_module <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::fluidRow(
      shiny::column(12, shiny::selectInput(ns("file_selector"),
        "Select date range:",
        choices = NULL,
        selected = NULL
      ))
    )
  )
}

input_data_files_server <- function(id, folder_path) {
  shiny::moduleServer(id, function(input, output, session) {
    files <- shiny::reactive({
      shiny::req(folder_path())
      if (dir.exists(folder_path())) {
        all_files <- list.files(folder_path(), full.names = FALSE)
        filtered_files <- all_files[grepl("_epoch_data\\.csv$", all_files)]
        display_names <- sub("_epoch_data\\.csv$", "", filtered_files)
        stats::setNames(display_names, display_names)
      } else {
        character(0)
      }
    })

    shiny::observe({
      shiny::updateSelectInput(session, "file_selector", choices = files())
    })

    selected_file <- shiny::reactive({
      shiny::req(input$file_selector)
      input$file_selector
    })

    return(selected_file)
  })
}


load_data_module_server <- function(id, folder_path, selected_file) {
  shiny::moduleServer(id, function(input, output, session) {
    data <- shiny::reactive({
      shiny::req(folder_path(), selected_file())
      sessions_path <- paste0(folder_path(), "/", selected_file(), "_sessions_reports.csv")
      epochs_path <- paste0(folder_path(), "/", selected_file(), "_epoch_data.csv")
      if (file.exists(sessions_path) && file.exists(epochs_path)) {
        load_data(folder_path(), selected_file())
      } else {
        NULL
      }
    })
  })
}