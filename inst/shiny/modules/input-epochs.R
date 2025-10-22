input_epochs_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h5("Epochs"),
    shinyWidgets::radioGroupButtons(
      inputId = ns("epoch_input_type"),
      label = NULL,
      choices = c("Single file upload", "Batch upload"),
      direction = "vertical",
      status = "outline-secondary",
      width = "100%"
    ),
    shiny::conditionalPanel(
      condition = paste0("input['", ns("epoch_input_type"), "'] == 'Single file upload'"),
      shiny::fileInput(
        inputId = ns("epochs_file"),
        label = NULL,
        accept = c(".csv", ".xls", ".xlsx", ".edf", ".rec")
      )
    ),
    shiny::conditionalPanel(
      condition = paste0("input['", ns("epoch_input_type"), "'] == 'Batch upload'"),
      shiny::textInput(
        inputId = ns("batch_file_pattern"),
        label = "Filename pattern:",
        value = ""
      ),
      shinyFiles::shinyDirButton(ns("folder_select"), "Choose folder", "Please select the folder containing the epoch files"),
      shiny::actionButton(ns("load_epochs_batch"), "Load Batch Epoch Data")
    )
  )
}

input_epochs_server <- function(id, common) {
  shiny::moduleServer(id, function(input, output, session) {

    # Single file upload ----
    shiny::observeEvent(input$epochs_file, {
      shiny::req(input$epochs_file)
      common$logger |> write_log(paste0("Loading epoch file: ", input$epochs_file$name), type = "starting")
      data <- load_epochs(input$epochs_file$datapath)
      if (is.null(data)) {
        common$logger |> write_log(paste0("No epoch data found in file: ", input$epochs_file$name), type = "error")
        return()
      }
      init_epochs(data, common)
    })

    # Batch file upload ----
    volumes <- shinyFiles::getVolumes()()
    shinyFiles::shinyDirChoose(input, "folder_select", roots = volumes, session = session)

    shiny::observeEvent(input$load_epochs_batch, {
      folder_path <- shinyFiles::parseDirPath(roots = volumes, input$folder_select)
      if (length(folder_path) == 0) return()
      common$logger |> write_log(paste0("Batch-loading epoch files from: ", folder_path), type = "starting")
      data <- load_batch(folder_path, input$batch_file_pattern, type = "epochs")
      init_epochs(data, common)
    })

  })
}

init_epochs <- function(epochs, common) {
  epochs$annotation <- ""
  common$epochs(epochs)
  common$epoch_filters(data.frame(from_sessions = rep(TRUE, nrow(epochs))))
}
