input_sessions_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h5("Sessions"),
    shinyWidgets::radioGroupButtons(
      inputId = ns("session_input_type"),
      label = NULL,
      choices = c("Single file upload", "Batch upload", "Build from Epoch data"),
      direction = "vertical",
      status = "outline-secondary",
      width = "100%"
    ),
    shiny::conditionalPanel(
      condition = paste0("input['", ns("session_input_type"), "'] == 'Single file upload'"),
      shiny::fileInput(
        inputId = ns("sessions_file"),
        label = NULL,
        accept = c(".csv", ".xls", ".xlsx", ".edf", ".rec")
      ),
    ),
    shiny::conditionalPanel(
      condition = paste0("input['", ns("session_input_type"), "'] == 'Batch upload'"),
      shiny::textInput(
        inputId = ns("batch_file_pattern"),
        label = "Filename pattern:",
        value = ""
      ),
      shinyFiles::shinyDirButton(ns("folder_select"), "Choose folder", "Please select the folder containing the session files"),
      shiny::actionButton(ns("load_sessions_batch"), "Load Batch Session Data")
    ),
    shiny::conditionalPanel(
      condition = paste0("input['", ns("session_input_type"), "'] == 'Build from Epoch data'"),
      shiny::actionButton(ns("build_sessions_from_epochs"), "Build Sessions from Epoch Data")
    ),
  )
}

input_sessions_server <- function(id, common) {
  shiny::moduleServer(id, function(input, output, session) {

    # Single file upload ----
    shiny::observeEvent(input$sessions_file, {
      shiny::req(input$sessions_file)
      common$logger |> write_log(paste0("Loading session file: ", input$sessions_file$name), type = "starting")
      data <- load_sessions(input$sessions_file$datapath)
      init_sessions(data, common)
    })

    # Batch file upload ----
    volumes <- shinyFiles::getVolumes()()
    shinyFiles::shinyDirChoose(input, "folder_select", roots = volumes, session = session)

    shiny::observeEvent(input$load_sessions_batch, {
      folder_path <- shinyFiles::parseDirPath(roots = volumes, input$folder_select)
      if (length(folder_path) == 0) return()
      common$logger |> write_log(paste0("Batch-loading session files from: ", folder_path), type = "starting")
      data <- load_sessions_batch(folder_path, input$batch_file_pattern)
      init_sessions(data, common)
    })
  })
}

init_sessions <- function(sessions, common) {
  col <- get_session_colnames(sessions)
  res <- clean_sessions(sessions, col_names = col)
  sessions <- res$sessions
  col <- res$col
  sessions$annotation <- ""
  common$sessions(sessions)
  common$sessions_colnames(col)
  common$session_filters(data.frame(no_sleep = rep(TRUE, nrow(sessions))))
  common$annotations(data.frame(
    id = sessions[[col$id]],

    annotation = "",
    stringsAsFactors = FALSE
  ))
}
