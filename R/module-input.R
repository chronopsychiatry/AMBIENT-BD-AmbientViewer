input_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::fileInput(
      ns("sessions_file"),
      "Sessions",
      accept = c(".csv")
    ),
    shiny::fileInput(
      ns("epochs_file"),
      "Epochs",
      accept = c(".csv")
    )
  )
}

input_server <- function(id, session) {
  shiny::moduleServer(id, function(input, output, session) {
    sessions <- shiny::reactive({
      shiny::req(input$sessions_file)
      logging::loginfo(paste0("Loading sessions file: ", input$sessions_file$name))
      column_check <- check_csv_column(input$sessions_file$datapath, "session_start",
                                       "Session file does not contain 'session_start' column. Please check the csv file contains session data.")
      if (!column_check) {
        return(NULL)
      }
      load_sessions(input$sessions_file$datapath)
    })

    epochs <- shiny::reactive({
      shiny::req(input$epochs_file)
      logging::loginfo(paste0("Loading epochs file: ", input$epochs_file$name))
      column_check <- check_csv_column(input$epochs_file$datapath, "timestamp",
                                       "Epoch file does not contain 'timestamp' column. Please check the csv file contains epoch data.")
      if (!column_check) {
        return(NULL)
      }
      load_epochs(input$epochs_file$datapath)
    })

    list(sessions = sessions, epochs = epochs)
  })
}

check_csv_column <- function(file_path, column_name, error_msg) {
  colnames <- file_path |>
    read.csv(nrows = 1) |>
    colnames()
  has_column <- column_name %in% colnames
  if (!has_column) {
    logging::logerror(error_msg)
    shiny::showNotification(error_msg, type = "error")
  }
  has_column
}
