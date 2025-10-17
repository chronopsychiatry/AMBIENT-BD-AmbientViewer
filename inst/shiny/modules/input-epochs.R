input_epochs_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h5("Epochs"),
    shiny::fileInput(
      ns("epochs_file"),
      NULL,
      accept = c(".csv")
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
      init_epochs(data, common)
      if (data$.data_type[1] == "somnofy_v1") {
        data$session_id <- stringr::str_extract(input$epochs_file$name, "^[^.]+")
      }
    })

  })
}

init_epochs <- function(epochs, common) {
  col <- get_epoch_colnames(epochs)
  epochs$annotation <- ""
  common$epochs(epochs)
  common$epochs_colnames(col)
  common$epoch_filters(data.frame(from_sessions = rep(TRUE, length(epochs[[col$timestamp]]))))
}

check_epoch_datatype <- function(epochs, common) {
  if (epochs$.data_type[1] == "none") {
    common$logger |> write_log("Could not detect epoch data type. Please set column names.", type = "warning")
  } else {
    common$logger |> write_log(paste0("Detected epoch data type: ", epochs$.data_type[1]), type = "complete")
    common$logger |> write_log("Column names were set automatically", type = "info")
  }
}
