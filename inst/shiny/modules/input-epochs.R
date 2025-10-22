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
    })

  })
}

init_epochs <- function(epochs, common) {
  epochs$annotation <- ""
  common$epochs(epochs)
  common$epoch_filters(data.frame(from_sessions = rep(TRUE, nrow(epochs))))
}
