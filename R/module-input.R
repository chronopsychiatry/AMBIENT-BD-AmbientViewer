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
      load_sessions(input$sessions_file$datapath)
    })

    epochs <- shiny::reactive({
      shiny::req(input$epochs_file)
      logging::loginfo(paste0("Loading epochs file: ", input$epochs_file$name))
      epochs <- load_epochs(input$epochs_file$datapath)
    })

    list(sessions = sessions, epochs = epochs)
  })
}
