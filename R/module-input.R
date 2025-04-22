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
      load_sessions(input$sessions_file$datapath)
    })

    epochs <- shiny::reactive({
      shiny::req(input$epochs_file)
      load_epochs(input$epochs_file$datapath)
    })

    list(sessions = sessions, epochs = epochs)
  })
}
