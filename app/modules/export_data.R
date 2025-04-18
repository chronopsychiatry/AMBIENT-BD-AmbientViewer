export_data_module <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::downloadButton(
      outputId = ns("download_sessions"),
      label = "Sessions"
    ),
    shiny::downloadButton(
      outputId = ns("download_epochs"),
      label = "Epochs"
    )
  )
}

export_data_server <- function(id, sessions, epochs) {
  shiny::moduleServer(id, function(input, output, session) {

    output$download_sessions <- get_table_download_handler(
      session = session,
      output_table = sessions,
      output_name = "sessions"
    )

    shiny::observe({
      shiny::req(sessions(), epochs())
      output$download_epochs <- get_table_download_handler(
        session = session,
        output_table = epochs,
        output_name = "epochs"
      )
    })
  })
}
