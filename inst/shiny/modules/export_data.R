export_data_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h4("Raw Data"),
    shiny::downloadButton(
      outputId = ns("download_sessions"),
      label = "Sessions"
    ),
    shiny::downloadButton(
      outputId = ns("download_epochs"),
      label = "Epochs"
    ),
    shiny::br(),
    shiny::br(),
    shiny::h4("Subject Report"),
    shiny::p("(Somnofy data only)"),
    shiny::textInput(
      inputId = ns("title"),
      label = "Report Title",
      value = ""
    ),
    shiny::downloadButton(
      outputId = ns("download_report"),
      label = "Subject Report"
    )
  )
}

export_data_server <- function(id, sessions, epochs) {
  shiny::moduleServer(id, function(input, output, session) {

    shiny::observe({
      shiny::req(sessions())
      sessions <- sessions()[sessions()$display, ]
      output$download_sessions <- get_table_download_handler(
        session = session,
        output_table = sessions,
        output_name = "sessions"
      )
    })

    shiny::observe({
      shiny::req(epochs())
      epochs <- epochs()[epochs()$display, ]
      output$download_epochs <- get_table_download_handler(
        session = session,
        output_table = epochs,
        output_name = "epochs"
      )
    })

    shiny::observe({
      shiny::req(sessions())
      sessions <- sessions()[sessions()$display, ]
      output$download_report <- get_report_download_handler(
        session = session,
        sessions = sessions,
        title = shiny::reactive(input$title)
      )
    })

  })
}
