summary_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h5(shiny::strong("Sessions")),
    shiny::div(
      style = "width: 800px;",
      shiny::tableOutput(ns("sessions_summary_table"))
    ),
    shiny::h5(shiny::strong("Epochs")),
    shiny::div(
      style = "width: 500px;",
      shiny::tableOutput(ns("epochs_summary_table"))
    )
  )
}

summary_server <- function(id, sessions, epochs, common) {
  shiny::moduleServer(id, function(input, output, session) {

    sessions_summary_table <- shiny::reactive({
      shiny::req(sessions())
      sessions <- sessions()[sessions()$display, ]
      get_sessions_summary(sessions, col_names = common$sessions_colnames())
    })

    epochs_summary_table <- shiny::reactive({
      shiny::req(epochs())
      epochs <- epochs()[epochs()$display, ]
      get_epochs_summary(epochs, col_names = common$epochs_colnames())
    })

    output$sessions_summary_table <- shiny::renderTable({
      shiny::req(sessions_summary_table())
      sessions_summary_table()
    })

    output$epochs_summary_table <- shiny::renderTable({
      shiny::req(epochs_summary_table())
      shiny::validate(
        shiny::need(epochs_summary_table()$total_sessions > 0, "None of the epochs match the selected sessions.")
      )
      epochs_summary_table()
    })
  })
}
