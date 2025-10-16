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

summary_server <- function(id, common) {
  shiny::moduleServer(id, function(input, output, session) {

    sessions_summary_table <- shiny::reactive({
      shiny::req(common$sessions(), common$session_filters())
      get_sessions_summary(
        apply_filters(common$sessions(), common$session_filters()),
        col_names = common$sessions_colnames()
      )
    })

    epochs_summary_table <- shiny::reactive({
      shiny::req(common$epochs(), common$epoch_filters())
      get_epochs_summary(
        apply_filters(common$epochs(), common$epoch_filters()),
        col_names = common$epochs_colnames()
      )
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
