summary_module_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h5(shiny::strong("Sessions")),
    shiny::uiOutput(ns("sessions_summary_table")),
    shiny::h5(shiny::strong("Epochs")),
    shiny::uiOutput(ns("epochs_summary_table"))
  )
}

summary_server <- function(id, sessions, epochs) {
  shiny::moduleServer(id, function(input, output, session) {

    sessions_summary_table <- shiny::reactive({
      shiny::req(sessions())
      get_sessions_summary(sessions())
    })

    epochs_summary_table <- shiny::reactive({
      shiny::req(epochs())
      get_epochs_summary(epochs())
    })

    output$sessions_summary_table <- shiny::renderTable({
      shiny::req(sessions_summary_table())
      sessions_summary_table()
    })

    output$epochs_summary_table <- shiny::renderTable({
      shiny::req(epochs_summary_table())
      epochs_summary_table()
    })
  })
}
