source("../R/data_tables.R")

summary_module_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::tableOutput(ns("summary_table"))
  )
}

summary_server <- function(id, sessions) {
  shiny::moduleServer(id, function(input, output, session) {
    summary_table <- shiny::reactive({
      shiny::req(sessions())
      get_sessions_summary(sessions())
    })

    output$summary_table <- shiny::renderTable({
      shiny::req(summary_table())
      summary_table()
    })
  })
}
