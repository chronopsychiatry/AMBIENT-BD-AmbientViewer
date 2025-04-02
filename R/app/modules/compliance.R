library(tidyverse)

source("../compliance.R")

compliance_module <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::tableOutput(ns("compliance_table"))
  )
}

compliance_server <- function(id, filtered_sessions) {
  shiny::moduleServer(id, function(input, output, session) {
    # Reactive to generate the compliance table
    compliance_table <- shiny::reactive({
      shiny::req(filtered_sessions())
      make_compliance_table(filtered_sessions())
    })

    # Render the compliance table
    output$compliance_table <- shiny::renderTable({
      shiny::req(compliance_table())
      shiny::validate(
        shiny::need(nrow(compliance_table()) > 0, "There are no duplicate sessions on the same night.")
      )
      compliance_table()
    })

    # Observe changes to the compliance table and update the tab title color
    shiny::observe({
      shiny::req(compliance_table())
      if (nrow(compliance_table()) > 0) {
        shinyjs::runjs("$('#main_tabs li a[data-value=\"compliance_tab\"]').css('color', 'red');")
      } else {
        shinyjs::runjs("$('#main_tabs li a[data-value=\"compliance_tab\"]').css('color', '');")
      }
    })
  })
}

make_compliance_table <- function(sessions) {
  compliance_table <- get_non_complying_sessions(sessions) %>%
    dplyr::mutate(
      start_time = substr(session_start, 12, 16),
      end_time = substr(session_end, 12, 16),
      session_duration = difftime(lubridate::ymd_hms(session_end),
                                  lubridate::ymd_hms(session_start),
                                  units = "hours"),
      night = format(night, "%Y-%m-%d")
    ) %>%
    dplyr::select(id, night, start_time, end_time, session_duration)
  return(compliance_table)
}
