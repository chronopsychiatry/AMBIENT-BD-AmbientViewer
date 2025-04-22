compliance_module <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::uiOutput(ns("compliance_text")),
    shiny::tableOutput(ns("compliance_table")),
    shiny::downloadButton(
      outputId = ns("download_compliance"),
      label = ""
    )
  )
}

compliance_server <- function(id, filtered_sessions) {
  shiny::moduleServer(id, function(input, output, session) {
    compliance_table <- shiny::reactive({
      shiny::req(filtered_sessions())
      make_compliance_table(filtered_sessions())
    })

    output$compliance_table <- shiny::renderTable({
      shiny::req(compliance_table())
      shiny::validate(
        shiny::need(nrow(compliance_table()) > 0, "There are no duplicate sessions on the same night.")
      )
      compliance_table()
    })

    output$compliance_text <- shiny::renderUI({
      shiny::req(compliance_table())
      if (nrow(compliance_table()) > 0) {
        shiny::HTML("<br/><p>The compliance table below lists nights where multiple sessions have been recorded.</p>")
      }
    })

    shiny::observe({
      shiny::req(compliance_table())
      if (nrow(compliance_table()) > 0) {
        shinyjs::runjs("$('#main_tabs_tables li a[data-value=\"compliance_tab\"]').css('color', 'red');")
      } else {
        shinyjs::runjs("$('#main_tabs_tables li a[data-value=\"compliance_tab\"]').css('color', '');")
      }
    })

    output$download_compliance <- get_table_download_handler(
      session = session,
      output_table = compliance_table,
      output_name = "compliance"
    )

  })
}

#' @importFrom rlang .data
make_compliance_table <- function(sessions) {
  get_non_complying_sessions(sessions) |>
    dplyr::mutate(
      start_time = substr(.data$session_start, 12, 16),
      sleep_onset = substr(.data$time_at_sleep_onset, 12, 16),
      wakeup_time = substr(.data$time_at_wakeup, 12, 16),
      end_time = substr(.data$session_end, 12, 16),
      session_duration_h = difftime(lubridate::ymd_hms(.data$session_end),
                                    lubridate::ymd_hms(.data$session_start),
                                    units = "hours"),
      night = format(.data$night, "%Y-%m-%d"),
      time_in_bed_h = .data$time_in_bed / 60 / 60
    ) |>
    dplyr::select("id", "night", "start_time", "sleep_onset", "wakeup_time", "end_time", "session_duration_h", "time_in_bed_h")
}
