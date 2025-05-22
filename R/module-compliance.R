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

compliance_server <- function(id, sessions, sessions_colnames) {
  shiny::moduleServer(id, function(input, output, session) {
    compliance_table <- shiny::reactive({
      shiny::req(sessions())
      get_compliance_table(sessions(), sessions_colnames())
    })

    output$compliance_table <- shiny::renderTable({
      shiny::req(compliance_table())
      if (nrow(compliance_table()) == 0) {
        logging::loginfo("Compliance table is empty (there are no duplicate sessions on the same night).")
      }
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

    shiny::observe({
      shiny::req(sessions())
      output_table <- sessions() |>
        dplyr::filter(.data$display) |>
        get_non_complying_sessions(col_names = sessions_colnames())
      output$download_compliance <- get_table_download_handler(
        session = session,
        output_table = output_table,
        output_name = "compliance"
      )
    })
  })
}

#' @importFrom rlang .data
get_compliance_table <- function(sessions, col_names = NULL) {
  sessions |>
    dplyr::filter(.data$display) |>
    get_non_complying_sessions(col_names = col_names) |>
    make_sessions_display_table(col_names = col_names)
}

#' @importFrom rlang .data
make_sessions_display_table <- function(sessions, col_names = NULL) {
  col <- get_session_colnames(sessions, col_names)
  sessions <- sessions |>
    dplyr::mutate(
      start_time = parse_time(.data[[col$session_start]]) |> format("%H:%M"),
      sleep_onset = parse_time(.data[[col$time_at_sleep_onset]]) |> format("%H:%M"),
      wakeup_time = parse_time(.data[[col$time_at_wakeup]]) |> format("%H:%M"),
      end_time = parse_time(.data[[col$session_end]]) |> format("%H:%M"),
      session_duration_h = difftime(parse_time(.data[[col$session_end]]),
                                    parse_time(.data[[col$session_start]]),
                                    units = "hours"),
      night = lubridate::as_date(.data[[col$night]]) |> format("%Y-%m-%d"),
      time_in_bed_h = if (!is.null(col$time_in_bed)) .data[[col$time_in_bed]] / 60 / 60 else NA
    ) |>
    dplyr::select(
      col$id,
      "night",
      "start_time",
      "sleep_onset",
      "wakeup_time",
      "end_time",
      "session_duration_h",
      "time_in_bed_h",
      "annotation"
    )
}
