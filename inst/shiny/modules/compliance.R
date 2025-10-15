compliance_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::uiOutput(ns("compliance_text")),
    shiny::tableOutput(ns("compliance_table")),
    shiny::downloadButton(
      outputId = ns("download_compliance"),
      label = NULL,
      class = "small-btn"
    )
  )
}

compliance_server <- function(id, common) {
  shiny::moduleServer(id, function(input, output, session) {
    compliance_table <- shiny::reactive({
      shiny::req(common$sessions())
      if (is.null(common$sessions_colnames()$night)) {
        shiny::HTML("'Night' column was not specified.<br/>Please set a column name for night.")
      }
      shiny::validate(shiny::need(!is.null(common$sessions_colnames()$night), message = FALSE))
      get_compliance_table(common$sessions(), common$sessions_colnames())
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
        shiny::HTML("<p>The compliance table below lists nights where multiple sessions have been recorded.</p>")
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
      shiny::req(common$sessions())
      shiny::validate(
        shiny::need(!is.null(common$sessions_colnames()$night), message = FALSE)
      )
      output_table <- common$sessions() |>
        dplyr::filter(.data$display) |>
        get_non_complying_sessions(col_names = common$sessions_colnames())
      output$download_compliance <- get_table_download_handler(
        session = session,
        common = common,
        output_table = output_table,
        output_name = "compliance"
      )
    })
  })
}

#' @importFrom rlang .data
get_compliance_table <- function(common, col_names = NULL) {
  common$sessions |>
    apply_filters(common$session_filters()) |>
    get_non_complying_sessions(col_names = col_names) |>
    make_sessions_display_table(col_names = col_names)
}

make_sessions_display_table <- function(sessions, col_names = NULL) {
  col <- col_names
  sessions <- sessions |>
    dplyr::mutate(
      start = parse_time(get_col(sessions, col$session_start)) |> format("%H:%M"),
      sleep_onset = parse_time(get_col(sessions, col$time_at_sleep_onset)) |> format("%H:%M"),
      wakeup = parse_time(get_col(sessions, col$time_at_wakeup)) |> format("%H:%M"),
      end = parse_time(get_col(sessions, col$session_end)) |> format("%H:%M"),
      session_duration_h = difftime(parse_time(get_col(sessions, col$session_end)),
                                    parse_time(get_col(sessions, col$session_start)),
                                    units = "hours"),
      night = if (!is.null(col$night)) {
        lubridate::as_date(get_col(sessions, col$night)) |> format("%Y-%m-%d")
      } else {
        NA
      },
      time_in_bed_h = if (!is.null(col$time_in_bed)) get_col(sessions, col$time_in_bed) / 60 / 60 else NA
    ) |>
    dplyr::select(
      dplyr::any_of(c(
        col$id,
        col$subject_id,
        "night",
        "start",
        "sleep_onset",
        "wakeup",
        "end",
        "session_duration_h",
        "time_in_bed_h",
        "annotation"
      ))
    )
}
