filtering_module <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::sliderInput(
      inputId = ns("date_range"),
      label = "Date Range:",
      min = Sys.Date() + 1,
      max = Sys.Date() + 1,
      value = c(Sys.Date() + 1, Sys.Date() + 1),
      timeFormat = "%Y-%m-%d"
    ),
    shinyWidgets::sliderTextInput(
      inputId = ns("time_range"),
      label = "Sleep Onset Time:",
      choices = c(
                  "13", "14", "15", "16", "17",
                  "18", "19", "20", "21", "22", "23",
                  "00", "01", "02", "03", "04", "05",
                  "06", "07", "08", "09", "10", "11",
                  "12"),
      selected = c("13", "12"),
      grid = TRUE
    ),
    shiny::sliderInput(
      ns("min_time_in_bed"),
      "Minimum Time in Bed:",
      min = 0, max = 12, value = 0, step = 1, post = "h",
      ticks = FALSE
    )
  )
}

filtering_tab <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::uiOutput(ns("removed_sessions_text")),
    shiny::tableOutput(ns("removed_sessions")),
    shiny::downloadButton(
      outputId = ns("download_removed_sessions"),
      label = ""
    )
  )
}

filtering_server <- function(id, sessions) {
  shiny::moduleServer(id, function(input, output, session) {

    # Set the date range slider dynamically
    shiny::observe({
      shiny::req(sessions())
      if (nrow(sessions()) == 0) {
        shiny::updateSliderInput(
          session,
          inputId = "date_range",
          min = Sys.Date() + 1,
          max = Sys.Date() + 1,
          value = c(Sys.Date() + 1, Sys.Date() + 1)
        )
      }

      min_date <- min(sessions()$night, na.rm = TRUE)
      max_date <- max(sessions()$night, na.rm = TRUE)

      shiny::updateSliderInput(
        session,
        inputId = "date_range",
        min = min_date,
        max = max_date,
        value = c(min_date, max_date)
      )
    })

    selected_sessions <- shiny::reactive({
      shiny::req(sessions())
      sessions() |>
        filter_by_night_range(input$date_range[1], input$date_range[2])
    })

    filtered_sessions <- shiny::reactive({
      shiny::req(sessions())

      from_time <- sprintf("%02d:00", as.numeric(input$time_range[1]) %% 24)
      to_time <- sprintf("%02d:00", as.numeric(input$time_range[2]) %% 24)

      if (as.Date(input$date_range[1], format = "%Y-%m-%d") != Sys.Date() + 1) {
        logging::loginfo(paste0("Filtering:",
                                " date_range: ", input$date_range[1], "-", input$date_range[2],
                                " sleep_onset_range: ", from_time, "-", to_time,
                                " min_time_in_bed: ", input$min_time_in_bed))
      }

      selected_sessions() |>
        remove_sessions_no_sleep() |>
        set_min_time_in_bed(input$min_time_in_bed) |>
        set_session_sleep_onset_range(from_time, to_time)
    })

    removed_sessions <- shiny::reactive({
      shiny::req(selected_sessions(), filtered_sessions())
      get_removed_sessions_table(selected_sessions(), filtered_sessions())
    })

    output$removed_sessions <- shiny::renderTable({
      shiny::req(removed_sessions())
      shiny::validate(
        shiny::need(nrow(removed_sessions()) > 0,
                    paste0("No sessions have been removed between ", input$date_range[1], " and ", input$date_range[2], "."))
      )
      removed_sessions()
    })

    shiny::observe({
      shiny::req(removed_sessions(), filtered_sessions())
      logging::loginfo(paste0("Removed sessions: ", nrow(removed_sessions()), ". Remaining sessions: ", nrow(filtered_sessions())))
    })

    output$removed_sessions_text <- shiny::renderUI({
      shiny::req(removed_sessions())
      if (nrow(removed_sessions()) > 0) {
        shiny::HTML(paste0(
          "<br/><p>The filtering table below shows sessions that were removed by filtering.</p>",
          "<p>", nrow(removed_sessions()), " sessions have been removed between ", input$date_range[1], " and ", input$date_range[2], ".</p>"
        ))
      }
    })

    output$download_removed_sessions <- get_table_download_handler(
      session = session,
      output_table = shiny::reactive(get_removed_sessions(sessions(), filtered_sessions())),
      output_name = "removed_sessions"
    )

    filtered_sessions
  })
}

#' @importFrom rlang .data
get_removed_sessions_table <- function(sessions, filtered_sessions) {
  get_removed_sessions(sessions, filtered_sessions) |>
    make_sessions_display_table()
}
