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

    shiny::reactive({
      shiny::req(sessions())

      from_time <- sprintf("%02d:00", as.numeric(input$time_range[1]) %% 24)
      to_time <- sprintf("%02d:00", as.numeric(input$time_range[2]) %% 24)

      if (as.Date(input$date_range[1], format = "%Y-%m-%d") != Sys.Date() + 1) {
        logging::loginfo(paste0("Filtering:",
                                " date_range: ", input$date_range[1], "-", input$date_range[2],
                                " sleep_onset_range: ", from_time, "-", to_time,
                                " min_time_in_bed: ", input$min_time_in_bed))
      }

      sessions() |>
        remove_sessions_no_sleep() |>
        set_min_time_in_bed(input$min_time_in_bed) |>
        set_session_sleep_onset_range(from_time, to_time) |>
        filter_by_night_range(input$date_range[1], input$date_range[2])
    })
  })
}
