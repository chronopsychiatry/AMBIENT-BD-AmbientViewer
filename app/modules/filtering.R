filtering_module <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::sliderInput(
      inputId = ns("date_range"),
      label = "Date Range:",
      min = Sys.Date(),
      max = Sys.Date(),
      value = c(Sys.Date(), Sys.Date()),
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
        # Handle empty dataframe: set placeholder values
        shiny::updateSliderInput(
          session,
          inputId = "date_range",
          min = Sys.Date(),
          max = Sys.Date(),
          value = c(Sys.Date(), Sys.Date())
        )
        return()
      }

      min_date <- min(sessions()$night, na.rm = TRUE)
      max_date <- max(sessions()$night, na.rm = TRUE)

      # Update the slider with the new date range
      shiny::updateSliderInput(
        session,
        inputId = "date_range",
        min = min_date,
        max = max_date,
        value = c(min_date, max_date)
      )
    })

    filtered_sessions <- shiny::reactive({
      shiny::req(sessions())

      # Convert time range to "HH:MM" format
      from_time <- sprintf("%02d:00", as.numeric(input$time_range[1]) %% 24)
      to_time <- sprintf("%02d:00", as.numeric(input$time_range[2]) %% 24)

      # Apply filters
      filtered <- remove_sessions_no_sleep(sessions())
      filtered <- set_min_time_in_bed(filtered, input$min_time_in_bed)
      filtered <- set_session_sleep_onset_range(filtered, from_time, to_time)

      # Filter by date range
      if (!is.null(input$date_range)) {
        filtered <- filtered %>%
          dplyr::filter(night >= as.Date(input$date_range[1]) &
                          night <= as.Date(input$date_range[2]))
      }

      return(filtered)
    })

    return(filtered_sessions)
  })
}
