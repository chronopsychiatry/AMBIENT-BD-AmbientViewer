source("../R/timeseries_plots.R")

timeseries_module_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::selectInput(
      inputId = ns("variable"),
      label = "Select Variable:",
      choices = NULL
    ),
    shiny::checkboxInput(
      inputId = ns("exclude_zero"),
      label = "Exclude Zero Values",
      value = FALSE
    ),
    shiny::sliderInput(
      inputId = ns("date_range"),
      label = "Select Date Range:",
      min = Sys.Date(),
      max = Sys.Date(),
      value = c(Sys.Date(), Sys.Date()),
      timeFormat = "%Y-%m-%d"
    ),
    shiny::plotOutput(ns("timeseries_plot"))
  )
}

timeseries_module_server <- function(id, epochs, sessions) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    plot_options <- shiny::reactiveValues(variable = NULL, date_range = NULL)

    # Filter epochs based on sessions
    filtered_epochs <- shiny::reactive({
      shiny::req(epochs(), sessions())
      filter_epochs_from_sessions(epochs(), sessions())
    })

    # Populate the variable dropdown dynamically
    shiny::observe({
      shiny::req(filtered_epochs())
      if (nrow(filtered_epochs()) == 0) {
        # Handle empty dataframe: set placeholder values
        updateSelectInput(
          session,
          inputId = "variable",
          choices = NULL,
          selected = NULL
        )
        plot_options$variable <- NULL
        return()
      }

      excluded_vars <- c("timestamp", "motion_data_count", "hour", "date",
      "night", "adjusted_time", "session_id", "sleep_stage", "epoch_duration")
      available_vars <- setdiff(names(filtered_epochs()), excluded_vars)

      # Update the dropdown, but preserve the selected variable if possible
      current_variable <- plot_options$variable
      if (!is.null(current_variable) && current_variable %in% available_vars) {
        selected_variable <- current_variable
      } else {
        selected_variable <- available_vars[1]
      }
      plot_options$variable <- selected_variable

      updateSelectInput(
        session,
        inputId = "variable",
        choices = available_vars,
        selected = selected_variable
      )
    })

    # Set the date range slider dynamically
    shiny::observe({
      shiny::req(filtered_epochs())
      if (nrow(filtered_epochs()) == 0) {
        # Handle empty dataframe: set placeholder values
        updateSliderInput(
          session,
          inputId = "date_range",
          min = Sys.Date(),
          max = Sys.Date(),
          value = c(Sys.Date(), Sys.Date())
        )
        plot_options$date_range <- NULL
        return()
      }


      min_date <- min(filtered_epochs()$night, na.rm = TRUE)
      max_date <- max(filtered_epochs()$night, na.rm = TRUE)

      # Preserve the selected date range if possible
      current_date_range <- plot_options$date_range
      if (!is.null(current_date_range) &&
            current_date_range[1] >= min_date &&
            current_date_range[2] <= max_date) {
        selected_date_range <- current_date_range
      } else {
        selected_date_range <- c(min_date, max_date)
      }
      plot_options$date_range <- selected_date_range

      updateSliderInput(
        session,
        inputId = "date_range",
        min = min_date,
        max = max_date,
        value = selected_date_range
      )
    })

    # Update the stored plot options when the user changes them
    shiny::observe({
      plot_options$variable <- input$variable
      plot_options$date_range <- input$date_range
    })

    # Render the timeseries plot
    output$timeseries_plot <- shiny::renderPlot({
      shiny::req(input$variable, input$date_range, filtered_epochs())
      plot_timeseries(
        epochs = filtered_epochs(),
        variable = input$variable,
        start_date = input$date_range[1],
        end_date = input$date_range[2],
        exclude_zero = input$exclude_zero
      )
    })
  })
}