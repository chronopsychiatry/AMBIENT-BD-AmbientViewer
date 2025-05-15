timeseries_sessions_module_ui <- function(id) {
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
    shiny::plotOutput(ns("timeseries_sessions_plot")),
    shiny::downloadButton(
      outputId = ns("download_plot"),
      label = NULL
    ),
    shiny::radioButtons(
      inputId = ns("download_format"),
      label = NULL,
      choices = c("PNG" = "png", "PDF" = "pdf", "SVG" = "svg"),
      inline = TRUE
    )
  )
}

timeseries_sessions_module_server <- function(id, sessions, sessions_colnames) {
  shiny::moduleServer(id, function(input, output, session) {

    plot_options <- shiny::reactiveValues(variable = NULL)

    # Populate the variable dropdown dynamically
    shiny::observe({
      shiny::req(sessions())
      df <- sessions()[sessions()$display, ]
      if (nrow(df) == 0) {
        # Handle empty dataframe: set placeholder values
        shiny::updateSelectInput(
          session,
          inputId = "variable",
          choices = NULL,
          selected = NULL
        )
        plot_options$variable <- NULL
        return()
      }

      col <- sessions_colnames()
      excluded_vars <- c(col$id, "state", col$subject_id, col$device_id, col$night, ".data_type")
      available_vars <- setdiff(names(df), excluded_vars)

      # Update the dropdown, but preserve the selected variable if possible
      current_variable <- plot_options$variable
      if (!is.null(current_variable) && current_variable %in% available_vars) {
        selected_variable <- current_variable
      } else {
        selected_variable <- available_vars[1]
      }
      plot_options$variable <- selected_variable

      shiny::updateSelectInput(
        session,
        inputId = "variable",
        choices = available_vars,
        selected = selected_variable
      )
    })

    # Update the stored plot options when the user changes them
    shiny::observe({
      plot_options$variable <- input$variable
    })

    timeseries_sessions_plot <- shiny::reactive({
      shiny::req(input$variable, sessions())
      sessions <- sessions()[sessions()$display, ]
      plot_timeseries_sessions(
        sessions = sessions,
        variable = input$variable,
        exclude_zero = input$exclude_zero,
        col_names = sessions_colnames()
      )
    })

    output$timeseries_sessions_plot <- shiny::renderPlot({
      shiny::req(timeseries_sessions_plot())
      timeseries_sessions_plot()
    })

    output$download_plot <- get_plot_download_handler(
      session = session,
      output_plot = timeseries_sessions_plot,
      format = shiny::reactive(input$download_format),
      width = 12,
      height = 6
    )

  })
}
