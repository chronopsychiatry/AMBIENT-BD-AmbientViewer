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
    shiny::plotOutput(ns("timeseries_plot")),
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

timeseries_module_server <- function(id, epochs) {
  shiny::moduleServer(id, function(input, output, session) {

    plot_options <- shiny::reactiveValues(variable = NULL)

    # Populate the variable dropdown dynamically
    shiny::observe({
      shiny::req(epochs())
      if (nrow(epochs()) == 0) {
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

      excluded_vars <- c("timestamp", "motion_data_count",
                         "night", "session_id", "sleep_stage", "epoch_duration")
      available_vars <- setdiff(names(epochs()), excluded_vars)

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

    timeseries_plot <- shiny::reactive({
      shiny::req(input$variable, epochs())
      plot_timeseries(
        epochs = epochs(),
        variable = input$variable,
        exclude_zero = input$exclude_zero
      )
    })

    output$timeseries_plot <- shiny::renderPlot({
      shiny::req(timeseries_plot())
      timeseries_plot()
    })

    output$download_plot <- get_plot_download_handler(
      session = session,
      output_plot = timeseries_plot,
      format = shiny::reactive(input$download_format),
      width = 12,
      height = 6
    )

  })
}
