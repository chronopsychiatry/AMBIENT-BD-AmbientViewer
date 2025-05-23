timeseries_module_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::fluidRow(
      shiny::column(4,
        shiny::selectInput(
          inputId = ns("variable"),
          label = "Select Variable:",
          choices = NULL
        )
      ),
      shiny::column(4,
        shiny::selectInput(
          inputId = ns("colorby"),
          label = "Colour by:",
          choices = NULL
        )
      )
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

timeseries_module_server <- function(id, epochs, epochs_colnames) {
  shiny::moduleServer(id, function(input, output, session) {

    plot_options <- shiny::reactiveValues(variable = NULL, colorby = NULL)
    update_variable_dropdown(epochs, epochs_colnames, plot_options, input, session)
    update_colorby_dropdown(epochs, epochs_colnames, plot_options, input, session)

    timeseries_plot <- shiny::reactive({
      shiny::req(input$variable, epochs())
      epochs <- epochs()[epochs()$display, ]
      col <- epochs_colnames()
      shiny::validate(
        shiny::need(!is.null(col$timestamp), "'timestamp' column was not specified."),
        shiny::need(!is.null(col$night), "'night' column was not specified.")
      )
      plot_timeseries(
        epochs = epochs,
        variable = input$variable,
        color_by = input$colorby,
        exclude_zero = input$exclude_zero,
        col_names = epochs_colnames()
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
