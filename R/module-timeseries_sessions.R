timeseries_sessions_module_ui <- function(id) {
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
    plotly::plotlyOutput(ns("timeseries_sessions_plot")),
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

    plot_options <- shiny::reactiveValues(variable = NULL, colorby = NULL)
    update_variable_dropdown(sessions, sessions_colnames, plot_options, input, session)
    update_colorby_dropdown(sessions, sessions_colnames, plot_options, input, session)

    timeseries_sessions_plot <- shiny::reactive({
      shiny::req(input$variable, sessions())
      sessions <- sessions()[sessions()$display, ]
      col <- sessions_colnames()
      shiny::validate(
        shiny::need(!is.null(col$night), "'night' column was not specified.")
      )
      plot_timeseries_sessions(
        sessions = sessions,
        variable = input$variable,
        color_by = input$colorby,
        exclude_zero = input$exclude_zero,
        col_names = sessions_colnames()
      )
    })

    output$timeseries_sessions_plot <- plotly::renderPlotly({
      shiny::req(timeseries_sessions_plot())
      plotly::ggplotly(timeseries_sessions_plot())
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
