sleep_distributions_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::fluidRow(
      shiny::column(4,
        shiny::selectInput(
          inputId = ns("plot_type"),
          label = "Select Plot Type:",
          choices = c("Boxplot", "Histogram", "Density")
        )
      ),
      shiny::column(4,
        shiny::uiOutput(ns("binwidth_slider")),
        shiny::uiOutput(ns("adjust_slider"))
      )
    ),
    shiny::plotOutput(ns("sleep_distribution_plot")),
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

sleep_distributions_server <- function(id, sessions, sessions_colnames) {
  shiny::moduleServer(id, function(input, output, session) {

    output$binwidth_slider <- shiny::renderUI({
      shiny::req(sessions())
      col <- sessions_colnames()
      if (input$plot_type == "Histogram") {
        shiny::sliderInput(
          session$ns("binwidth"),
          "Binwidth:",
          min = 0.1,
          max = 2,
          value = 0.25,
          step = 0.05,
          ticks = FALSE
        )
      }
    })

    output$adjust_slider <- shiny::renderUI({
      shiny::req(sessions())
      col <- sessions_colnames()
      if (input$plot_type == "Density") {
        shiny::sliderInput(
          session$ns("adjust"),
          "Adjust:",
          min = 0.2,
          max = 3,
          value = 1,
          step = 0.2,
          ticks = FALSE
        )
      }
    })

    sleep_distribution_plot <- shiny::reactive({
      shiny::req(input$plot_type, sessions())
      sessions <- sessions()[sessions()$display, ]
      col <- sessions_colnames()
      shiny::validate(
        shiny::need(!is.null(col$time_at_sleep_onset), "'time_at_sleep_onset' column was not specified."),
        shiny::need(!is.null(col$time_at_midsleep), "'time_at_midsleep' column was not specified."),
        shiny::need(!is.null(col$time_at_wakeup), "'time_at_wakeup' column was not specified."),
        shiny::need(nrow(sessions) > 0, "")
      )
      switch(input$plot_type,
        "Boxplot" = {
          sleeptimes_boxplot(sessions, sessions_colnames())
        },
        "Histogram" = {
          shiny::req(input$binwidth)
          sleeptimes_histogram(sessions, sessions_colnames(), binwidth = input$binwidth)
        },
        "Density" = {
          shiny::req(input$adjust)
          sleeptimes_density(sessions, sessions_colnames(), adjust = input$adjust)
        }
      )
    })

    output$sleep_distribution_plot <- shiny::renderPlot({
      shiny::req(sleep_distribution_plot())
      sleep_distribution_plot()
    })

    output$download_plot <- get_plot_download_handler(
      session = session,
      output_plot = sleep_distribution_plot,
      format = shiny::reactive(input$download_format),
      width = 12,
      height = 6
    )
  })
}
