sleep_clock_module_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::plotOutput(ns("sleep_clock_plot")),
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

sleep_clock_module_server <- function(id, sessions) {
  shiny::moduleServer(id, function(input, output, session) {

    sleep_clock_plot <- shiny::reactive({
      shiny::req(sessions())
      plot_sleep_clock(sessions = sessions())
    })

    output$sleep_clock_plot <- shiny::renderPlot({
      shiny::req(sleep_clock_plot())
      sleep_clock_plot()
    })

    output$download_plot <- get_plot_download_handler(
      session = session,
      output_plot = sleep_clock_plot,
      format = shiny::reactive(input$download_format),
      width = 7,
      height = 7
    )

  })
}
