sleep_bubbles_module_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::plotOutput(ns("sleep_bubbles_plot")),
    shiny::downloadButton(
      outputId = ns("download_plot"),
      label = NULL
    ),
    shiny::radioButtons(
      inputId = ns("download_format"),
      label = NULL,
      choices = c("PNG" = "png", "SVG" = "svg"),
      inline = TRUE
    )
  )
}

sleep_bubbles_module_server <- function(id, sessions) {
  shiny::moduleServer(id, function(input, output, session) {

    # Reactive expression to store the plot
    sleep_bubbles_plot <- shiny::reactive({
      shiny::req(sessions())
      plot_sleep_bubbles(sessions = sessions())
    })

    # Render the plot
    output$sleep_bubbles_plot <- shiny::renderPlot({
      shiny::req(sleep_bubbles_plot())
      sleep_bubbles_plot()
    })

    output$download_plot <- get_plot_download_handler(
      session = session,
      output_plot = sleep_bubbles_plot,
      format = shiny::reactive(input$download_format)
    )

  })
}