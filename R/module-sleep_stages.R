sleep_stages_module_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::plotOutput(ns("sleep_stages_plot")),
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

sleep_stages_module_server <- function(id, epochs, sessions) {
  shiny::moduleServer(id, function(input, output, session) {

    sleep_stages_plot <- shiny::reactive({
      shiny::req(epochs())
      plot_sleep_stages(epochs = epochs())
    })

    output$sleep_stages_plot <- shiny::renderPlot({
      shiny::req(sleep_stages_plot())
      sleep_stages_plot()
    })

    output$download_plot <- get_plot_download_handler(
      session = session,
      output_plot = sleep_stages_plot,
      format = shiny::reactive(input$download_format)
    )

  })
}
