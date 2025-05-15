sleep_spiral_module_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::plotOutput(ns("sleep_spiral_plot")),
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

sleep_spiral_module_server <- function(id, epochs, epochs_colnames) {
  shiny::moduleServer(id, function(input, output, session) {

    sleep_spiral_plot <- shiny::reactive({
      shiny::req(epochs())
      epochs <- epochs()[epochs()$display, ]
      if (nrow(epochs) == 0) {
        return(NULL)
      }
      plot_sleep_spiral(epochs = epochs, col_names = epochs_colnames())
    })

    output$sleep_spiral_plot <- shiny::renderPlot({
      shiny::req(sleep_spiral_plot())
      sleep_spiral_plot()
    })

    output$download_plot <- get_plot_download_handler(
      session = session,
      output_plot = sleep_spiral_plot,
      format = shiny::reactive(input$download_format),
      width = 8,
      height = 6
    )

  })
}
