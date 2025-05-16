hypnogram_module_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::plotOutput(ns("hypnogram_plot")),
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

hypnogram_module_server <- function(id, epochs, epochs_colnames) {
  shiny::moduleServer(id, function(input, output, session) {

    hypnogram_plot <- shiny::reactive({
      shiny::req(epochs())
      epochs <- epochs()[epochs()$display, ]
      if (nrow(epochs) == 0) {
        return(NULL)
      }
      plot_hypnogram(epochs = epochs, col_names = epochs_colnames())
    })

    output$hypnogram_plot <- shiny::renderPlot({
      shiny::req(hypnogram_plot())
      hypnogram_plot()
    })

    output$download_plot <- get_plot_download_handler(
      session = session,
      output_plot = hypnogram_plot,
      format = shiny::reactive(input$download_format),
      width = 12,
      height = 6
    )

  })
}
