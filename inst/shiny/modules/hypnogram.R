hypnogram_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::HTML("<b>Requires Epoch data</b>"),
    shiny::plotOutput(ns("hypnogram_plot")),
    shiny::downloadButton(
      outputId = ns("download_plot"),
      label = NULL,
      class = "small-btn"
    ),
    shiny::radioButtons(
      inputId = ns("download_format"),
      label = NULL,
      choices = c("PNG" = "png", "PDF" = "pdf", "SVG" = "svg"),
      inline = TRUE
    )
  )
}

hypnogram_server <- function(id, epochs, common) {
  shiny::moduleServer(id, function(input, output, session) {

    hypnogram_plot <- shiny::reactive({
      shiny::req(epochs())
      epochs <- epochs()[epochs()$display, ]
      if (nrow(epochs) == 0) {
        return(NULL)
      }
      col <- common$epochs_colnames()
      shiny::validate(
        shiny::need(!is.null(col$timestamp), "'timestamp' column was not specified."),
        shiny::need(!is.null(col$sleep_stage), "'sleep_stage' column was not specified.")
      )
      plot_hypnogram(epochs = epochs, col_names = common$epochs_colnames())
    })

    output$hypnogram_plot <- shiny::renderPlot({
      shiny::req(hypnogram_plot())
      hypnogram_plot()
    })

    output$download_plot <- get_plot_download_handler(
      session = session,
      common = common,
      output_plot = hypnogram_plot,
      format = shiny::reactive(input$download_format),
      width = 12,
      height = 6
    )

  })
}
