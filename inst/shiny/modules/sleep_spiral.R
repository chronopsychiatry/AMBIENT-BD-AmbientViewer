sleep_spiral_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::HTML("<b>Requires Epoch data</b>"),
    shiny::selectInput(
      inputId = ns("colorby"),
      label = "Colour by:",
      choices = NULL
    ),
    shiny::plotOutput(ns("sleep_spiral_plot")),
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

sleep_spiral_server <- function(id, epochs, common) {
  shiny::moduleServer(id, function(input, output, session) {

    plot_options <- shiny::reactiveValues(colorby = NULL)
    update_colorby_dropdown(epochs, common$epochs_colnames, plot_options, input, session)

    sleep_spiral_plot <- shiny::reactive({
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
      plot_sleep_spiral(
        epochs = epochs,
        color_by = input$colorby,
        col_names = common$epochs_colnames()
      )
    })

    output$sleep_spiral_plot <- shiny::renderPlot({
      shiny::req(sleep_spiral_plot())
      sleep_spiral_plot()
    })

    output$download_plot <- get_plot_download_handler(
      session = session,
      common = common,
      output_plot = sleep_spiral_plot,
      format = shiny::reactive(input$download_format),
      width = 8,
      height = 6
    )

  })
}
