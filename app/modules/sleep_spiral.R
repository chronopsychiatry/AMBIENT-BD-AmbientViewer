source("../R/sleep_spiral.R")
source("./modules/plot_helpers.R")

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
      choices = c("PNG" = "png", "SVG" = "svg"),
      inline = TRUE
    )
  )
}

sleep_spiral_module_server <- function(id, epochs, sessions) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Filter epochs based on sessions
    filtered_epochs <- shiny::reactive({
      shiny::req(epochs(), sessions())
      filter_epochs_from_sessions(epochs(), sessions())
    })

    # Reactive expression to store the plot
    sleep_spiral_plot <- shiny::reactive({
      shiny::req(filtered_epochs())
      plot_sleep_spiral(epochs = filtered_epochs())
    })

    # Render the plot
    output$sleep_spiral_plot <- shiny::renderPlot({
      shiny::req(sleep_spiral_plot())
      sleep_spiral_plot()
    })

    # Download handler for the plot
    output$download_plot <- get_plot_download_handler(
      session = session,
      output_plot = sleep_spiral_plot,
      format = shiny::reactive(input$download_format)
    )

  })
}
