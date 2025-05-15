bedtimes_waketimes_module_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::selectInput(
      inputId = ns("groupby"),
      label = "Time grouping:",
      choices = c("night", "weekday", "workday")
    ),
    shiny::plotOutput(ns("bedtimes_waketimes_plot")),
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

bedtimes_waketimes_module_server <- function(id, sessions, sessions_colnames) {
  shiny::moduleServer(id, function(input, output, session) {

    bedtimes_waketimes_plot <- shiny::reactive({
      shiny::req(sessions())
      sessions <- sessions()[sessions()$display, ]
      if (nrow(sessions) == 0) {
        return(NULL)
      }
      plot_bedtimes_waketimes(sessions = sessions, groupby = input$groupby, col_names = sessions_colnames())
    })

    output$bedtimes_waketimes_plot <- shiny::renderPlot({
      shiny::req(bedtimes_waketimes_plot())
      bedtimes_waketimes_plot()
    })

    output$download_plot <- get_plot_download_handler(
      session = session,
      output_plot = bedtimes_waketimes_plot,
      format = shiny::reactive(input$download_format),
      width = 8,
      height = 6
    )

  })
}
