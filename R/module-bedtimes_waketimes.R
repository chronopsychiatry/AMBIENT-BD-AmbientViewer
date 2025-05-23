bedtimes_waketimes_module_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::fluidRow(
      shiny::column(4,
        shiny::selectInput(
          inputId = ns("groupby"),
          label = "Group by:",
          choices = c("night", "weekday", "workday")
        )
      ),
      shiny::column(4,
        shiny::selectInput(
          inputId = ns("colorby"),
          label = "Colour by:",
          choices = NULL
        )
      )
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

    plot_options <- shiny::reactiveValues(colorby = NULL)
    update_colorby_dropdown(sessions, sessions_colnames, plot_options, input, session)

    shiny::observe({
      if (!is.null(input$colorby) && input$colorby != "default" && input$groupby != "night") {
        shiny::updateSelectInput(
          session,
          inputId = "groupby",
          selected = "night"
        )
      }
    })

    bedtimes_waketimes_plot <- shiny::reactive({
      shiny::req(sessions())
      sessions <- sessions()[sessions()$display, ]
      if (nrow(sessions) == 0) {
        return(NULL)
      }
      col <- sessions_colnames()
      shiny::validate(
        shiny::need(!is.null(col$time_at_sleep_onset), "'time_at_sleep_onset' column was not specified."),
        shiny::need(!is.null(col$time_at_wakeup), "'time_at_wakeup' column was not specified."),
        shiny::need(!is.null(col$night), "'night' column was not specified."),
        shiny::need(!is.null(col$is_workday), "'is_workday' column was not specified.")
      )
      plot_bedtimes_waketimes(
        sessions = sessions,
        groupby = input$groupby,
        color_by = input$colorby,
        col_names = sessions_colnames()
      )
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
