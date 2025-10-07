sleep_regularity_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::HTML("Click on metrics names for more information.<br>"),
    shiny::tableOutput(ns("sleep_sessions_regularity_table")),
    shiny::tableOutput(ns("sleep_epochs_regularity_table")),
    shiny::HTML("<span>Metrics based on <a href='https://doi.org/10.1093/sleep/zsab103' target='_blank'>Fischer et al. (2021)</a></span>")
  )
}

sleep_regularity_server <- function(id, sessions, epochs, common) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    sessions_in <- sessions
    epochs_in <- epochs

    sessions <- shiny::reactive({
      shiny::req(sessions_in())
      sessions_in()[sessions_in()$display, ]
    })

    epochs <- shiny::reactive({
      shiny::req(epochs_in())
      epochs_in()[epochs_in()$display, ]
    })

    metric_names_sessions <- c(
      "Mid-sleep Standard Deviation",
      "Social Jet Lag",
      "Composite Phase Deviation",
      "Chronotype"
    )
    metric_ids_sessions <- c(
      "msd", "sjl", "cpd", "chronotype"
    )

    metric_names_epochs <- c(
      "Interdaily Stability",
      "Sleep Regularity Index"
    )
    metric_ids_epochs <- c(
      "is", "sri"
    )

    metric_values_sessions <- shiny::reactive({
      if (is.null(sessions()) || nrow(sessions()) == 0) return(rep(NA, length(metric_names_sessions)))
      c(
        sd_time(sessions()[[common$sessions_colnames()$time_at_midsleep]], unit = "hour"),
        social_jet_lag(sessions(), col_names = common$sessions_colnames()),
        composite_phase_deviation(sessions(), col_names = common$sessions_colnames()),
        chronotype(sessions(), col_names = common$sessions_colnames())
      )
    })

    metric_values_epochs <- shiny::reactive({
      if (is.null(epochs()) || nrow(epochs()) == 0) return(rep(NA, length(metric_names_epochs)))
      c(
        interdaily_stability(epochs(), col_names = common$epochs_colnames()),
        sleep_regularity_index(epochs(), col_names = common$epochs_colnames())
      )
    })

    output$sleep_sessions_regularity_table <- shiny::renderUI({
      shiny::tags$table(
        class = "table",
        style = "width: 300px;",
        shiny::tags$thead(
          shiny::tags$tr(
            shiny::tags$th("Metric"),
            shiny::tags$th("Value")
          )
        ),
        shiny::tags$tbody(
          c(
            lapply(seq_along(metric_names_sessions), function(i) {
              shiny::tags$tr(
                shiny::tags$td(
                  shiny::actionLink(ns(paste0("metric_", metric_ids_sessions[i])), metric_names_sessions[i])
                ),
                shiny::tags$td(
                  round(metric_values_sessions()[i], 2)
                )
              )
            })
          )
        )
      )
    })

    output$sleep_epochs_regularity_table <- shiny::renderUI({
      shiny::tags$table(
        class = "table",
        style = "width: 300px;",
        shiny::tags$thead(
          shiny::tags$tr(
            shiny::tags$th("Metric"),
            shiny::tags$th("Value")
          )
        ),
        shiny::tags$tbody(
          c(
            lapply(seq_along(metric_names_epochs), function(i) {
              shiny::tags$tr(
                shiny::tags$td(
                  shiny::actionLink(ns(paste0("metric_", metric_ids_epochs[i])), metric_names_epochs[i])
                ),
                shiny::tags$td(
                  round(metric_values_epochs()[i], 2)
                )
              )
            })
          )
        )
      )
    })

    shiny::observeEvent(input$metric_msd, {
      show_metric_modal("Mid-sleep_Standard_Deviation")
    })
    shiny::observeEvent(input$metric_is, {
      show_metric_modal("Interdaily_Stability")
    })
    shiny::observeEvent(input$metric_sjl, {
      show_metric_modal("Social_Jet_Lag")
    })
    shiny::observeEvent(input$metric_cpd, {
      show_metric_modal("Composite_Phase_Deviation")
    })
    shiny::observeEvent(input$metric_sri, {
      show_metric_modal("Sleep_Regularity_Index")
    })
    shiny::observeEvent(input$metric_chronotype, {
      show_metric_modal("Chronotype")
    })

    show_metric_modal <- function(metric_name) {
      if (FALSE) markdown::markdownToHTML() # Added to avoid R CMD check warning about unused function
      rmd_path <- system.file("shiny", package = "AmbientViewer")
      shiny::showModal(
        shiny::modalDialog(
          title = gsub("_", " ", metric_name),
          size = "l",
          shiny::includeMarkdown(paste0(rmd_path, "/Rmd/", metric_name, ".Rmd")),
          easyClose = TRUE,
          footer = shiny::modalButton("Close")
        )
      )
    }
  })
}
