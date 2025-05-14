#' Ambient Viewer app
#'
#' This function launches the Ambient Viewer app, a Shiny application for visualizing and analyzing sleep data.
#' @import shiny
#' @export
ambient_viewer <- function() {

  options(shiny.maxRequestSize = 50 * 1024^2)

  www_path <- system.file("www", package = "AmbientViewer")
  if (nzchar(www_path)) {
    addResourcePath(prefix = "www", directoryPath = www_path)
  } else {
    cli::cli_warn(c("!" = "The 'www' directory could not be found. Static resources may not load correctly."))
  }

  log_file <- "logs/AmbientViewer.log"
  if (!dir.exists("logs")) {
    dir.create("logs")
    cli::cli_alert_info(c("i" = "Created directory at {getwd()}/logs for logging."))
  }
  logging::basicConfig()
  logging::addHandler(logging::writeToFile, file = log_file, level = "INFO")
  schedule_log_clearing(log_file)

  ui <- fluidPage(
    shinyjs::useShinyjs(),
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "www/styles.css"),
      tags$script(src = "www/getIP.js")
    ),

    tags$h1(class = "custom-title", "Ambient Viewer"),

    theme = bslib::bs_theme(
      version = 5,
      "font-size-base" = "0.9rem",
      "btn-padding-y" = "0.25rem",   # vertical padding (default is 0.375rem)
      "btn-padding-x" = "0.5rem",   # horizontal padding (default is 0.75rem)
      "input-padding-y" = "0.25rem", # input vertical padding
      "input-padding-x" = "0.5rem",  # input horizontal padding
      "input-border-radius" = "0.2rem"
    ),

    sidebarLayout(
      # Side panel ----
      sidebarPanel(
        width = 2,

        # Data input, filtering and export ----
        h4("Data Input"),
        input_ui("input"),
        br(),
        h4("Filtering"),
        filtering_module("filtering"),
        br(),
        h4("Data export"),
        export_data_module("export_data")
      ),

      # Main panel ----
      mainPanel(

        # Tabset for tables ----
        div(
          class = "tabset-box",
          tabsetPanel(
            id = "main_tabs_tables",
            type = "tabs",
            tabPanel("Summary", summary_module_ui("summary")),
            tabPanel("Compliance", compliance_module("compliance"), value = "compliance_tab"),
            tabPanel("Filtering", filtering_tab("filtering"), value = "filtering_tab")
          ),
        ),

        # Tabset for plots ----
        div(
          class = "tabset-box",
          tabsetPanel(
            id = "main_tabs_plots",
            type = "tabs",
            tabPanel("Sleep Clock", sleep_clock_module_ui("sleep_clock")),
            tabPanel("Sleep Spiral", sleep_spiral_module_ui("sleep_spiral")),
            tabPanel("Sleep onset & Wakeup", bedtimes_waketimes_module_ui("bedtimes_waketimes")),
            tabPanel("Sleep Bubbles", sleep_bubbles_module_ui("sleep_bubbles")),
            tabPanel("Sleep Stages", sleep_stages_module_ui("sleep_stages")),
            tabPanel("Hypnogram", hypnogram_module_ui("hypnogram")),
            tabPanel("Session Timeseries", timeseries_sessions_module_ui("timeseries_sessions")),
            tabPanel("Epoch Timeseries", timeseries_module_ui("timeseries"))
          )
        )

      )
    ),

    # Footer ----
    div(
      style = "text-align: left; margin-top: 5px; font-size: 12px; color: #555;",
      textOutput("footer_text")
    )
  )

  server <- function(input, output, session) {

    user_ip <- reactive({
      input$getIP
    })

    observe({
      ip <- user_ip()$ip
      if (!is.null(ip)) {
        logging::loginfo(paste0("Started Ambient Viewer ", utils::packageVersion("AmbientViewer"), " - ", ip))
      }
    })

    # Footer text
    output$footer_text <- renderText({
      paste0(
        "Ambient Viewer version ", utils::packageVersion("AmbientViewer"), ". ",
        "Developed at the University of Edinburgh as part of the Ambient-BD project."
      )
    })

    # Data loading module
    data <- input_server("input", session)
    sessions <- data$sessions
    epochs <- data$epochs
    sessions_colnames <- data$sessions_colnames
    epochs_colnames <- data$epochs_colnames

    # Filtering and compliance module
    filtered_sessions <- filtering_server("filtering", sessions, sessions_colnames)
    filtered_epochs <- reactive(filter_epochs_from_sessions(
      epochs(),
      filtered_sessions(),
      session_col_names = sessions_colnames(),
      epoch_col_names = epochs_colnames()
    ))
    compliance_server("compliance", filtered_sessions, sessions_colnames)

    # Summary table module
    summary_server("summary", filtered_sessions, filtered_epochs, sessions_colnames, epochs_colnames)

    # Export data module
    export_data_server("export_data", filtered_sessions, filtered_epochs)

    # Plotting modules
    sleep_clock_module_server("sleep_clock", filtered_sessions, sessions_colnames)
    sleep_spiral_module_server("sleep_spiral", filtered_epochs, epochs_colnames)
    bedtimes_waketimes_module_server("bedtimes_waketimes", filtered_sessions, sessions_colnames)
    sleep_bubbles_module_server("sleep_bubbles", filtered_sessions, sessions_colnames)
    sleep_stages_module_server("sleep_stages", filtered_epochs, epochs_colnames)
    hypnogram_module_server("hypnogram", filtered_epochs, epochs_colnames)
    timeseries_sessions_module_server("timeseries_sessions", filtered_sessions, sessions_colnames)
    timeseries_module_server("timeseries", filtered_epochs, epochs_colnames)

  }

  shinyApp(ui, server)
}
