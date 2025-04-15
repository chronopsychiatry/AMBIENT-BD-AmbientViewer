library(shiny)
library(shinyFiles)
library(logging)
library(AmbientViewer)

source("housekeeping.R")
module_files <- list.files("modules", pattern = "\\.R$", full.names = TRUE)
lapply(module_files, source)

# Configure logging
log_file <- "logs/AmbientViewer.log"
basicConfig()
addHandler(writeToFile, file = log_file, level = "INFO")
schedule_log_clearing(log_file)

ui <- fluidPage(
  shinyjs::useShinyjs(),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
    tags$script(src = "getIP.js")
  ),

  tags$h1(class = "custom-title", "Ambient Viewer"),

  sidebarLayout(
    # Side panel ----
    sidebarPanel(
      width = 2,

      # Data input, filtering and export ----
      h4("Data Input"),
      input_folder_module("folder_selector"),
      input_data_files_module("file_selector"),
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
          tabPanel("Compliance", compliance_module("compliance"), value = "compliance_tab")
        ),
      ),

      # Tabset for plots ----
      div(
        class = "tabset-box",
        tabsetPanel(
          id = "main_tabs_plots",
          type = "tabs",
          tabPanel("Sleep Spiral", sleep_spiral_module_ui("sleep_spiral")),
          tabPanel("Sleep Clock", sleep_clock_module_ui("sleep_clock")),
          tabPanel("Sleep Bubbles", sleep_bubbles_module_ui("sleep_bubbles")),
          tabPanel("Sleep Stages", sleep_stages_module_ui("sleep_stages")),
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

  user_ip <- shiny::reactive({
    input$getIP
  })

  observe({
    ip <- user_ip()$ip
    if (!is.null(ip)) {
      loginfo(paste0("Started Ambient Viewer ", packageVersion("AmbientViewer"), " - ", ip))
    }
  })

  # Footer text
  output$footer_text <- renderText({
    paste0(
      "Ambient Viewer version ", packageVersion("AmbientViewer"), ". ",
      "Developed at the University of Edinburgh as part of the Ambient-BD project."
    )
  })

  # Data loading module
  folder_path <- input_folder_server("folder_selector", session)
  selected_file <- input_data_files_server("file_selector", folder_path)
  data <- load_data_module_server("load_data", folder_path, selected_file)
  sessions <- shiny::reactive(data()$sessions)
  epochs <- shiny::reactive(data()$epochs)
  shiny::observe({
    shiny::req(sessions(), epochs())
    logging::loginfo(paste0("Loaded sessions ", selected_file(), " (", nrow(sessions()), " rows)"))
    logging::loginfo(paste0("Loaded epochs ", selected_file(), " (", nrow(epochs()), " rows)"))
  })

  # Filtering and compliance module
  filtered_sessions <- filtering_server("filtering", sessions)
  compliance_server("compliance", filtered_sessions)

  # Summary table module
  summary_server("summary", filtered_sessions)

  # Export data module
  export_data_server("export_data", filtered_sessions, epochs)

  # Plotting modules
  sleep_spiral_module_server("sleep_spiral", epochs, filtered_sessions)
  sleep_clock_module_server("sleep_clock", filtered_sessions)
  sleep_bubbles_module_server("sleep_bubbles", filtered_sessions)
  sleep_stages_module_server("sleep_stages", epochs, filtered_sessions)
  timeseries_sessions_module_server("timeseries_sessions", filtered_sessions)
  timeseries_module_server("timeseries", epochs, filtered_sessions)

}

shinyApp(ui, server)
