library(shiny)
library(shinyFiles)

source("modules/input.R")
source("modules/filtering.R")
source("modules/compliance.R")
source("modules/summary.R")
source("modules/timeseries.R")
source("modules/sleep_stages.R")

ui <- fluidPage(
  shinyjs::useShinyjs(),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),

  tags$h1(class = "custom-title", "Ambient Viewer"),

  sidebarLayout(
    # Sidebar panel for inputs ----
    sidebarPanel(
      width = 2,

      # Input: Select folder and date range ----
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

    # Main panel for displaying outputs ----
    mainPanel(

      # Output: Tabset ----
      div(
        class = "tabset-box",
        tabsetPanel(
          id = "main_tabs_tables",
          type = "tabs",
          tabPanel("Summary", summary_module_ui("summary")),
          tabPanel("Compliance", compliance_module("compliance"), value = "compliance_tab")
        ),
      ),

      div(
        class = "tabset-box",
        tabsetPanel(
          id = "main_tabs_plots",
          type = "tabs",
          tabPanel("Timeseries", timeseries_module_ui("timeseries")),
          tabPanel("Sleep Stages", sleep_stages_module_ui("sleep_stages"))
        )
      )

    )
  )
)

server <- function(input, output, session) {

  # Data loading module
  # folder_path <- input_folder_server("folder_selector", session)
  # For testing purposes:
  folder_path <- shiny::reactive("//home/dthedie/Documents/Ambient-BD/downloader_data/downloaded_data/Testing/future_neuro_pilot-sub_01JNDH3Z5NP0PSV82NFBGPV31X/data")
  selected_file <- input_data_files_server("file_selector", folder_path)
  data <- load_data_module_server("load_data", folder_path, selected_file)
  sessions <- shiny::reactive(data()$sessions)
  epochs <- shiny::reactive(data()$epochs)

  # Filtering and compliance module
  filtered_sessions <- filtering_server("filtering", sessions)
  compliance_server("compliance", filtered_sessions)

  # Summary table module
  summary_server("summary", filtered_sessions)

  # Export data module
  export_data_server("export_data", filtered_sessions, epochs)

  # Plotting module
  timeseries_module_server("timeseries", epochs, filtered_sessions)
  sleep_stages_module_server("sleep_stages", epochs, filtered_sessions)

}

shinyApp(ui, server)
