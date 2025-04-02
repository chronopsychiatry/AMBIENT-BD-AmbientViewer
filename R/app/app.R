library(shiny)
library(shinyFiles)

source("modules/input.R")
source("modules/filtering.R")
source("modules/compliance.R")

ui <- fluidPage(
  shinyjs::useShinyjs(),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),

  titlePanel("Ambient Viewer"),

  sidebarLayout(

    # Sidebar panel for inputs ----
    sidebarPanel(

      # Input: Select folder and date range ----
      h4("Data Input"),
      input_folder_module("folder_selector"),
      input_data_files_module("file_selector"),
      br(),
      h4("Filtering"),
      filtering_module("filtering")
    ),

    # Main panel for displaying outputs ----
    mainPanel(

      # Output: Tabset ----
      tabsetPanel(
        id = "main_tabs",
        type = "tabs",
        tabPanel("Plot", plotOutput("plot")),
        tabPanel("Summary", verbatimTextOutput("summary")),
        tabPanel("Compliance", compliance_tab_module("compliance"), value = "compliance_tab"),
      )

    )
  )
)

server <- function(input, output, session) {

  # Data loading module
  # folder_path <- input_folder_server("folder_selector", session)
  # For testing purposes:
  folder_path <- shiny::reactive("E:/Daniel/Ambient-BD/downloaded_data/Testing/future_neuro_pilot-sub_01JNDH3Z5NP0PSV82NFBGPV31X/data")
  selected_file <- input_data_files_server("file_selector", folder_path)
  data <- load_data_module_server("load_data", folder_path, selected_file)
  sessions <- shiny::reactive(data()$sessions)
  epochs <- shiny::reactive(data()$epochs)

  # Filtering and compliance module
  filtered_sessions <- filtering_server("filtering", sessions)
  compliance_server("compliance", filtered_sessions)

}

shinyApp(ui, server)
