function(input, output, session) {

  # Init log window
  init_log_msg <- function() {
    intro <- paste0("Welcome to Ambient Viewer ", utils::packageVersion("AmbientViewer"))
    brk <- paste(rep("------", 5), collapse = "")
    expl <- "Please find messages for the user in this log window."
    log_init <- gsub(".{4}$", "", paste(intro, brk, expl, brk, "", sep = "<br>"))
    log_init
  }
  common$logger <- reactiveVal(init_log_msg())

  # Write out logs to the log Window
  observeEvent(common$logger(), {
    shinyjs::html(id = "logHeader", html = common$logger(), add = FALSE)
    shinyjs::js$scrollLogger()
  })

  # Footer text
  output$footer_text <- renderText({
    paste0(
      "Ambient Viewer version ", utils::packageVersion("AmbientViewer"), ". ",
      "Developed at the University of Edinburgh as part of the Ambient-BD project."
    )
  })

  # Data loading module
  input_server("input", session, common)

  # Filtering module
  filtered_sessions <- filtering_server("filtering", common)
  filtered_epochs <- reactive({
    req(common$epochs(), filtered_sessions())
    filter_epochs_from_sessions(
      common$epochs(),
      filtered_sessions(),
      session_col_names = common$sessions_colnames(),
      epoch_col_names = common$epochs_colnames(),
      flag_only = TRUE
    )
  })

  # Annotation module
  annotated_sessions <- annotation_server("annotation", filtered_sessions, common)
  annotated_epochs <- reactive(annotate_epochs_from_sessions(
    annotated_sessions(),
    filtered_epochs(),
    common$sessions_colnames(),
    common$epochs_colnames()
  ))

  # Compliance module
  compliance_server("compliance", annotated_sessions, common)

  # Summary table module
  summary_server("summary", annotated_sessions, annotated_epochs, common)

  # Sleep regularity module
  sleep_regularity_server("sleep_regularity", annotated_sessions, annotated_epochs, common)

  # Export data module
  export_data_server("export_data", annotated_sessions, annotated_epochs, common)

  # Plotting modules
  sleep_clock_server("sleep_clock", annotated_sessions, common)
  sleep_spiral_server("sleep_spiral", annotated_epochs, common)
  bedtimes_waketimes_server("bedtimes_waketimes", annotated_sessions, common)
  sleep_distributions_server("sleep_distributions", annotated_sessions, common)
  sleep_bubbles_server("sleep_bubbles", annotated_sessions, common)
  hypnogram_server("hypnogram", annotated_epochs, common)
  timeseries_sessions_server("timeseries_sessions", annotated_sessions, common)
  timeseries_server("timeseries", annotated_epochs, common)

}
