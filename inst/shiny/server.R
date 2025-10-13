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
  filtering_server("filtering", common)

  # Annotation module
  annotation_server("annotation", common)

  # Compliance module
  compliance_server("compliance", common$sessions, common)

  # Summary table module
  summary_server("summary", common$sessions, common$epochs, common)

  # Sleep regularity module
  sleep_regularity_server("sleep_regularity", common$sessions, common$epochs, common)

  # Export data module
  export_data_server("export_data", common$sessions, common$epochs, common)

  # Plotting modules
  sleep_clock_server("sleep_clock", common$sessions, common)
  sleep_spiral_server("sleep_spiral", common$epochs, common)
  bedtimes_waketimes_server("bedtimes_waketimes", common$sessions, common)
  sleep_distributions_server("sleep_distributions", common$sessions, common)
  sleep_bubbles_server("sleep_bubbles", common$sessions, common)
  hypnogram_server("hypnogram", common$epochs, common)
  timeseries_sessions_server("timeseries_sessions", common$sessions, common)
  timeseries_server("timeseries", common$epochs, common)

}
