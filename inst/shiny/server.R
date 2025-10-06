function(input, output, session) {

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

  # Filtering module
  filtered_sessions <- filtering_server("filtering", sessions, sessions_colnames, data$annotations)
  filtered_epochs <- reactive(filter_epochs_from_sessions(
    epochs(),
    filtered_sessions(),
    session_col_names = sessions_colnames(),
    epoch_col_names = epochs_colnames(),
    flag_only = TRUE
  ))

  # Annotation module
  annotated_sessions <- annotation_server("annotation", filtered_sessions, sessions_colnames, data$annotations)
  annotated_epochs <- reactive(annotate_epochs_from_sessions(
    annotated_sessions(),
    filtered_epochs(),
    sessions_colnames(),
    epochs_colnames()
  ))

  # Compliance module
  compliance_server("compliance", annotated_sessions, sessions_colnames)

  # Summary table module
  summary_server("summary", annotated_sessions, annotated_epochs, sessions_colnames, epochs_colnames)

  # Sleep regularity module
  sleep_regularity_server("sleep_regularity", annotated_sessions, annotated_epochs, sessions_colnames, epochs_colnames)

  # Export data module
  export_data_server("export_data", annotated_sessions, annotated_epochs)

  # Plotting modules
  sleep_clock_server("sleep_clock", annotated_sessions, sessions_colnames)
  sleep_spiral_server("sleep_spiral", annotated_epochs, epochs_colnames)
  bedtimes_waketimes_server("bedtimes_waketimes", annotated_sessions, sessions_colnames)
  sleep_distributions_server("sleep_distributions", annotated_sessions, sessions_colnames)
  sleep_bubbles_server("sleep_bubbles", annotated_sessions, sessions_colnames)
  hypnogram_server("hypnogram", annotated_epochs, epochs_colnames)
  timeseries_sessions_server("timeseries_sessions", annotated_sessions, sessions_colnames)
  timeseries_server("timeseries", annotated_epochs, epochs_colnames)

}
