update_colorby_dropdown <- function(df, df_colnames, plot_options, input, session, input_id = "colorby") {
  shiny::observe({
    shiny::req(df())
    df <- df()[df()$display, ]
    if (nrow(df) == 0) {
      shiny::updateSelectInput(
        session,
        inputId = input_id,
        choices = NULL,
        selected = NULL
      )
      plot_options$colorby <- "default"
      return()
    }

    col <- Filter(Negate(is.null), df_colnames())

    excluded_vars <- c("session_start", "session_end", "time_at_sleep_onset", "time_at_wakeup",
                       "sleep_period", "time_in_bed", "timestamp", "signal_quality_mean")
    available_vars <- setdiff(c("default", "annotation", unlist(col, use.names = FALSE)), excluded_vars)

    # Update the dropdown, but preserve the selected variable if possible
    current_variable <- plot_options$colorby
    if (!is.null(current_variable) && current_variable %in% available_vars) {
      selected_variable <- current_variable
    } else {
      selected_variable <- available_vars[1]
    }
    plot_options$colorby <- selected_variable

    shiny::updateSelectInput(
      session,
      inputId = input_id,
      choices = available_vars,
      selected = selected_variable
    )
  })

  shiny::observe({
    plot_options$colorby <- input[[input_id]]
  })
}

update_variable_dropdown <- function(df, df_colnames, plot_options, input, session, input_id = "variable") {
  shiny::observe({
    shiny::req(df())
    df <- df()[df()$display, ]
    if (nrow(df) == 0) {
      # Handle empty dataframe: set placeholder values
      shiny::updateSelectInput(
        session,
        inputId = input_id,
        choices = NULL,
        selected = NULL
      )
      plot_options$variable <- NULL
      return()
    }

    col <- df_colnames()
    excluded_vars <- c(col$id, "state", col$subject_id, col$device_id, col$night, col$timestamp, "motion_data_count",
                       col$session_id, col$sleep_stage, "epoch_duration", ".data_type", "filename", "display")
    available_vars <- setdiff(names(df), excluded_vars)

    # Update the dropdown, but preserve the selected variable if possible
    current_variable <- plot_options$variable
    if (!is.null(current_variable) && current_variable %in% available_vars) {
      selected_variable <- current_variable
    } else {
      selected_variable <- available_vars[1]
    }
    plot_options$variable <- selected_variable

    shiny::updateSelectInput(
      session,
      inputId = input_id,
      choices = available_vars,
      selected = selected_variable
    )
  })

  # Update the stored plot options when the user changes them
  shiny::observe({
    plot_options$variable <- input[[input_id]]
  })
}