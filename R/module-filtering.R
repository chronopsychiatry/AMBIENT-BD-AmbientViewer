filtering_module <- function(id) {
  ns <- shiny::NS(id)
  bslib::accordion(
    bslib::accordion_panel(
      "Date",
      shiny::uiOutput(ns("date_range_slider"))
    ),
    bslib::accordion_panel(
      "Subject",
      shiny::uiOutput(ns("subject_select")),
      shiny::uiOutput(ns("sex_select")),
      shiny::uiOutput(ns("age_range_slider"))
    ),
    bslib::accordion_panel(
      "Sleep",
      shiny::uiOutput(ns("sleep_onset_range")),
      shiny::uiOutput(ns("time_in_bed_slider"))
    ),
    open = NULL
  )
}

filtering_tab <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::uiOutput(ns("removed_sessions_text")),
    shiny::tableOutput(ns("removed_sessions")),
    shiny::downloadButton(
      outputId = ns("download_removed_sessions"),
      label = ""
    )
  )
}

filtering_server <- function(id, sessions, sessions_colnames, annotations) {
  shiny::moduleServer(id, function(input, output, session) {

    output$date_range_slider <- shiny::renderUI({
      shiny::req(sessions())
      col <- sessions_colnames()
      if (!is.null(col$night) && nrow(sessions()) > 0) {
        min_date <- min(sessions()[[col$night]], na.rm = TRUE)
        max_date <- max(sessions()[[col$night]], na.rm = TRUE)
        shiny::sliderInput(
          inputId = session$ns("date_range"),
          label = "Date Range:",
          min = min_date,
          max = max_date,
          value = c(min_date, max_date),
          timeFormat = "%Y-%m-%d",
          ticks = FALSE
        )
      }
    })

    output$subject_select <- shiny::renderUI({
      shiny::req(sessions())
      col <- sessions_colnames()
      if (!is.null(col$subject_id)) {
        subject_choices <- unique(sessions()[[col$subject_id]])
        shinyWidgets::pickerInput(
          inputId = session$ns("subject_filter"),
          label = "Subjects:",
          choices = subject_choices,
          selected = subject_choices,
          multiple = TRUE,
          options = list(
            `actions-box` = TRUE,
            `live-search` = FALSE
          )
        )
      }
    })

    output$age_range_slider <- shiny::renderUI({
      shiny::req(sessions())
      col <- sessions_colnames()
      if (!is.null(col$birth_year)) {
        birth_years <- sessions()[[col$birth_year]]
        birth_years <- birth_years[!is.na(birth_years)]
        if (length(birth_years) > 0) {
          min_age <- lubridate::year(Sys.Date()) - max(birth_years)
          max_age <- lubridate::year(Sys.Date()) - min(birth_years)
          shiny::sliderInput(
            session$ns("age_range"),
            "Age Range:",
            min = min_age,
            max = max_age,
            value = c(min_age, max_age),
            step = 1
          )
        }
      }
    })

    output$sex_select <- shiny::renderUI({
      shiny::req(sessions())
      col <- sessions_colnames()
      if (!is.null(col$sex)) {
        sex_choices <- unique(sessions()[[col$sex]])
        shinyWidgets::pickerInput(
          inputId = session$ns("sex_filter"),
          label = "Sex:",
          choices = sex_choices,
          selected = sex_choices,
          multiple = TRUE,
          options = list(
            `actions-box` = TRUE,
            `live-search` = FALSE
          )
        )
      }
    })

    output$time_in_bed_slider <- shiny::renderUI({
      shiny::req(sessions())
      col <- sessions_colnames()
      if (!is.null(col$time_in_bed)) {
        shiny::sliderInput(
          inputId = session$ns("time_in_bed"),
          label = "Minimum Time in Bed:",
          min = 0,
          max = 12,
          value = 0,
          step = 1,
          post = "h",
          ticks = FALSE
        )
      }
    })

    output$sleep_onset_range <- shiny::renderUI({
      shiny::req(sessions())
      col <- sessions_colnames()
      if (!is.null(col$time_at_sleep_onset)) {
        shinyWidgets::sliderTextInput(
          inputId = session$ns("time_range"),
          label = "Sleep Onset Time:",
          choices = sprintf("%02d", c(13:23, 0:12)),
          selected = c("13", "12"),
          grid = TRUE
        )
      }
    })

    filtered_sessions <- shiny::reactive({
      shiny::req(sessions())
      col <- sessions_colnames()

      from_time <- if (!is.null(input$time_range[1])) paste0(input$time_range[1], ":00") else NULL
      to_time <- if (!is.null(input$time_range[2])) paste0(input$time_range[2], ":00") else NULL

      df <- sessions() |>
        tidyr::drop_na(dplyr::all_of(unname(unlist(col)))) |>
        remove_sessions_no_sleep(col_names = col)

      if (!"display" %in% colnames(df)) {
        df$display <- TRUE
      }
      if (!is.null(col$night)) {
        df <- df |>
          filter_by_night_range(input$date_range[1], input$date_range[2], col_names = col, flag_only = TRUE)
      }
      if (!is.null(col$time_at_sleep_onset)) {
        df <- df |>
          set_session_sleep_onset_range(from_time, to_time, col_names = col, flag_only = TRUE)
      }
      if (!is.null(col$time_in_bed)) {
        df <- df |>
          set_min_time_in_bed(input$time_in_bed, col_names = col, flag_only = TRUE)
      }
      if (!is.null(col$birth_year)) {
        df <- df |>
          filter_by_age_range(input$age_range[1], input$age_range[2], col_names = col, flag_only = TRUE)
      }
      if (!is.null(col$sex)) {
        df <- df |>
          filter_by_sex(input$sex_filter, col_names = col, flag_only = TRUE)
      }
      if (!is.null(col$subject_id)) {
        df <- df |>
          select_subjects(input$subject_filter, col_names = col, flag_only = TRUE)
      }
      df
    })

    removed_sessions <- shiny::reactive({
      shiny::req(filtered_sessions())
      col <- sessions_colnames()
      filtered_sessions() |>
        dplyr::mutate(annotation = annotations()$annotation[match(.data[[col$id]], annotations()$id)]) |>
        get_removed_sessions_table(col_names = sessions_colnames())
    })

    output$removed_sessions <- shiny::renderTable({
      shiny::req(removed_sessions())
      shiny::validate(
        shiny::need(nrow(removed_sessions()) > 0,
                    "No sessions have been removed.")
      )
      removed_sessions()
    })

    shiny::observe({
      shiny::req(removed_sessions(), filtered_sessions())
    })

    output$removed_sessions_text <- shiny::renderUI({
      shiny::req(removed_sessions())
      if (nrow(removed_sessions()) > 0) {
        shiny::HTML(paste0(
          "<br/><p>The filtering table below shows sessions that were removed by filtering.</p>",
          "<p>", nrow(removed_sessions()), " sessions have been removed.</p>"
        ))
      }
    })

    shiny::observe({
      shiny::req(filtered_sessions())
      output$download_removed_sessions <- get_table_download_handler(
        session = session,
        output_table = shiny::reactive(filtered_sessions()[!filtered_sessions()$display, ]),
        output_name = "removed_sessions"
      )
    })

    filtered_sessions
  })
}

get_removed_sessions_table <- function(sessions, col_names = NULL) {
  sessions[!sessions$display, ] |>
    make_sessions_display_table(col_names = col_names)
}
