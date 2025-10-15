input_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h4("1. Load data"),
    shiny::actionButton(ns("load_example_data"), "Load Example Data"),
    input_sessions_ui(ns("sessions_input_panel")),
    shiny::hr(),
    shiny::h5("Epochs"),
    shiny::fileInput(
      ns("epochs_file"),
      NULL,
      accept = c(".csv")
    ),
    shiny::h4("2. Set column names"),
    shiny::actionButton(ns("open_session_col_names"), "Set Session Columns"),
    shiny::br(), shiny::br(),
    shiny::actionButton(ns("open_epoch_col_names"), "Set Epoch Columns"),
  )
}

input_server <- function(id, common) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Example data----
    shiny::observeEvent(input$load_example_data, {
      common$logger |> write_log("Loaded example session and epoch data", type = "complete")

      session_data <- AmbientViewer::example_sessions
      epoch_data <- AmbientViewer::example_epochs
      common$sessions_colnames(get_session_colnames(session_data))
      common$epochs_colnames(get_epoch_colnames(epoch_data))
      common$sessions(init_sessions(session_data, common))
      common$epochs(epoch_data)
    })

    # Sessions ----
    input_sessions_server("sessions_input_panel", common)


    shiny::observeEvent(input$open_session_col_names, {
      shiny::req(common$sessions())
      show_colnames_modal(
        ns = ns,
        colnames_list = colnames(common$sessions()),
        current_map = common$sessions_colnames(),
        title = "Set Session Column Names",
        save_id = "save_session_col_names",
        reset_id = "reset_session_col_names"
      )
    })

    shiny::observeEvent(input$reset_session_col_names, {
      common$sessions_colnames(get_session_colnames(common$sessions()))
      shiny::removeModal()
    })

    shiny::observeEvent(input$save_session_col_names, {
      keys <- names(common$sessions_colnames())
      vals <- lapply(keys, function(key) {
        val <- input[[paste0("col_", key)]]
        if (identical(val, "")) NULL else val
      })
      common$sessions_colnames(stats::setNames(vals, keys))
      if (!is.null(common$sessions_colnames()$night)) {
        sessions <- common$sessions()
        night_col <- common$sessions_colnames()$night
        sessions[[night_col]] <- as.Date(sessions[[night_col]])
        common$sessions(sessions)
      }
      if (is.null(common$sessions_colnames()$night) && !is.null(common$sessions_colnames()$session_start)) {
        sessions <- common$sessions() |>
          group_sessions_by_night(col_names = common$sessions_colnames())
        set_colname(common$sessions_colnames, "night", "night")
        common$sessions(sessions)
      }
      shiny::removeModal()
    })


    # Epochs ----
    shiny::observeEvent(input$epochs_file, {
      shiny::req(input$epochs_file)
      common$logger |> write_log(paste0("Loading epoch file: ", input$epochs_file$name), type = "starting")
      data <- load_epochs(input$epochs_file$datapath)
      if (data$.data_type[1] == "none") {
        common$logger |> write_log("Could not detect epoch data type. Please set column names.", type = "warning")
      } else {
        common$logger |> write_log(paste0("Detected epoch data type: ", data$.data_type[1]), type = "complete")
        common$logger |> write_log("Column names were set automatically", type = "info")
      }
      if (data$.data_type[1] == "somnofy_v1") {
        data$session_id <- stringr::str_extract(input$epochs_file$name, "^[^.]+")
      }
      common$epochs(data)
      common$epochs_colnames(get_epoch_colnames(data))
    })

    epochs <- shiny::reactive({
      shiny::req(common$epochs())
      data <- common$epochs()
      col <- common$epochs_colnames()
      if (!is.null(col$timestamp) && (is.null(col$night))) {
        data <- group_epochs_by_night(data, col_names = list(timestamp = col$timestamp))
        set_colname(common$epochs_colnames, "night", "night")
      }
      data
    })

    shiny::observeEvent(input$open_epoch_col_names, {
      shiny::req(epochs())
      show_colnames_modal(
        ns = ns,
        colnames_list = colnames(epochs()),
        current_map = common$epochs_colnames(),
        title = "Set Epoch Column Names",
        save_id = "save_epoch_col_names",
        reset_id = "reset_epoch_col_names"
      )
    })

    shiny::observeEvent(input$reset_epoch_col_names, {
      common$epochs_colnames(get_epoch_colnames(epochs()))
      shiny::removeModal()
    })

    shiny::observeEvent(input$save_epoch_col_names, {
      keys <- names(common$epochs_colnames())
      vals <- lapply(keys, function(key) {
        val <- input[[paste0("col_", key)]]
        if (identical(val, "")) NULL else val
      })
      common$epochs_colnames(stats::setNames(vals, keys))
      if (is.null(common$epochs_colnames()$night) && !is.null(common$epochs_colnames()$timestamp)) {
        epochs <- epochs() |>
          group_epochs_by_night(col_names = common$epochs_colnames())
        set_colname(common$epochs_colnames, "night", "night")
        common$epochs(epochs)
      }
      shiny::removeModal()
    })
  })
}

show_colnames_modal <- function(
  ns,
  colnames_list,
  current_map,
  title = "Set Session Column Names",
  save_id = "save_col_names",
  reset_id = "reset_col_names"
) {
  inputs <- lapply(names(current_map), function(key) {
    current_value <- as.character(current_map[[key]])
    if (is.null(current_map[[key]]) || is.na(current_map[[key]])) current_value <- ""
    choices <- c("", colnames_list)
    label_text <- .sessions_long[[key]] %||% .epochs_long[[key]] %||% key
    help_text <- .sessions_help[[key]] %||% .epochs_help[[key]] %||% NULL
    label <- shiny::tagList(
      label_text,
      if (!is.null(help_text)) bslib::tooltip(
        shiny::tags$span(
          shiny::icon("circle-info"),
          class = "colnames-help"
        ),
        help_text,
        placement = "right",
        options = list(delay = list(show = 0, hide = 100))
      )
    )
    shiny::selectInput(
      inputId = ns(paste0("col_", key)),
      label = label,
      choices = choices,
      selected = current_value
    )
  })
  shiny::showModal(
    shiny::modalDialog(
      title = title,
      size = "l",
      easyClose = TRUE,
      footer = shiny::tagList(
        shiny::actionButton(ns(reset_id), "Reset"),
        shiny::modalButton("Cancel"),
        shiny::actionButton(ns(save_id), "Save")
      ),
      shiny::p("Hint: type in the boxes to search for column names."),
      do.call(shiny::tagList, inputs)
    )
  )
}

set_colname <- function(colnames_reactive, key, value) {
  col_map <- colnames_reactive()
  col_map[[key]] <- value
  colnames_reactive(col_map)
}
