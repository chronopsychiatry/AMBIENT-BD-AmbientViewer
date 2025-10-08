input_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h4("1. Load data"),
    shiny::actionButton(ns("load_example_data"), "Load Example Data"),
    shiny::div(
      style = "margin-bottom: -0.6rem;",
      shiny::fileInput(
        ns("sessions_file"),
        "Sessions",
        accept = c(".csv")
      )
    ),
    shiny::div(
      style = "margin-bottom: -0.6rem;",
      shiny::fileInput(
        ns("epochs_file"),
        "Epochs",
        accept = c(".csv")
      )
    ),
    shiny::h4("2. Set column names"),
    shiny::actionButton(ns("open_session_col_names"), "Set Session Columns"),
    shiny::br(), shiny::br(),
    shiny::actionButton(ns("open_epoch_col_names"), "Set Epoch Columns"),
  )
}

input_server <- function(id, session, common) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Example data----
    shiny::observeEvent(input$load_example_data, {
      common$logger |> write_log("Loaded example session and epoch data", type = "complete")

      common$sessions(AmbientViewer::example_sessions)
      common$epochs(AmbientViewer::example_epochs)
      common$sessions_colnames(get_session_colnames(common$sessions()))
      common$epochs_colnames(get_epoch_colnames(common$epochs()))
    })

    # Sessions ----
    shiny::observeEvent(input$sessions_file, {
      shiny::req(input$sessions_file)
      common$logger |> write_log(paste0("Loading session file: ", input$sessions_file$name), type = "starting")
      data <- load_sessions(input$sessions_file$datapath)
      if (data$.data_type[1] == "none") {
        common$logger |> write_log("Could not detect session data type. Please set column names.", type = "warning")
      } else {
        common$logger |> write_log(paste0("Detected session data type: ", data$.data_type[1]), type = "info")
      }
      common$sessions(data)
      common$sessions_colnames(get_session_colnames(data))
    })

    sessions <- shiny::reactive({
      shiny::req(common$sessions())
      data <- common$sessions()
      col <- common$sessions_colnames()
      if (!is.null(col$session_start) && (is.null(col$night) || !"night" %in% colnames(data))) {
        data <- group_sessions_by_night(data, col_names = list(session_start = col$session_start))
        set_colname(common$sessions_colnames, "night", "night")
      }
      if (!is.null(data$annotation)) {
        common$annotations(data.frame(
          id = data[[common$sessions_colnames()$id]],
          annotation = as.character(data$annotation),
          stringsAsFactors = FALSE
        ))
      } else {
        data$annotation <- ""
        common$annotations(data.frame(
          id = data[[common$sessions_colnames()$id]],
          annotation = "",
          stringsAsFactors = FALSE
        ))
      }
      data
    })

    shiny::observeEvent(input$open_session_col_names, {
      shiny::req(sessions())
      show_colnames_modal(
        ns = ns,
        colnames_list = colnames(sessions()),
        current_map = common$sessions_colnames(),
        title = "Set Session Column Names",
        save_id = "save_session_col_names",
        reset_id = "reset_session_col_names"
      )
    })

    shiny::observeEvent(input$reset_session_col_names, {
      common$sessions_colnames(get_session_colnames(sessions()))
      shiny::removeModal()
    })

    shiny::observeEvent(input$save_session_col_names, {
      shiny::req(sessions())
      keys <- names(common$sessions_colnames())
      vals <- lapply(keys, function(key) {
        val <- input[[paste0("col_", key)]]
        if (identical(val, "-")) NULL else val
      })
      common$sessions_colnames(stats::setNames(vals, keys))
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
        common$logger |> write_log(paste0("Detected epoch data type: ", data$.data_type[1]), type = "info")
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
      if (!is.null(col$timestamp) && (is.null(col$night) || !"night" %in% colnames(data))) {
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
        if (identical(val, "-")) NULL else val
      })
      common$epochs_colnames(stats::setNames(vals, keys))
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
    choices <- c("-", colnames_list)
    shiny::selectInput(
      inputId = ns(paste0("col_", key)),
      label = key,
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
      do.call(shiny::tagList, inputs)
    )
  )
}

set_colname <- function(colnames_reactive, key, value) {
  col_map <- colnames_reactive()
  col_map[[key]] <- value
  colnames_reactive(col_map)
}
