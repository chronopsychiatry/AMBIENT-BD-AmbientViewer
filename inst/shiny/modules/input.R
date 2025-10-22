input_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h4("1. Load data"),
    shiny::actionButton(ns("load_example_data"), "Load Example Data"),
    input_sessions_ui(ns("sessions_input_panel")),
    shiny::hr(),
    input_epochs_ui(ns("epochs_input_panel")),
    shiny::h4("2. Set column names"),
    shiny::actionButton(ns("open_session_col_names"), "Set Session Columns"),
    shiny::actionButton(ns("open_epoch_col_names"), "Set Epoch Columns"),
  )
}

input_server <- function(id, common) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Example data----
    shiny::observeEvent(input$load_example_data, {
      init_sessions(AmbientViewer::example_sessions, common)
      init_epochs(AmbientViewer::example_epochs, common)
      common$logger |> write_log("Loaded example session and epoch data", type = "complete")
    })

    # Sessions ----
    input_sessions_server("sessions_input_panel", common)

    shiny::observeEvent(input$open_session_col_names, {
      shiny::req(common$sessions())
      show_colnames_modal(
        ns = ns,
        colnames_list = colnames(common$sessions()),
        current_map = get_colnames(common$sessions()),
        title = "Set Session Column Names",
        save_id = "save_session_col_names",
        reset_id = "reset_session_col_names"
      )
    })

    shiny::observeEvent(input$reset_session_col_names, {
      sessions <- set_colnames(common$sessions(), NULL)
      col <- get_colnames(sessions)
      common$sessions(set_colnames(sessions, col))
      shiny::removeModal()
    })

    shiny::observeEvent(input$save_session_col_names, {
      keys <- names(get_colnames(common$sessions()))
      vals <- lapply(keys, function(key) {
        val <- input[[paste0("col_", key)]]
        if (identical(val, "")) NULL else val
      })
      common$sessions(set_colnames(common$sessions(), stats::setNames(vals, keys)))
      common$sessions(clean_sessions(common$sessions))
      shiny::removeModal()
    })


    # Epochs ----
    input_epochs_server("epochs_input_panel", common)

    shiny::observeEvent(input$open_epoch_col_names, {
      shiny::req(epochs())
      show_colnames_modal(
        ns = ns,
        colnames_list = colnames(epochs()),
        current_map = get_colnames(common$epochs()),
        title = "Set Epoch Column Names",
        save_id = "save_epoch_col_names",
        reset_id = "reset_epoch_col_names"
      )
    })

    shiny::observeEvent(input$reset_epoch_col_names, {
      epochs <- set_colnames(common$epochs(), NULL)
      col <- get_colnames(epochs)
      common$epochs(set_colnames(epochs, col))
      shiny::removeModal()
    })

    shiny::observeEvent(input$save_epoch_col_names, {
      keys <- names(get_colnames(common$epochs()))
      vals <- lapply(keys, function(key) {
        val <- input[[paste0("col_", key)]]
        if (identical(val, "")) NULL else val
      })
      common$epochs(set_colnames(common$epochs(), stats::setNames(vals, keys)))
      common$epochs(clean_epochs(common$epochs))
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
