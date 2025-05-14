input_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::div(
      style = "margin-bottom: -0.6rem;",
      shiny::fileInput(
        ns("sessions_file"),
        "Sessions",
        accept = c(".csv")
      )
    ),
    shiny::actionButton(ns("open_session_col_names"), "Set Session Columns"),
    shiny::br(), shiny::br(),
    shiny::div(
      style = "margin-bottom: -0.6rem;",
      shiny::fileInput(
        ns("epochs_file"),
        "Epochs",
        accept = c(".csv")
      )
    ),
    shiny::actionButton(ns("open_epoch_col_names"), "Set Epoch Columns"),
    shiny::br(), shiny::br()
  )
}

input_server <- function(id, session) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Sessions ----
    sessions_colnames <- shiny::reactiveVal()

    sessions <- shiny::reactive({
      shiny::req(input$sessions_file)
      logging::loginfo(paste0("Loading sessions file: ", input$sessions_file$name))
      sessions <- load_sessions(input$sessions_file$datapath)
      sessions_colnames(get_session_colnames(sessions))
      sessions
    })

    shiny::observeEvent(input$open_session_col_names, {
      shiny::req(sessions())
      show_colnames_modal(
        ns = ns,
        colnames_list = colnames(sessions()),
        current_map = sessions_colnames(),
        title = "Set Session Column Names",
        save_id = "save_session_col_names",
        reset_id = "reset_session_col_names"
      )
    })

    shiny::observeEvent(input$reset_session_col_names, {
      sessions_colnames(get_session_colnames(sessions()))
      shiny::removeModal()
    })

    shiny::observeEvent(input$save_session_col_names, {
      keys <- names(sessions_colnames())
      vals <- lapply(keys, function(key) {
        val <- input[[paste0("col_", key)]]
        if (identical(val, "-")) NULL else val
      })
      sessions_colnames(setNames(vals, keys))
      shiny::removeModal()
    })


    # Epochs ----
    epochs_colnames <- shiny::reactiveVal()

    epochs <- shiny::reactive({
      shiny::req(input$epochs_file)
      logging::loginfo(paste0("Loading epochs file: ", input$epochs_file$name))
      epochs <- load_epochs(input$epochs_file$datapath)
      if (epochs$.data_type[1] == "somnofy_v1") {
        epochs$session_id <- stringr::str_extract(input$epochs_file$name, "^[^.]+")
      }
      epochs_colnames(get_epoch_colnames(epochs))
      epochs
    })

    shiny::observeEvent(input$open_epoch_col_names, {
      shiny::req(epochs())
      show_colnames_modal(
        ns = ns,
        colnames_list = colnames(epochs()),
        current_map = epochs_colnames(),
        title = "Set Epoch Column Names",
        save_id = "save_epoch_col_names",
        reset_id = "reset_epoch_col_names"
      )
    })

    shiny::observeEvent(input$reset_epoch_col_names, {
      epochs_colnames(get_epoch_colnames(epochs()))
      shiny::removeModal()
    })

    shiny::observeEvent(input$save_epoch_col_names, {
      keys <- names(epochs_colnames())
      vals <- lapply(keys, function(key) {
        val <- input[[paste0("col_", key)]]
        if (identical(val, "-")) NULL else val
      })
      epochs_colnames(setNames(vals, keys))
      shiny::removeModal()
    })

    list(
      sessions = sessions,
      epochs = epochs,
      sessions_colnames = sessions_colnames,
      epochs_colnames = epochs_colnames
    )
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
      footer = tagList(
        shiny::actionButton(ns(reset_id), "Reset"),
        shiny::modalButton("Cancel"),
        shiny::actionButton(ns(save_id), "Save")
      ),
      do.call(shiny::tagList, inputs)
    )
  )
}
