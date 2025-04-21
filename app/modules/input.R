input_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    input_folder_module(ns("folder_selector")),
    shiny::br(),
    shiny::br(),
    input_data_files_module(ns("file_selector")),
    shiny::br(),
  )
}

input_server <- function(id, session) {
  shiny::moduleServer(id, function(input, output, session) {
    folder_path <- input_folder_server("folder_selector", session)
    shiny::observe({
      shiny::req(folder_path())
      if (folder_path() != "") {
        logging::loginfo(paste0("Loading files from: ", folder_path()))
      }
    })
    selected_sessions <- input_sessions_files_server("file_selector", folder_path)
    selected_epochs <- input_epochs_files_server("file_selector", folder_path, sessions)

    sessions <- shiny::reactive({
      shiny::req(folder_path(), selected_sessions())
      load_sessions(file.path(folder_path(), selected_sessions()))
    })
    epochs <- shiny::reactive({
      shiny::req(folder_path(), selected_epochs())
      load_epochs(file.path(folder_path(), selected_epochs()))
    })

    shiny::observe({
      shiny::req(sessions())
      logging::loginfo(paste0("Loaded sessions ", selected_sessions(), " (", nrow(sessions()), " rows)"))

      shiny::req(epochs())
      logging::loginfo(paste0("Loaded epochs ", selected_epochs(), " (", nrow(epochs()), " rows)"))
    })
    list(sessions = sessions, epochs = epochs)
  })
}
