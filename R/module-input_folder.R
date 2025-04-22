input_folder_module <- function(id) {
  ns <- shiny::NS(id)
  shinyFiles::shinyDirButton(ns("folder"),
                             "Select Folder",
                             "Please select the folder containing the data files")
}

input_folder_server <- function(id, session) {
  shiny::moduleServer(id, function(input, output, session) {
    volumes <- shinyFiles::getVolumes()()
    shinyFiles::shinyDirChoose(input, "folder", roots = volumes, session = session)
    folder_path <- shiny::reactiveVal("")

    shiny::observe({
      shiny::req(input$folder)
      folder_path(shinyFiles::parseDirPath(volumes, input$folder))
    })

    folder_path
  })
}
