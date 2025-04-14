export_data_module <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::downloadButton(
      outputId = ns("download_sessions"),
      label = "Sessions"
    ),
    shiny::downloadButton(
      outputId = ns("download_epochs"),
      label = "Epochs"
    )
  )
}

export_data_server <- function(id, filtered_sessions, epochs) {
  shiny::moduleServer(id, function(input, output, session) {

    # Download handler for the sessions
    output$download_sessions <- shiny::downloadHandler(
      filename = function() {
        paste("Sessions_", Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        shiny::req(filtered_sessions())
        readr::write_csv(filtered_sessions(), file)
        logging::loginfo(paste0("Exporting Session data (", nrow(filtered_sessions()), " rows)"))
      }
    )

    # Download handler for the epochs
    output$download_epochs <- shiny::downloadHandler(
      filename = function() {
        paste("Epochs_", Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        shiny::req(filtered_sessions(), epochs())
        epoch_data <- filter_epochs_from_sessions(epochs(), filtered_sessions())
        readr::write_csv(epoch_data, file)
        logging::loginfo(paste0("Exporting Epoch data (", nrow(epoch_data), " rows)"))
      }
    )


  })
}
