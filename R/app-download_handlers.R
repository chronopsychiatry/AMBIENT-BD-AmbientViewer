get_plot_download_handler <- function(session, output_plot, format, width = 8, height = 6) {
  shiny::downloadHandler(
    filename = function() {
      paste0("plot_", Sys.Date(), ".", format())
    },
    content = function(file) {
      shiny::req(output_plot())
      plot <- output_plot()

      logging::loginfo(paste0("Exporting plot in ", format(), " format."))

      ggplot2::ggsave(filename = file, plot = plot, device = format(), bg = "white", width = width, height = height)
    }
  )
}

get_table_download_handler <- function(session, output_table, output_name = "") {
  shiny::downloadHandler(
    filename = function() {
      paste0(output_name, "_", Sys.Date(), ".csv")
    },
    content = function(file) {
      shiny::req(output_table())
      readr::write_csv(output_table(), file)
      logging::loginfo(paste0("Exporting table: ", output_name, " (", nrow(output_table()), " rows)"))
    }
  )
}
