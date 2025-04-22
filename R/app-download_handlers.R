get_plot_download_handler <- function(session, output_plot, format) {
  shiny::downloadHandler(
    filename = function() {
      paste0("plot_", Sys.Date(), ".", format())
    },
    content = function(file) {
      shiny::req(output_plot())
      plot <- output_plot()

      if (format() == "png") {
        png(file, width = 1600, height = 1200, res = 300)
        print(plot)
        dev.off()
      } else if (format() == "svg") {
        svg(file, width = 8, height = 6)
        print(plot)
        dev.off()
      }
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
