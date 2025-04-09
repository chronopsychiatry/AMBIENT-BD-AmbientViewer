get_plot_download_handler <- function(session, output_plot, format) {
  download_plot <- shiny::downloadHandler(
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
  return(download_plot)
}