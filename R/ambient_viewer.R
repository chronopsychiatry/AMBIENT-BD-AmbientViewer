#' Ambient Viewer app
#'
#' This function launches the Ambient Viewer app, a Shiny application for visualizing and analyzing sleep data.
#' @import shiny
#' @export
ambient_viewer <- function() {

  logging::basicConfig()

  app_path <- system.file("shiny", package = "AmbientViewer")
  shiny::runApp(app_path)
}
