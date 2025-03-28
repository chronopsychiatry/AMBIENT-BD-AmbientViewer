#' Load session and epoch data
#'
#' @param folder The folder where the data is stored
#' @param basename The time range of the files to load
#' @returns A list containing the sessions and epochs dataframes
#' @export
#' @examples
#' load_data("data", "2025-03-03_2025-03-11")
load_data <- function(folder, basename) {
  sessions <- read.csv(paste0(folder, "/", basename, "_sessions_reports.csv"))
  epochs <- read.csv(paste0(folder, "/", basename, "_epoch_data.csv"))
  return(list(sessions = sessions, epochs = epochs))
}
