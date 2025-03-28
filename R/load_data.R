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
  epochs <- group_epochs_by_night(epochs)
  return(list(sessions = sessions, epochs = epochs))
}

#' Create a grouping by night for epoch data
#'
#' @param epochs The epochs dataframe
#' @returns The epochs dataframe with additional columns
#' @details The function creates a new column `night` that groups the epochs by night,
#' and an `adjusted_time` column to facilitate plotting from 12PM to 12PM.
#' @export
#' @examples
#' epochs <- group_epochs_by_night(epochs)
group_epochs_by_night <- function(epochs) {
  epochs <- epochs %>%
    dplyr::mutate(
      timestamp = as.POSIXct(timestamp, format = "%Y-%m-%dT%H:%M:%OS"),
      date = as.Date(timestamp),
      hour = as.numeric(format(timestamp, "%H")) + as.numeric(format(timestamp, "%M")) / 60,
      # Adjust time to always go from 12 PM to 12 PM
      adjusted_time = ifelse(hour < 12, hour + 24, hour) - 12,
      night = as.Date(ifelse(hour < 12, as.Date(date), as.Date(date) + 1))
    )
  return(epochs)
}