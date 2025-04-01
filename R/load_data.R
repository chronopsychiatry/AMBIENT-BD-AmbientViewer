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
  sessions <- group_sessions_by_night(sessions)
  epochs <- group_epochs_by_night(epochs)
  return(list(sessions = sessions, epochs = epochs))
}

#' Create a grouping by night for epoch data
#'
#' @param epochs The epochs dataframe
#' @returns The epochs dataframe with the `night` column added
#' @details The function creates a new column `night` that groups the epochs by night,
#' and an `adjusted_time` column to facilitate plotting from 12PM to 12PM.
#' Timepoints before 12 PM are considered part of the previous night.
#' @export
#' @examples
#' epochs <- group_epochs_by_night(epochs)
group_epochs_by_night <- function(epochs) {
  epochs <- epochs %>%
    dplyr::mutate(
      timestamp = as.POSIXct(timestamp, format = "%Y-%m-%dT%H:%M:%OS"),
      date = as.Date(timestamp),
      hour = as.numeric(format(timestamp, "%H")) + as.numeric(format(timestamp, "%M")) / 60,
      # Adjust time to always go from 12 PM to 12 PM (for plotting)
      adjusted_time = ifelse(hour < 12, hour + 24, hour) - 12,
      night = as.Date(ifelse(hour < 12, date - 1, date))
    )
  return(epochs)
}

#' Create a grouping by night for session data
#'
#' @param sessions The sessions dataframe
#' @returns The sessions dataframe with the `night` column added
#' @details The function creates a new column `night` that groups the sessions by night depending on their start time.
#' Sessions that start before 12 PM are considered part of the previous night.
#' @export
#' @examples
#' sessions <- group_sessions_by_night(sessions)
group_sessions_by_night <- function(sessions) {
  sessions <- sessions %>%
    dplyr::mutate(
      session_start = as.POSIXct(session_start, format = "%Y-%m-%dT%H:%M:%OS"),
      date = as.Date(session_start),
      start_hour = as.numeric(format(session_start, "%H")) + as.numeric(format(session_start, "%M")) / 60,
      night = as.Date(ifelse(start_hour < 12, date - 1, date))
    )
  return(sessions)
}
