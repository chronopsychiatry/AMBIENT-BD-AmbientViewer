library(tidyverse)

#' Load session data
#'
#' @param sessions_file The path to the sessions file
#' @returns A dataframe containing the session data
#' @details The function loads the session data from a CSV file and groups the sessions by night.
#' @export
#' @examples
#' sessions <- load_sessions("data/sessions_reports.csv")
load_sessions <- function(sessions_file) {
  if (file.info(sessions_file)$size < 10) {
    return(NULL)
  }
  sessions <- read.csv(sessions_file)
  sessions <- group_sessions_by_night(sessions)
  return(sessions)
}

#' Load epoch data
#'
#' @param epochs_file The path to the epochs file
#' @returns A dataframe containing the epoch data
#' @details The function loads the epoch data from a CSV file and groups the epochs by night.
#' @export
#' @examples
#' epochs <- load_epochs("data/epoch_data.csv")
load_epochs <- function(epochs_file) {
  if (file.info(epochs_file)$size < 10) {
    return(NULL)
  }
  epochs <- read.csv(epochs_file)
  epochs <- group_epochs_by_night(epochs)
  return(epochs)
}

#' Load session and epoch data
#'
#' @param folder The folder where the data is stored
#' @param basename The time range of the files to load
#' @returns A list containing the sessions and epochs dataframes
#' @export
#' @examples
#' load_data("data", "2025-03-03_2025-03-11")
load_data <- function(folder, basename) {
  sessions_file <- paste0(folder, "/", basename, "_sessions_reports.csv")
  epochs_file <- paste0(folder, "/", basename, "_epoch_data.csv")
  sessions <- load_sessions(sessions_file)
  epochs <- load_epochs(epochs_file)
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
      timestamp = as.POSIXct(timestamp, format = "%Y-%m-%dT%H:%M:%OS", tz = "UTC"),
      date = as.Date(timestamp, tz = "UTC"),
      hour = as.numeric(format(timestamp, "%H", tz = "UTC")) + as.numeric(format(timestamp, "%M", tz = "UTC")) / 60,
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
      start_time = as.POSIXct(session_start, format = "%Y-%m-%dT%H:%M:%OS", tz = "UTC"),
      date = as.Date(start_time, tz = "UTC"),
      start_hour = as.numeric(format(start_time, "%H", tz = "UTC")) +
        as.numeric(format(start_time, "%M", tz = "UTC")) / 60,
      night = as.Date(ifelse(start_hour < 12, date - 1, date))
    ) %>%
    dplyr::select(-start_time, -date, -start_hour)
  return(sessions)
}
