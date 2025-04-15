#' Calculate the mean time from a vector of time strings
#'
#' This function calculates the mean time from a vector of time strings in the format "YYYY-MM-DD HH:MM:SS".
#' @param time_vector A vector of time strings in the format "YYYY-MM-DD HH:MM:SS".
#' @returns A string representing the mean time in the format "HH:MM".
#' @export
mean_time <- function(time_vector) {
  if (length(time_vector) == 0) {
    return(NA_character_)
  }

  if (!inherits(time_vector, "POSIXct")) {
    time_vector <- time_vector[!is.na(time_vector) & time_vector != ""]
    time_vector <- lubridate::ymd_hms(time_vector, tz = "UTC")
  }

  seconds_since_midnight <- as.numeric(difftime(time_vector, lubridate::floor_date(time_vector, "day"), units = "secs"))

  # Calculate the mean of the circular times
  mean_angle <- atan2(mean(sin(2 * pi * seconds_since_midnight / 86400)),
                      mean(cos(2 * pi * seconds_since_midnight / 86400)))

  # Convert the mean angle back to seconds since midnight
  mean_seconds <- (mean_angle / (2 * pi)) * 86400
  if (mean_seconds < 0) {
    mean_seconds <- mean_seconds + 86400
  }

  mean_time <- format(as.POSIXct(mean_seconds, origin = "1970-01-01", tz = "UTC"), "%H:%M")

  return(mean_time)
}
