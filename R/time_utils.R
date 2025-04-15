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

  ref_day <- lubridate::floor_date(time_vector, unit = "day")

  time_vector |>
    difftime(time1 = _, time2 = ref_day, units = "secs") |>
    as.numeric() |>
    convert_times_to_mean_angle(unit = "second") |>
    convert_angle_to_time(unit = "second") |>
    as.POSIXct(x = _, origin = "1970-01-01", tz = "UTC") |>
    format("%H:%M")
}

#' Convert a vector of times to a mean angle
#'
#' This function converts a vector of times to a mean angle in radians.
#' It is useful to calculate average times spanning midnight
#' @param times A vector of times in seconds.
#' @param unit A string indicating the unit of time. Can be "second", "minute", or "hour".
#' @returns A numeric value representing the mean angle in radians.
#' @export
#' @seealso [convert_angle_to_time()] to convert the mean angle back to time format.
convert_times_to_mean_angle <- function(times, unit = "second") {
  conversion_factor <- get_time_per_day(unit = unit)
  atan2(mean(sin(2 * pi * times / conversion_factor)),
        mean(cos(2 * pi * times / conversion_factor)))
}

#' Convert an angle to time
#'
#' This function converts an angle in radians to time in the provided unit (can be "second", "minute" or "hour").
#' @param angle A numeric value representing the angle in radians.
#' @param unit A string indicating the unit of time. Can be "second", "minute", or "hour".
#' @returns A numeric value representing the time in the specified unit.
#' @export
#' @seealso [convert_times_to_mean_angle()] to calculate the average angle from a vector of time values.
convert_angle_to_time <- function(angle, unit = "second") {
  conversion_factor <- get_time_per_day(unit = unit)
  time <- (angle / (2 * pi)) * conversion_factor
  if (time >= 0) time else time + conversion_factor
}

#' Get the number of seconds, minutes or hours in a day
#'
#' @param unit A string indicating the unit of time. Can be "second", "minute", or "hour".
#' @returns The number of seconds, minutes or hours in a day.
#' @export
get_time_per_day <- function(unit = "second") {
  switch(unit,
    second = 86400,
    minute = 1440,
    hour = 24,
    stop("Invalid unit. Use 'second', 'minute', or 'hour'.", call = FALSE)
  )
}

#' Check if a column contains datetime data in ISO 8601 format
#'
#' @param column A vector of character strings to check
#' @returns TRUE if all non-NA values are in ISO 8601 format, FALSE otherwise
#' @export
is_iso8601_datetime <- function(column) {
  column <- column[!is.na(column) & column != ""]
  parsed <- suppressWarnings(lubridate::ymd_hms(column, quiet = TRUE, tz = "UTC"))
  return(all(!is.na(parsed)))
}
