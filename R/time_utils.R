#' Calculate the mean time from a vector of time strings
#'
#' This function calculates the mean time from a vector of time strings in the format "YYYY-MM-DD HH:MM:SS".
#' @param time_vector A vector of time strings in the format "YYYY-MM-DD HH:MM:SS".
#' @returns A string representing the mean time in the format "HH:MM".
#' @export
#' @examples
#' # Use on a vector of time strings
#' time_vector <- c("2025-04-08 23:00:00", "2025-04-09 01:00:00")
#' mean_time(time_vector)
#'
#' # Use on a dataframe column
#' mean_time(example_sessions$time_at_sleep_onset)
mean_time <- function(time_vector) {
  if (length(time_vector) == 0) {
    return(NA_character_)
  }

  time_vector <- time_vector[!is.na(time_vector)]
  if (!inherits(time_vector, "POSIXct")) {
    time_vector <- time_vector[time_vector != ""]
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
  if (!is.numeric(times) || any(times < 0)) {
    stop("`times` must be a numeric vector with non-negative values.", call. = FALSE)
  }
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
  if (!is.numeric(angle)) {
    stop("`angle` must be a numeric value.", call. = FALSE)
  }
  conversion_factor <- get_time_per_day(unit = unit)
  time <- (angle / (2 * pi)) * conversion_factor
  if (time >= 0) time else time + conversion_factor
}

#' Shift times to break at 12 pm
#'
#' This function shifts times so that the day starts at 12 PM.
#' This is useful for plotting night data
#' @param times A vector of times in POSIXct format (or character convertible to POSIXct).
#' @return A numerical vector of times (in hours) shifted to start at 12 PM
#' @export
#' @examples
#' # Shift sessions start times to start at 12 PM
#' shifted_times <- shift_times_by_12h(example_sessions$session_start)
#'
#' # Use dplyr::mutate to dicrectly add the shifted times to a dataframe
#' epochs <- example_epochs |>
#'   dplyr::mutate(shifted_time = shift_times_by_12h(timestamp))
shift_times_by_12h <- function(times) {
  if (length(times) == 0) {
    return(NA_real_)
  }

  times <- times[!is.na(times)]
  if (!inherits(times, "POSIXct")) {
    times <- lubridate::ymd_hms(times, tz = "UTC")
  }

  hour <- as.numeric(format(times, "%H", tz = "UTC")) +
    as.numeric(format(times, "%M", tz = "UTC")) / 60
  ifelse(hour < 12, hour + 24, hour) - 12
}

get_time_per_day <- function(unit = "second") {
  switch(unit,
    second = 86400,
    minute = 1440,
    hour = 24,
    stop("Invalid unit. Use 'second', 'minute', or 'hour'.", call = FALSE)
  )
}

is_iso8601_datetime <- function(column) {
  column <- column[!is.na(column) & column != ""]
  parsed <- suppressWarnings(lubridate::ymd_hms(column, quiet = TRUE, tz = "UTC"))
  all(!is.na(parsed))
}
