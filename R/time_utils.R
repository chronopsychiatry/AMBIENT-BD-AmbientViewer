#' Calculate the mean time from a vector of time strings
#'
#' This function calculates the mean time from a vector of time strings in the format "YYYY-MM-DD HH:MM:SS".
#' @param time_vector A vector of time strings in format "YYYY-MM-DD HH:MM:SS", "HH:MM:SS" or "HH:MM".
#' @returns A string representing the mean time in the format "HH:MM".
#' @export
#' @family time processing
#' @examples
#' # Use on a vector of time strings representing full dates
#' time_vector <- c("2025-04-08 23:00:00", "2025-04-09 01:00:00")
#' mean_time(time_vector)
#'
#' # Use on time-only strings
#' time_vector <- c("22:56", "01:32")
#' mean_time(time_vector)
#'
#' # Use on a dataframe column
#' mean_time(example_sessions$time_at_sleep_onset)
mean_time <- function(time_vector) {
  if (length(time_vector) == 0) {
    return(NA_character_)
  }

  time_vector <- time_vector |>
    parse_time() |>
    stats::na.omit()

  ref_day <- lubridate::floor_date(time_vector, unit = "day")

  time_vector |>
    difftime(time1 = _, time2 = ref_day, units = "secs") |>
    as.numeric() |>
    convert_times_to_mean_angle(unit = "second") |>
    convert_angle_to_time(unit = "second") |>
    as.POSIXct(origin = "1970-01-01", tz = "UTC") |>
    format("%H:%M")
}

#' Calculate the minimum time from 12pm to 12pm
#'
#' This function calculates the minimum time from a vector of time strings in the format "YYYY-MM-DD HH:MM:SS".
#' It considers a time window from 12pm to 12pm the next day, so 11:00 is considered later than 13:00.
#' @param time_vector A vector of time strings in the format "YYYY-MM-DD HH:MM:SS".
#' @returns A string representing the minimum time in the format "HH:MM".
#' @export
#' @family time processing
#' @seealso [max_time()] to calculate the maximum time in the same format.
#' @examples
#' min_time(c("2025-04-08 23:00:00", "2025-04-09 01:00:00", "2025-04-09 02:30:00"))
min_time <- function(time_vector) {
  time_vector |>
    shift_times_by_12h() |>
    time_to_hours() |>
    min(na.rm = TRUE) |>
    (\(x) (x + 12) * 3600)() |>
    as.POSIXct(tz = "UTC") |>
    format("%H:%M")
}

#' Calculate the maximum time from 12pm to 12pm
#'
#' This function calculates the maximum time from a vector of time strings in the format "YYYY-MM-DD HH:MM:SS".
#' It considers a time window from 12pm to 12pm the next day, so 11:00 is considered later than 13:00.
#' @param time_vector A vector of time strings in the format "YYYY-MM-DD HH:MM:SS".
#' @returns A string representing the maximum time in the format "HH:MM".
#' @export
#' @family time processing
#' @seealso [min_time()] to calculate the minimum time in the same format.
#' @examples
#' max_time(c("2025-04-08 23:00:00", "2025-04-09 01:00:00", "2025-04-09 02:30:00"))
max_time <- function(time_vector) {
  time_vector |>
    shift_times_by_12h() |>
    time_to_hours() |>
    max(na.rm = TRUE) |>
    (\(x) (x + 12) * 3600)() |>
    as.POSIXct(tz = "UTC") |>
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
#' @family time processing
#' @seealso [convert_angle_to_time()] to convert the mean angle back to time format.
#' @examples
#' convert_times_to_mean_angle(c(23, 10, 0), unit = "hour")
convert_times_to_mean_angle <- function(times, unit = "second") {
  if (!is.numeric(times) || any(times < 0)) {
    cli::cli_abort(c("!" = "times must be a numeric vector with non-negative values."))
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
#' @family time processing
#' @seealso [convert_times_to_mean_angle()] to calculate the average angle from a vector of time values.
#' @examples
#' convert_angle_to_time(pi/2, unit = "hour")
convert_angle_to_time <- function(angle, unit = "second") {
  if (!is.numeric(angle)) {
    cli::cli_abort(c("!" = "angle must be a numeric value."))
  }
  conversion_factor <- get_time_per_day(unit = unit)
  time <- (angle / (2 * pi)) * conversion_factor
  if (time >= 0) time else time + conversion_factor
}

#' Shift times to break at 12 pm
#'
#' This function shifts times so that the day starts at 12 PM.
#' This is useful for plotting night data
#' @param times A vector of times in POSIXct format, character convertible to POSIXct, or numerical (in hours).
#' @return A vector of times in POSIXct format (or numerical if numerical provided as input) shifted to start at 12 PM
#' @export
#' @family time processing
#' @examples
#' # Shift a vector of times in HH:MM format
#' shift_times_by_12h(c("02:30", "16:00"))
#' #> "14:30" "04:00"
#'
#' # Shift times in YYYY-MM-DD HH:MM:SS format
#' shift_times_by_12h(c("2025-04-08 23:00:00", "2025-04-09 01:00:00"))
#' #> "2025-04-08 11:00" "2025-04-09 13:00"
#'
#' # Shift sessions start times to start at 12 PM
#' shifted_times <- shift_times_by_12h(example_sessions$session_start)
#'
#' # Use dplyr::mutate to dicrectly add the shifted times to a dataframe
#' epochs <- example_epochs |>
#'   dplyr::mutate(shifted_time = shift_times_by_12h(timestamp))
shift_times_by_12h <- function(times) {
  if (inherits(times, "numeric")) {
    return(dplyr::if_else(times < 12, times + 12, times - 12))
  }

  times <- parse_time(times)
  times <- dplyr::if_else(lubridate::hour(times) < 12, times + lubridate::hours(12), times - lubridate::hours(12))

  if (all(lubridate::year(times) == 0000)) {
    times |> format("%H:%M")
  } else {
    times
  }
}

#' Create a grouping by night for epoch data
#'
#' @param epochs The epochs dataframe
#' @returns The epochs dataframe with the `night` column added
#' @details The function creates a new column `night` that groups the epochs by night.
#' Timepoints before 12 PM are considered part of the previous night.
#' @importFrom rlang .data
#' @export
#' @seealso [group_sessions_by_night()] to group session data by night.
#' @family time processing
#' @examples
#' epochs <- group_epochs_by_night(example_epochs)
group_epochs_by_night <- function(epochs) {
  epochs |>
    dplyr::mutate(
      time_stamp = parse_time(.data$timestamp),
      date = lubridate::as_date(.data$time_stamp),
      hour = time_to_hours(.data$time_stamp),
      night = lubridate::as_date(ifelse(.data$hour < 12, .data$date - 1, .data$date))
    ) |>
    dplyr::select(-"time_stamp", -"date", -"hour")
}

#' Create a grouping by night for session data
#'
#' @param sessions The sessions dataframe
#' @returns The sessions dataframe with the `night` column added
#' @details The function creates a new column `night` that groups the sessions by night depending on their start time.
#' Sessions that start before 12 PM are considered part of the previous night.
#' @export
#' @family time processing
#' @seealso [group_epochs_by_night()] to group epoch data by night.
#' @examples
#' sessions <- group_sessions_by_night(example_sessions)
group_sessions_by_night <- function(sessions) {
  sessions |>
    dplyr::mutate(
      start_time = parse_time(.data$session_start),
      date = lubridate::as_date(.data$start_time),
      start_hour = time_to_hours(.data$start_time),
      night = lubridate::as_date(ifelse(.data$start_hour < 12, date - 1, date))
    ) |>
    dplyr::select(-"start_time", -"date", -"start_hour")
}

get_time_per_day <- function(unit = "second") {
  switch(unit,
    second = 86400,
    minute = 1440,
    hour = 24,
    cli::cli_abort(c("!" = "unit must be one of 'second', 'minute', or 'hour'.",
                     "x" = "You supplied {unit}."))
  )
}

is_iso8601_datetime <- function(column) {
  column <- column[!is.na(column) & column != ""]
  parsed <- suppressWarnings(lubridate::ymd_hms(column, quiet = TRUE))
  all(!is.na(parsed))
}

time_to_hours <- function(time_vector) {
  time_vector <- parse_time(time_vector)
  lubridate::hour(time_vector) + lubridate::minute(time_vector) / 60
}

parse_time <- function(time_vector) {
  if ((inherits(time_vector, "POSIXct") || inherits(time_vector, "POSIXt") || inherits(time_vector, "numeric"))) {
    return(time_vector)
  }
  time_vector <- gsub("(\\+|-)[0-9]{2}:[0-9]{2}$|Z$", "", time_vector) # Remove timezone information
  time_formats <- c("ymd_HMS", "ymd_HMSz", "HMS", "HM")
  lubridate::parse_date_time(time_vector, orders = time_formats, tz = NULL, quiet = TRUE)
}
