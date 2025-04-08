#' Set minimum time in bed
#'
#' @param sessions The sessions dataframe
#' @param min_time_in_bed The minimum time in bed in hours
#' @returns The sessions dataframe with only the sessions that meet the minimum time in bed requirement
#' @export
set_min_time_in_bed <- function(sessions, min_time_in_bed) {
  sessions <- sessions[sessions$time_in_bed >= min_time_in_bed * 60 * 60, ]
  return(sessions)
}

#' Set session start time range
#'
#' @param sessions The sessions dataframe
#' @param from_time Include sessions that started after this time
#' @param to_time Include sessions that started before this time
#' @returns The sessions dataframe with only the sessions that started within the specified time range
#' @export
set_session_start_time_range <- function(sessions, from_time, to_time) {
  session_times <- substr(sessions$session_start, 12, 16)

  session_times <- as.POSIXct(session_times, format = "%H:%M")
  from_time <- as.POSIXct(from_time, format = "%H:%M")
  to_time <- as.POSIXct(to_time, format = "%H:%M")

  if (from_time <= to_time) {
    sessions <- sessions[session_times >= from_time & session_times <= to_time, ]
  } else {
    sessions <- sessions[session_times >= from_time | session_times <= to_time, ]
  }
  return(sessions)
}

#' Remove sessions with no sleep
#'
#' @param sessions The sessions dataframe
#' @returns The sessions dataframe with only the sessions that have a sleep period greater than 0
#' @export
remove_sessions_no_sleep <- function(sessions) {
  sessions <- sessions[sessions$sleep_period > 0, ]
  return(sessions)
}

#' Get non-complying sessions (i.e. where there is more than one session on the same day)
#'
#' @param sessions The sessions dataframe
#' @returns The sessions dataframe with only the sessions that are non-complying
#' @export
get_non_complying_sessions <- function(sessions) {
  # sessions$date <- substr(sessions$session_start, 1, 10)
  non_complying_sessions <- sessions[sessions$night %in% sessions$night[duplicated(sessions$night)], ]
  return(non_complying_sessions)
}
