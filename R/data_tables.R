#' Make a summary of session information
#'
#' This function summarizes session information, including the number of sessions, mean session length,
#' mean time at sleep onset and wakeup, subject and device ID.
#' @param sessions The sessions dataframe.
#' @returns A single-row dataframe summarizing session information.
#' @export
get_sessions_summary <- function(sessions) {
  sessions_summary <- sessions |>
    dplyr::mutate(
      session_start = lubridate::ymd_hms(session_start, tz = "UTC"),
      session_end = lubridate::ymd_hms(session_end, tz = "UTC"),
      sleep_onset = lubridate::ymd_hms(time_at_sleep_onset, tz = "UTC"),
      wakeup_time = lubridate::ymd_hms(time_at_wakeup, tz = "UTC"),
      session_duration_hours = as.numeric(difftime(session_end, session_start, units = "hours"))
    ) |>
    dplyr::summarise(
      subject_id = paste(unique(subject_id), collapse = ", "),
      device_id = paste(unique(device_serial_number), collapse = ", "),
      total_sessions = dplyr::n(),
      mean_sleep_onset = mean_time(dplyr::pull(., sleep_onset)),
      mean_wakeup_time = mean_time(dplyr::pull(., wakeup_time)),
      mean_session_length = mean(session_duration_hours, na.rm = TRUE)
    )
  return(sessions_summary)
}
