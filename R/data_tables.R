#' Make a summary of session information
#'
#' This function summarizes session information, including the number of sessions, mean session length,
#' mean time at sleep onset and wakeup, subject and device ID.
#' @param sessions The sessions dataframe.
#' @returns A single-row dataframe summarizing session information.
#' @importFrom rlang .data
#' @export
#' @examples
#' get_sessions_summary(example_sessions)
get_sessions_summary <- function(sessions) {
  sessions |>
    dplyr::mutate(
      session_start = lubridate::ymd_hms(.data$session_start, tz = "UTC"),
      session_end = lubridate::ymd_hms(.data$session_end, tz = "UTC"),
      sleep_onset = lubridate::ymd_hms(.data$time_at_sleep_onset, tz = "UTC"),
      wakeup_time = lubridate::ymd_hms(.data$time_at_wakeup, tz = "UTC"),
      session_duration_hours = as.numeric(difftime(.data$session_end, .data$session_start, units = "hours"))
    ) |>
    dplyr::summarise(
      subject_id = paste(unique(.data$subject_id), collapse = ", "),
      device_id = paste(unique(.data$device_serial_number), collapse = ", "),
      total_sessions = dplyr::n(),
      mean_sleep_onset = mean_time(.data$sleep_onset),
      mean_wakeup_time = mean_time(.data$wakeup_time),
      mean_session_length = mean(.data$session_duration_hours, na.rm = TRUE)
    )
}
