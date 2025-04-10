#' Make a summary of session information
#'
#' This function summarizes session information, including the number of sessions, mean session length, subject and device ID.
#' @param sessions The sessions dataframe.
#' @returns A single-row dataframe summarizing session information.
#' @export
get_sessions_summary <- function(sessions) {
  sessions_summary <- sessions %>%
    dplyr::mutate(
      session_start = as.POSIXct(session_start, format = "%Y-%m-%dT%H:%M:%OS", tz = "UTC"),
      session_end = as.POSIXct(session_end, format = "%Y-%m-%dT%H:%M:%OS", tz = "UTC"),
      session_duration_hours = as.numeric(difftime(session_end, session_start, units = "hours"))
    ) %>%
    dplyr::summarise(
      subject_id = paste(unique(subject_id), collapse = ", "),
      device_id = paste(unique(device_serial_number), collapse = ", "),
      total_sessions = dplyr::n(),
      mean_session_length = mean(session_duration_hours, na.rm = TRUE)
    )
  return(sessions_summary)
}
