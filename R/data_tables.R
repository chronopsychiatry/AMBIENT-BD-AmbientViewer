#' Make a summary of session information
#'
#' This function summarises session information, including the number of sessions, mean session length,
#' mean time at sleep onset and wakeup, subject and device ID.
#' @param sessions The sessions dataframe.
#' @returns A single-row dataframe summarizing session information.
#' @importFrom rlang .data
#' @export
#' @family data tables
#' @examples
#' get_sessions_summary(example_sessions)
#' @seealso [get_epochs_summary()] to summarise epoch information.
get_sessions_summary <- function(sessions) {
  sessions |>
    dplyr::summarise(
      total_sessions = dplyr::n(),
      mean_sleep_onset = mean_time(.data$time_at_sleep_onset),
      mean_wakeup_time = mean_time(.data$time_at_wakeup),
      mean_time_in_bed = mean(.data$time_in_bed) / 3600,
    )
}

#' Summarise epoch information
#'
#' This function displays the number of sessions in the epoch data, as well as the start and end dates of the epoch data
#' @param epochs The epochs dataframe
#' @returns A single-row dataframe summarising epoch information
#' @importFrom rlang .data
#' @export
#' @family data tables
#' @examples
#' get_epochs_summary(example_epochs)
#' @seealso [get_sessions_summary()] to summarise session information.
get_epochs_summary <- function(epochs) {
  if (nrow(epochs) == 0) {
    return(data.frame(total_sessions = 0, start_date = NA, end_date = NA))
  }

  epochs |>
    dplyr::mutate(timestamp = parse_time(.data$timestamp)) |>
    dplyr::summarise(
      total_sessions = dplyr::n_distinct(.data$session_id),
      start_date = format(min(.data$timestamp, na.rm = TRUE), "%Y-%m-%d"),
      end_date = format(max(.data$timestamp, na.rm = TRUE), "%Y-%m-%d")
    )
}
