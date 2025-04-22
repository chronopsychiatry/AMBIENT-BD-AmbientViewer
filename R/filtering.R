#' Filter epochs based on session IDs
#'
#' @param epochs The epochs dataframe
#' @param sessions The sessions dataframe
#' @returns The epochs dataframe with only the epochs that belong to the specified sessions
#' @export
#' @examples
#' # Apply filtering to sessions to keep specific nights, and filter epochs accordingly
#' filtered_sessions <- filter_by_night_range(example_sessions, "2025-04-07", "2025-04-10")#
#' filtered_epochs <- filter_epochs_from_sessions(example_epochs, filtered_sessions)
#'
#' @seealso [filter_by_night_range()] to filter sessions by night range.
#' @family filtering
filter_epochs_from_sessions <- function(epochs, sessions) {
  epochs[epochs$session_id %in% unique(sessions$id), ]
}

#' Filter sessions for nights within a night range
#'
#' @param sessions The sessions dataframe
#' @param from_night The start night of the range (inclusive) in YYYY-MM-DD format
#' @param to_night The end night of the range (inclusive) in YYYY-MM-DD format
#' @returns The sessions dataframe with only the sessions that fall within the specified night range
#' @importFrom rlang .data
#' @export
#' @family filtering
#' @examples
#' filtered_sessions <- filter_by_night_range(example_sessions, "2025-04-07", "2025-04-10")
filter_by_night_range <- function(sessions, from_night, to_night) {
  from_night <- if (is.null(from_night)) min(sessions$night) else from_night
  to_night <- if (is.null(to_night)) min(sessions$night) else to_night
  sessions |>
    dplyr::filter(.data$night >= as.Date(from_night) &
                    .data$night <= as.Date(to_night))
}

#' Select subjects by ID
#'
#' @param sessions The sessions dataframe
#' @param subject_ids The subject IDs to select
#' @returns The sessions dataframe with only the sessions that belong to the specified subjects
#' @export
#' @family filtering
#' @seealso [select_devices()] to select sessions by device ID.
select_subjects <- function(sessions, subject_ids) {
  sessions[sessions$subject_id %in% subject_ids, ]
}

#' Select devices by ID
#'
#' @param sessions The sessions dataframe
#' @param device_ids The device IDs to select
#' @returns The sessions dataframe with only the sessions recorded by the specified devices
#' @export
#' @family filtering
#' @seealso [select_subjects()] to select sessions by subject ID.
select_devices <- function(sessions, device_ids) {
  sessions[sessions$device_id %in% device_ids, ]
}
