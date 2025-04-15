#' Filter epochs based on session IDs
#'
#' @param epochs The epochs dataframe
#' @param sessions The sessions dataframe
#' @returns The epochs dataframe with only the epochs that belong to the specified sessions
#' @export
filter_epochs_from_sessions <- function(epochs, sessions) {
  epochs[epochs$session_id %in% unique(sessions$id), ]
}

#' Select subjects by ID
#'
#' @param sessions The sessions dataframe
#' @param subject_ids The subject IDs to select
#' @returns The sessions dataframe with only the sessions that belong to the specified subjects
#' @export
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
#' @seealso [select_subjects()] to select sessions by subject ID.
select_devices <- function(sessions, device_ids) {
  sessions[sessions$device_id %in% device_ids, ]
}
