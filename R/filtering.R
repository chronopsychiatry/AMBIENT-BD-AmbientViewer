#' Filter epochs based on session IDs
#'
#' @param epochs The epochs dataframe
#' @param sessions The sessions dataframe
#' @returns The epochs dataframe with only the epochs that belong to the specified sessions
#' @export
#' @examples
#' epochs <- filter_epochs_from_sessions(epochs, sessions)
filter_epochs_from_sessions <- function(epochs, sessions) {
  epochs <- epochs[epochs$session_id %in% unique(sessions$id), ]
  return(epochs)
}

#' Select subjects by ID
#'
#' @param sessions The sessions dataframe
#' @param subject_ids The subject IDs to select
#' @returns The sessions dataframe with only the sessions that belong to the specified subjects
#' @export
#' @examples
#' sessions <- select_subjects(sessions, c("sub_A", "sub_B", "sub_C"))
select_subjects <- function(sessions, subject_ids) {
  sessions <- sessions[sessions$subject_id %in% subject_ids, ]
  return(sessions)
}

#' Select devices by ID
#'
#' @param sessions The sessions dataframe
#' @param device_ids The device IDs to select
#' @returns The sessions dataframe with only the sessions recorded by the specified devices
#' @export
#' @examples
#' sessions <- select_devices(sessions, c("VT_001", "VT_002", "VT_003"))
select_devices <- function(sessions, device_ids) {
  sessions <- sessions[sessions$device_id %in% device_ids, ]
  return(sessions)
}
