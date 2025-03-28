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
