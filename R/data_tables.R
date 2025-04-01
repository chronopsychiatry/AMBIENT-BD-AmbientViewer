#' Get duplicate sessions
#'
#' This function identifies sessions that occurred during the same "night" (from 12pm to 12pm).
#' @param sessions The sessions dataframe.
#' @returns A dataframe containing only the duplicate sessions.
#' @export
#' @examples
#' duplicate_sessions <- get_duplicate_sessions(sessions)
get_duplicate_sessions <- function(sessions) {
  duplicate_sessions <- sessions %>%
    dplyr::mutate(
      session_start = as.POSIXct(session_start, format = "%Y-%m-%dT%H:%M:%OS"),
      session_end = as.POSIXct(session_end, format = "%Y-%m-%dT%H:%M:%OS"),
      session_duration_hours = as.numeric(difftime(session_end, session_start, units = "hours"))
    ) %>%
    dplyr::group_by(night) %>%
    dplyr::filter(dplyr::n() > 1) %>%
    dplyr::ungroup()
  return(duplicate_sessions)
}
