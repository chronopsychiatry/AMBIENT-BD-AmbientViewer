#' Load session data
#'
#' @param sessions_file The path to the sessions file
#' @returns A dataframe containing the session data
#' @details The function loads the session data from a CSV file and groups the sessions by night.
#' @export
#' @family data loading
load_sessions <- function(sessions_file) {
  if (file.info(sessions_file)$size < 10) {
    return(NULL)
  }
  utils::read.csv(sessions_file) |>
    group_sessions_by_night()
}

#' Load epoch data
#'
#' @param epochs_file The path to the epochs file
#' @returns A dataframe containing the epoch data
#' @details The function loads the epoch data from a CSV file and groups the epochs by night.
#' @export
#' @family data loading
load_epochs <- function(epochs_file) {
  if (file.info(epochs_file)$size < 10) {
    return(NULL)
  }
  utils::read.csv(epochs_file) |>
    group_epochs_by_night()
}
