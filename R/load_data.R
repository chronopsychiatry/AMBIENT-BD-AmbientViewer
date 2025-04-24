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
  df <- utils::read.csv(sessions_file)
  if ("session_start" %in% names(df)) {
    df |>
      group_sessions_by_night()
  } else {
    cli::cli_abort(c(
      "!" = "Can't find a 'session_start' column.",
      "i" = "Please check the csv file contains session data."
    ))
  }
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
  df <- utils::read.csv(epochs_file)
  if ("timestamp" %in% names(df)) {
    df |>
      group_epochs_by_night()
  } else {
    cli::cli_abort(c(
      "!" = "Can't find a 'timestamp' column.",
      "i" = "Please check the csv file contains epoch data."
    ))
  }
}
