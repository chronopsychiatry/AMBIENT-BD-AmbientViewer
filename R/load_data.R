#' Load session data
#'
#' @param sessions_file The path to the sessions file
#' @returns A dataframe containing the session data
#' @details The function loads the session data from a CSV file and groups the sessions by night.
#' @export
#' @family data loading
load_sessions <- function(sessions_file) {
  if (!file.exists(sessions_file)) {
    cli::cli_abort(c(
      "!" = "Sessions file not found: {.file {sessions_file}}",
      "i" = "Please check the file path."
    ))
  }

  df <- utils::read.csv(sessions_file)
  df <- convert_old_session_format(df)

  if (nrow(df) == 0) {
    cli::cli_warn(c(
      "!" = "Sessions table is empty",
      "i" = "Returning NULL"
    ))
    return(NULL)
  }
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
  if (!file.exists(epochs_file)) {
    cli::cli_abort(c(
      "!" = "Epochs file not found: {.file {epochs_file}}",
      "i" = "Please check the file path."
    ))
  }

  df <- utils::read.csv(epochs_file)
  df <- convert_old_epoch_format(df, epochs_file)

  if (nrow(df) == 0) {
    cli::cli_warn(c(
      "!" = "Epochs table is empty",
      "i" = "Returning NULL"
    ))
    return(NULL)
  }
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

convert_old_session_format <- function(sessions) {
  if (!"id" %in% colnames(sessions) && "session_id" %in% colnames(sessions)) {
    sessions |>
      dplyr::rename(id = "session_id") |>
      dplyr::rename(subject_id = "user_id")
  } else {
    sessions
  }
}

convert_old_epoch_format <- function(epochs, epochs_file) {
  if (!"session_id" %in% colnames(epochs)) {
    epochs |>
      dplyr::mutate(session_id = stringr::str_extract(basename(epochs_file), "^[^.]+"))
  } else {
    epochs
  }
}
