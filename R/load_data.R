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

  sessions <- utils::read.csv(sessions_file)

  if (nrow(sessions) == 0) {
    cli::cli_warn(c(
      "!" = "Sessions table is empty",
      "i" = "Returning NULL"
    ))
    return(NULL)
  }

  sessions <- get_sessions_format(sessions)

  if ("session_start" %in% names(sessions)) {
    sessions |>
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

  epochs <- utils::read.csv(epochs_file)

  if (nrow(epochs) == 0) {
    cli::cli_warn(c(
      "!" = "Epochs table is empty",
      "i" = "Returning NULL"
    ))
    return(NULL)
  }

  epochs <- get_epochs_format(epochs)

  if (epochs$.data_type[1] == "somnofy_v1") {
    epochs <- epochs |>
      dplyr::mutate(session_id = stringr::str_extract(basename(epochs_file), "^[^.]+"))
  }

  if ("timestamp" %in% names(epochs)) {
    epochs |>
      group_epochs_by_night()
  } else {
    cli::cli_abort(c(
      "!" = "Can't find a 'timestamp' column.",
      "i" = "Please check the csv file contains epoch data."
    ))
  }
}

#' Set the data type for a dataframe
#'
#' @param df The dataframe to set the data type for
#' @param data_type The data type to set. Currently available data types: "somnofy_v1", "somnofy_v2"
#' @returns The dataframe with the data type set
#' @details The dataframe type is used by Ambient Viewer functions to determine the correct column names.
#' Note: you do not need to set the data type if you are using the `load_sessions` or `load_epochs` functions.
#' @export
#' @examples
#' example_sessions <- set_data_type(example_sessions, "somnofy_v2")
set_data_type <- function(df, data_type) {
  if (!data_type %in% c("somnofy_v1", "somnofy_v2")) {
    cli::cli_abort(c(
      "!" = "Invalid data type: {.val {data_type}}",
      "i" = "Available data types: somnofy_v1, somnofy_v2."
    ))
  }
  df$.data_type <- data_type
  df
}

get_sessions_format <- function(sessions) {
  if (all(c(
    "id", "subject_id", "device_serial_number", "session_start", "session_end",
    "time_at_sleep_onset", "time_at_wakeup", "sleep_period", "time_in_bed", "is_workday"
  ) %in% colnames(sessions))) {
    sessions <- set_data_type(sessions, "somnofy_v2")
  } else if (all(c(
    "session_id", "user_id", "sex", "birth_year", "session_start", "session_end",
    "time_at_sleep_onset", "time_at_wakeup", "sleep_period", "time_in_bed", "is_workday"
  ) %in% colnames(sessions))) {
    sessions <- set_data_type(sessions, "somnofy_v1")
  } else {
    cli::cli_warn(c(
      "!" = "Could not infer the Sessions data type.",
      "i" = "Please check the csv file contains session data."
    ))
  }
  sessions
}

get_epochs_format <- function(epochs) {
  if (all(c("timestamp", "session_id", "sleep_stage") %in% colnames(epochs))) {
    epochs <- set_data_type(epochs, "somnofy_v2")
  } else if (all(c("timestamp", "sleep_stage") %in% colnames(epochs))) {
    epochs <- set_data_type(epochs, "somnofy_v1")
  } else {
    cli::cli_warn(c(
      "!" = "Could not infer the Epochs data type.",
      "i" = "Please check the csv file contains epoch data."
    ))
  }
  epochs
}
