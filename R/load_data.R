#' Load session data
#'
#' @param sessions_file The path to the sessions file
#' @returns A dataframe containing the session data
#' @details The function loads the session data from a file and groups the sessions by night.
#' Supported formats: CSV, Excel, EDF.
#' @export
#' @family data loading
load_sessions <- function(sessions_file) {
  if (!file.exists(sessions_file)) {
    cli::cli_abort(c(
      "!" = "Sessions file not found: {.file {sessions_file}}",
      "i" = "Please check the file path."
    ))
  }

  file_ext <- tolower(tools::file_ext(sessions_file))
  if (file_ext == "csv") {
    sessions <- utils::read.csv(sessions_file)
  } else if (file_ext %in% c("xls", "xlsx")) {
    sessions <- readxl::read_excel(sessions_file) |>
      as.data.frame()
  } else if (file_ext %in% c("edf", "rec")) {
    sessions <- read_edf_sessions(sessions_file) |>
      dplyr::mutate(session_id = dplyr::row_number())
  } else {
    cli::cli_abort(c(
      "!" = "Unsupported file type: {.val {file_ext}}",
      "i" = "Please provide a CSV or Excel file."
    ))
  }

  sessions <- sessions |>
    dplyr::mutate(dplyr::across(dplyr::where(is.character), ~dplyr::na_if(., "")))

  if (nrow(sessions) == 0) {
    cli::cli_warn(c(
      "!" = "Sessions table is empty",
      "i" = "Returning NULL"
    ))
    return(NULL)
  }

  fmt <- get_sessions_format(sessions)
  sessions <- set_data_type(sessions, fmt)

  if (fmt == "ggir") {
    sessions |> clean_ggir_sessions()
  } else if (fmt %in% c("somnofy_v1", "somnofy_v2", "edf")) {
    sessions |> group_sessions_by_night()
  } else {
    sessions |>
      dplyr::mutate(session_id = dplyr::row_number())
  }
}

#' Load epoch data
#'
#' @param epochs_file The path to the epochs file
#' @returns A dataframe containing the epoch data
#' @details The function loads the epoch data from a file and groups the epochs by night.
#' Supported formats: CSV, Excel, EDF.
#' @export
#' @family data loading
#' @importFrom rlang .data
load_epochs <- function(epochs_file, timestamp = "timestamp", annotation = "annotation") {
  if (!file.exists(epochs_file)) {
    cli::cli_abort(c(
      "!" = "Epochs file not found: {.file {epochs_file}}",
      "i" = "Please check the file path."
    ))
  }

  file_ext <- tolower(tools::file_ext(epochs_file))
  if (file_ext == "csv") {
    epochs <- utils::read.csv(epochs_file)
  } else if (file_ext %in% c("xls", "xlsx")) {
    epochs <- readxl::read_excel(epochs_file) |>
      as.data.frame()
  } else if (file_ext %in% c("edf", "rec")) {
    epochs <- read_edf_epochs(epochs_file, timestamp, annotation)
  } else {
    cli::cli_abort(c(
      "!" = "Unsupported file type: {.val {file_ext}}",
      "i" = "Please provide a CSV, Excel or EDF file."
    ))
  }

  epochs <- epochs |>
    dplyr::mutate(dplyr::across(dplyr::where(is.character), ~dplyr::na_if(., "")))

  if (nrow(epochs) == 0) {
    cli::cli_warn(c(
      "!" = "Epochs table is empty",
      "i" = "Returning NULL"
    ))
    return(NULL)
  }

  fmt <- get_epochs_format(epochs)
  epochs <- set_data_type(epochs, fmt)
  epochs <- clean_epochs(epochs)

  if (fmt == "somnofy_v1") {
    epochs <- epochs |>
      dplyr::mutate(session_id = stringr::str_extract(basename(epochs_file), "^[^.]+"))
  }

  if (fmt %in% c("somnofy_v1", "somnofy_v2", "ggir")) {
    epochs |>
      group_epochs_by_night()
  } else {
    epochs
  }
}

#' Load session data in batch mode
#'
#' @param folder_path The path to the folder containing session files
#' @param pattern An optional pattern to filter files in the folder
#' @returns A dataframe containing the combined session data from all files in the folder
#' @family data loading
#' @export
load_sessions_batch <- function(folder_path, pattern = "") {
  if (!dir.exists(folder_path)) {
    cli::cli_abort(c(
      "!" = "Folder not found: {.file {folder_path}}",
      "i" = "Please check the folder path."
    ))
  }

  all_files <- list.files(folder_path, full.names = TRUE)

  if (pattern != "") {
    all_files <- all_files[grepl(pattern, basename(all_files))]
  }

  if (length(all_files) == 0) {
    cli::cli_warn(c(
      "!" = "No files found in folder: {.file {folder_path}} with pattern: {.val {pattern}}",
      "i" = "Returning NULL"
    ))
    return(NULL)
  }

  all_sessions <- data.frame()
  for (f in all_files) {
    sessions <- load_sessions(f)
    if (!is.null(sessions)) {
      sessions <- sessions
        # dplyr::mutate(dplyr::across(dplyr::everything(), as.character))
      all_sessions <- dplyr::bind_rows(all_sessions, sessions)
    }
  }
  all_sessions
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
  if (!data_type %in% c("somnofy_v1", "somnofy_v2", "ggir", "edf", "none")) {
    cli::cli_abort(c(
      "!" = "Invalid data type: {.val {data_type}}",
      "i" = "Available data types: somnofy_v1, somnofy_v2, ggir, edf."
    ))
  }
  df$.data_type <- data_type
  df
}

get_sessions_format <- function(sessions) {
  if (all(c(
    "id", "session_start", "session_end", "time_at_sleep_onset", "time_at_wakeup"
  ) %in% colnames(sessions))) {
    "somnofy_v2"
  } else if (all(c(
    "session_id", "session_start", "session_end", "time_at_sleep_onset", "time_at_wakeup"
  ) %in% colnames(sessions))) {
    "somnofy_v1"
  } else if (all(c(
    "ID", "sleeponset_ts", "wakeup_ts", "dur_spt_sleep_min", "daytype", "GGIRversion"
  ) %in% colnames(sessions))) {
    "ggir"
  } else if (all(c(
    "startTime", "endTime", "sleep_period"
  ) %in% colnames(sessions))) {
    "edf"
  } else {
    cli::cli_warn(c(
      "!" = "Could not infer the Sessions data type.",
      "i" = "Setting all column names to NULL."
    ))
    "none"
  }
}

get_epochs_format <- function(epochs) {
  if (all(c("timenum", "window", "SleepPeriodTime") %in% colnames(epochs))) {
    "ggir"
  } else if (all(c("timestamp", "session_id", "sleep_stage") %in% colnames(epochs))) {
    "somnofy_v2"
  } else if (all(c("timestamp", "sleep_stage") %in% colnames(epochs))) {
    "somnofy_v1"
  } else {
    cli::cli_warn(c(
      "!" = "Could not infer the Epochs data type.",
      "i" = "Setting all column names to NULL."
    ))
    "none"
  }
}
