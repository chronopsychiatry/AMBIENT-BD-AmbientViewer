#' Filter epochs based on session IDs
#'
#' @param epochs The epochs dataframe
#' @param sessions The sessions dataframe
#' @param session_col_names A list to override default session column names. This function uses columns:
#' - `id`
#' @param epoch_col_names A list to override default epoch column names. This function uses columns:
#' - `session_id`
#' @returns The epochs dataframe with only the epochs that belong to the specified sessions
#' @export
#' @examples
#' # Apply filtering to sessions to keep specific nights, and filter epochs accordingly
#' filtered_sessions <- filter_by_night_range(example_sessions, "2025-04-07", "2025-04-10")#
#' filtered_epochs <- filter_epochs_from_sessions(example_epochs, filtered_sessions)
#'
#' @seealso [filter_by_night_range()] to filter sessions by night range.
#' @family filtering
filter_epochs_from_sessions <- function(epochs, sessions, session_col_names = NULL, epoch_col_names = NULL) {
  scol <- get_session_colnames(sessions, session_col_names)
  ecol <- get_epoch_colnames(epochs, epoch_col_names)

  if (sum(epochs[[ecol$session_id]] %in% unique(sessions[[scol$id]])) == 0) {
    cli::cli_warn(c("!" = "None of the epochs match the selected sessions.",
                    "i" = "Returning an empty epoch table."))
  }
  epochs[epochs[[ecol$session_id]] %in% unique(sessions[[scol$id]]), ]
}

#' Filter sessions for nights within a night range
#'
#' @param sessions The sessions dataframe
#' @param from_night The start night of the range (inclusive) in YYYY-MM-DD format
#' @param to_night The end night of the range (inclusive) in YYYY-MM-DD format
#' @param col_names A list to override default column names. This function uses columns:
#' - `night`
#' @returns The sessions dataframe with only the sessions that fall within the specified night range
#' @importFrom rlang .data
#' @export
#' @family filtering
#' @examples
#' filtered_sessions <- filter_by_night_range(example_sessions, "2025-04-07", "2025-04-10")
filter_by_night_range <- function(sessions, from_night, to_night, col_names = NULL) {
  col <- get_session_colnames(sessions, col_names)

  if (nrow(sessions) == 0) {
    return(sessions)
  }

  from_night <- if (is.null(from_night)) min(sessions[[col$night]]) else from_night
  to_night <- if (is.null(to_night)) min(sessions[[col$night]]) else to_night

  if (from_night > to_night) {
    cli::cli_abort(c("!" = "from_night must be before to_night."))
  }

  sessions |>
    dplyr::filter(.data[[col$night]] >= lubridate::as_date(from_night) &
                    .data[[col$night]] <= lubridate::as_date(to_night))
}

#' Filter sessions by age range
#'
#' @param sessions The sessions dataframe
#' @param min_age The minimum age of the subjects (inclusive)
#' @param max_age The maximum age of the subjects (inclusive)
#' @param col_names A list to override default column names. This function uses columns:
#' - `birth_year`
#' @returns The sessions dataframe with only the sessions that belong to subjects within the specified age range
#' @importFrom rlang .data
#' @export
#' @family filtering
#' @examples
#' filtered_sessions <- filter_by_age_range(example_sessions, min_age = 11, max_age = 18)
filter_by_age_range <- function(sessions, min_age, max_age, col_names = NULL) {
  col <- get_session_colnames(sessions, col_names)

  if (nrow(sessions) == 0) {
    return(sessions)
  }

  min_age <- if (is.null(min_age)) lubridate::year(Sys.Date()) - max(sessions[[col$birth_year]]) else min_age
  max_age <- if (is.null(max_age)) lubridate::year(Sys.Date()) - min(sessions[[col$birth_year]]) else max_age

  if (is.null(col$birth_year)) {
    cli::cli_abort(c("!" = "The sessions table does not contain a birth year column."))
  }
  if (min_age > max_age) {
    cli::cli_abort(c("!" = "min_age must be before max_age."))
  }

  min_birth_year <- lubridate::year(Sys.Date()) - max_age
  max_birth_year <- lubridate::year(Sys.Date()) - min_age
  sessions |>
    dplyr::filter(.data[[col$birth_year]] >= min_birth_year &
                    .data[[col$birth_year]] <= max_birth_year)
}

#' Filter by sex
#'
#' @param sessions The sessions dataframe
#' @param sex The sex to filter for (M, F, or NULL for both)
#' @param col_names A list to override default column names. This function uses columns:
#' - `sex`
#' @returns The sessions dataframe with only the sessions that belong to the specified sex
#' @importFrom rlang .data
#' @export
#' @family filtering
#' @examples
#' filtered_sessions <- filter_by_sex(example_sessions, "M")
filter_by_sex <- function(sessions, sex, col_names = NULL) {
  col <- get_session_colnames(sessions, col_names)

  if (nrow(sessions) == 0 || is.null(sex)) {
    return(sessions)
  }

  if (is.null(col$sex)) {
    cli::cli_abort(c("!" = "The sessions table must contain a Sex column."))
  }

  sessions[sessions[[col$sex]] %in% sex, ]
}

#' Select subjects by ID
#'
#' @param sessions The sessions dataframe
#' @param subject_ids The subject IDs to select
#' @param col_names A list to override default column names. This function uses columns:
#' - `subject_id`
#' @returns The sessions dataframe with only the sessions that belong to the specified subjects
#' @export
#' @family filtering
#' @seealso [select_devices()] to select sessions by device ID.
select_subjects <- function(sessions, subject_ids, col_names = NULL) {
  col <- get_session_colnames(sessions, col_names)

  if (nrow(sessions) == 0 || is.null(subject_ids)) {
    return(sessions)
  }

  if (sum(sessions[[col$subject_id]] %in% subject_ids) == 0) {
    cli::cli_warn(c("!" = "None of the subject IDs were found in the sessions table.",
                    "i" = "Available subject IDs: {unique(sessions[[col$subject_id]])}",
                    "i" = "Returning an empty sessions table."))
  }
  sessions[sessions[[col$subject_id]] %in% subject_ids, ]
}

#' Select devices by ID
#'
#' @param sessions The sessions dataframe
#' @param device_ids The device IDs to select
#' @param col_names A list to override default column names. This function uses columns:
#' - `device_id`
#' @returns The sessions dataframe with only the sessions recorded by the specified devices
#' @export
#' @family filtering
#' @seealso [select_subjects()] to select sessions by subject ID.
select_devices <- function(sessions, device_ids, col_names = NULL) {
  col <- get_session_colnames(sessions, col_names)

  if (nrow(sessions) == 0 || is.null(device_ids)) {
    return(sessions)
  }

  if (sum(sessions[[col$device_id]] %in% device_ids) == 0) {
    cli::cli_warn(c("!" = "None of the device IDs were found in the sessions table.",
                    "i" = "Available device IDs: {unique(sessions[[col$device_id]])}",
                    "i" = "Returning an empty sessions table."))
  }
  sessions[sessions[[col$device_id]] %in% device_ids, ]
}
