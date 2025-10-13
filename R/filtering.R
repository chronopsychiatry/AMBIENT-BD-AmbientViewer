#' Filter epochs based on session IDs
#'
#' @param epochs The epochs dataframe
#' @param sessions The sessions dataframe
#' @param session_col_names A list to override default session column names. This function uses columns:
#' - `session_start`
#' - `session_end`
#' @param epoch_col_names A list to override default epoch column names. This function uses columns:
#' - `timestamp`
#' @param return_mask If TRUE, returns a logical vector indicating which epochs belong to the specified sessions
#' @returns The epochs dataframe with only the epochs that belong to the specified sessions, or a logical vector if `return_mask` is TRUE
#' @export
#' @examples
#' # Apply filtering to sessions to keep specific nights, and filter epochs accordingly
#' filtered_sessions <- filter_by_night_range(example_sessions, "2025-04-07", "2025-04-10")
#' filtered_epochs <- filter_epochs_from_sessions(example_epochs, filtered_sessions)
#'
#' @seealso [filter_by_night_range()] to filter sessions by night range.
#' @family filtering
filter_epochs_from_sessions <- function(epochs, sessions, session_col_names = NULL, epoch_col_names = NULL, return_mask = FALSE) {
  scol <- get_session_colnames(sessions, session_col_names)
  ecol <- get_epoch_colnames(epochs, epoch_col_names)

  if (is.null(scol$session_start) || is.null(scol$session_end) || is.null(ecol$timestamp)) {
    mask <- rep(FALSE, nrow(epochs))
    if (return_mask) {
      return(mask)
    } else {
      return(epochs[mask, ])
    }
  }

  if (sum(epochs[[ecol$session_id]] %in% unique(sessions[[scol$id]])) == 0) {
    cli::cli_warn(c("!" = "None of the epochs match the selected sessions.",
                    "i" = "Returning an empty epoch table."))
  }

  mask <- vapply(epochs[[ecol$timestamp]], function(et) {
    any(et >= sessions[[scol$session_start]] & et <= sessions[[scol$session_end]])
  }, logical(1))

  if (return_mask) {
    mask
  } else {
    epochs[mask, ]
  }
}

#' Filter sessions for nights within a night range
#'
#' @param sessions The sessions dataframe
#' @param from_night The start night of the range (inclusive) in YYYY-MM-DD format
#' @param to_night The end night of the range (inclusive) in YYYY-MM-DD format
#' @param col_names A list to override default column names. This function uses columns:
#' - `night`
#' @param return_mask If TRUE, returns a logical vector indicating which sessions meet the night range requirement
#' @returns The sessions dataframe with only the sessions that fall within the specified night range, or a logical vector if `return_mask` is TRUE
#' @importFrom rlang .data
#' @export
#' @family filtering
#' @examples
#' filtered_sessions <- filter_by_night_range(example_sessions, "2025-04-07", "2025-04-10")
filter_by_night_range <- function(sessions, from_night, to_night, col_names = NULL, return_mask = FALSE) {
  col <- get_session_colnames(sessions, col_names)

  if (nrow(sessions) == 0) {
    return(sessions)
  }

  from_night <- if (is.null(from_night)) min(sessions[[col$night]]) else from_night
  to_night <- if (is.null(to_night)) max(sessions[[col$night]]) else to_night

  if (from_night > to_night) {
    cli::cli_abort(c("!" = "from_night must be before to_night."))
  }

  mask <- (sessions[[col$night]] >= lubridate::as_date(from_night) &
             sessions[[col$night]] <= lubridate::as_date(to_night))

  if (return_mask) {
    sessions
  } else {
    sessions[mask, ]
  }
}

#' Filter sessions by age range
#'
#' @param sessions The sessions dataframe
#' @param min_age The minimum age of the subjects (inclusive)
#' @param max_age The maximum age of the subjects (inclusive)
#' @param col_names A list to override default column names. This function uses columns:
#' - `birth_year`
#' @param return_mask If TRUE, returns a logical vector indicating which sessions belong to subjects within the specified age range
#' @returns The sessions dataframe with only the sessions that belong to subjects within the specified age range,
#' or a logical vector if `return_mask` is TRUE
#' @importFrom rlang .data
#' @export
#' @family filtering
#' @examples
#' filtered_sessions <- filter_by_age_range(example_sessions_v1, min_age = 11, max_age = 18)
filter_by_age_range <- function(sessions, min_age = NULL, max_age = NULL, col_names = NULL, return_mask = FALSE) {
  col <- get_session_colnames(sessions, col_names)

  if (nrow(sessions) == 0) {
    return(sessions)
  }

  if (is.null(col$birth_year)) {
    cli::cli_abort(c("!" = "The sessions table does not contain a birth year column."))
  }

  min_age <- if (is.null(min_age)) min(lubridate::year(sessions[[col$session_start]]) - sessions[[col$birth_year]]) else min_age
  max_age <- if (is.null(max_age)) max(lubridate::year(sessions[[col$session_start]]) - sessions[[col$birth_year]]) else max_age

  if (min_age > max_age) {
    cli::cli_abort(c("!" = "min_age must be before max_age."))
  }

  mask <- (sessions[[col$birth_year]] >= (lubridate::year(sessions[[col$session_start]]) - max_age) &
             sessions[[col$birth_year]] <= (lubridate::year(sessions[[col$session_start]]) - min_age))

  if (return_mask) {
    sessions
  } else {
    sessions[mask, ]
  }
}

#' Filter by sex
#'
#' @param sessions The sessions dataframe
#' @param sex The sex to filter for (M, F, or NULL for both)
#' @param col_names A list to override default column names. This function uses columns:
#' - `sex`
#' @param return_mask If TRUE, returns a logical vector indicating which sessions belong to the specified sex
#' @returns The sessions dataframe with only the sessions that belong to the specified sex, or a logical vector if `return_mask` is TRUE
#' @importFrom rlang .data
#' @export
#' @family filtering
#' @examples
#' filtered_sessions <- filter_by_sex(example_sessions_v1, "M")
filter_by_sex <- function(sessions, sex, col_names = NULL, return_mask = FALSE) {
  col <- get_session_colnames(sessions, col_names)

  if (nrow(sessions) == 0 || is.null(sex)) {
    return(sessions)
  }

  if (is.null(col$sex)) {
    cli::cli_abort(c("!" = "The sessions table must contain a Sex column."))
  }

  mask <- sessions[[col$sex]] %in% sex

  if (return_mask) {
    sessions
  } else {
    sessions[mask, ]
  }
}

#' Select subjects by ID
#'
#' @param sessions The sessions dataframe
#' @param subject_ids The subject IDs to select
#' @param col_names A list to override default column names. This function uses columns:
#' - `subject_id`
#' @param return_mask If TRUE, returns a logical vector indicating which sessions belong to the specified subjects
#' @returns The sessions dataframe with only the sessions that belong to the specified subjects, or a logical vector if `return_mask` is TRUE
#' @export
#' @family filtering
#' @seealso [select_devices()] to select sessions by device ID.
#' @examples
#' filtered_sessions <- select_subjects(example_sessions, c("sub_01JNDH3Z5NP0PSV82NFBGPV31X"))
select_subjects <- function(sessions, subject_ids, col_names = NULL, return_mask = FALSE) {
  col <- get_session_colnames(sessions, col_names)

  if (nrow(sessions) == 0 || is.null(subject_ids)) {
    return(sessions)
  }

  if (sum(sessions[[col$subject_id]] %in% subject_ids) == 0) {
    cli::cli_warn(c("!" = "None of the subject IDs were found in the sessions table.",
                    "i" = "Available subject IDs: {unique(sessions[[col$subject_id]])}",
                    "i" = "Returning an empty sessions table."))
  }

  mask <- sessions[[col$subject_id]] %in% subject_ids

  if (return_mask) {
    sessions
  } else {
    sessions[mask, ]
  }
}

#' Select devices by ID
#'
#' @param sessions The sessions dataframe
#' @param device_ids The device IDs to select
#' @param col_names A list to override default column names. This function uses columns:
#' - `device_id`
#' @param return_mask If TRUE, returns a logical vector indicating which sessions were recorded by the specified devices
#' @returns The sessions dataframe with only the sessions recorded by the specified devices, or a logical vector if `return_mask` is TRUE
#' @export
#' @family filtering
#' @seealso [select_subjects()] to select sessions by subject ID.
#' @examples
#' filtered_sessions <- select_devices(example_sessions, c("VTGVSRTHCA"))
select_devices <- function(sessions, device_ids, col_names = NULL, return_mask = FALSE) {
  col <- get_session_colnames(sessions, col_names)

  if (nrow(sessions) == 0 || is.null(device_ids)) {
    return(sessions)
  }

  if (sum(sessions[[col$device_id]] %in% device_ids) == 0) {
    cli::cli_warn(c("!" = "None of the device IDs were found in the sessions table.",
                    "i" = "Available device IDs: {unique(sessions[[col$device_id]])}",
                    "i" = "Returning an empty sessions table."))
  }

  mask <- sessions[[col$device_id]] %in% device_ids

  if (return_mask) {
    sessions
  } else {
    sessions[mask, ]
  }
}
