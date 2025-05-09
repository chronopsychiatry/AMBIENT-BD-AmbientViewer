get_session_colnames <- function(sessions, col_names) {
  calling_function <- as.character(sys.call(-1)[1])
  if (!".data_type" %in% colnames(sessions)) {
    cli::cli_abort(c(
      "!" = "The sessions data frame does not have a .data_type column.",
      "i" = paste0("Please load sessions with `load_sessions`, or pass column names as arguments to `",
                   calling_function, "`.")
    ))
  }

  default_list <- get(paste0(".sessions_col_", sessions$.data_type[1]))

  if (is.null(col_names)) {
    return(default_list)
  }

  result <- setNames(vector("list", length(col_names)), col_names)

  for (name in names(col_names)) {
    val <- col_names[[name]]
    if (is.null(val)) {
      val <- default_list[[name]]
    }
    if (!val %in% colnames(sessions)) {
      cli::cli_abort(c(
        "!" = "Column {val} not found in the sessions data frame."
      ))
    }
    result[[name]] <- val
  }

  result
}

get_epoch_colnames <- function(epochs, col_names) {
  calling_function <- as.character(sys.call(-1)[1])
  if (!".data_type" %in% colnames(epochs)) {
    cli::cli_abort(c(
      "!" = "The epochs data frame does not have a .data_type column.",
      "i" = paste0("Please load epochs with `load_epochs`, or pass column names as arguments to `",
                   calling_function, "`.")
    ))
  }

  default_list <- get(paste0(".epochs_col_", epochs$.data_type[1]))

  if (is.null(col_names)) {
    return(default_list)
  }

  result <- setNames(vector("list", length(col_names)), col_names)

  for (name in names(col_names)) {
    val <- col_names[[name]]
    if (is.null(val)) {
      val <- default_list[[name]]
    }
    if (!val %in% colnames(epochs)) {
      cli::cli_abort(c(
        "!" = "Column {val} not found in the epochs data frame."
      ))
    }
    result[[name]] <- val
  }

  result
}
