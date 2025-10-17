get_session_colnames <- function(sessions, col_names = NULL) {
  calling_function <- as.character(sys.call(-1)[1])
  if (!".data_type" %in% colnames(sessions) && is.null(col_names)) {
    cli::cli_abort(c(
      "!" = "The sessions data frame does not have a .data_type column.",
      "i" = paste0("Please load sessions with `load_sessions`, or pass column names as arguments to `",
                   calling_function, "`.")
    ))
  }

  if (nrow(sessions) == 0) {
    return("none")
  }

  if (".data_type" %in% colnames(sessions)) {
    fmt <- sessions$.data_type[1]
  } else {
    fmt <- "somnofy_v2"
  }

  default_list <- get(paste0(".sessions_col_", fmt))

  for (name in names(default_list)) {
    if (!default_list[name] %in% colnames(sessions)) {
      default_list[name] <- list(NULL)
    }
  }

  if (is.null(col_names)) {
    return(default_list)
  }

  override_col_names(sessions, col_names, default_list)
}

get_epoch_colnames <- function(epochs, col_names = NULL) {
  calling_function <- as.character(sys.call(-1)[1])
  if (!".data_type" %in% colnames(epochs) && is.null(col_names)) {
    cli::cli_abort(c(
      "!" = "The epochs data frame does not have a .data_type column.",
      "i" = paste0("Please load epochs with `load_epochs`, or pass column names as arguments to `",
                   calling_function, "`.")
    ))
  }

  # If the epochs data frame is empty, try to infer the format from the column names
  if (nrow(epochs) == 0) {
    fmt <- get_epochs_format(epochs)
    if (is.null(fmt)) {
      cli::cli_abort(c(
        "!" = "The epochs data frame is empty and data type could not be inferred.",
        "i" = "Check that your input data contains session data."
      ))
    } else {
      return(get(paste0(".epochs_col_", fmt)))
    }
  }

  if (".data_type" %in% colnames(epochs)) {
    fmt <- epochs$.data_type[1]
  } else {
    fmt <- "somnofy_v2"
  }

  default_list <- get(paste0(".epochs_col_", epochs$.data_type[1]))

  for (name in names(default_list)) {
    if (!default_list[name] %in% colnames(epochs)) {
      default_list[name] <- list(NULL)
    }
  }

  if (is.null(col_names)) {
    return(default_list)
  }

  override_col_names(epochs, col_names, default_list)
}

override_col_names <- function(data, col_names, default_list) {
  result <- stats::setNames(vector("list", length(col_names)), col_names)

  for (name in names(col_names)) {
    val <- col_names[[name]]
    if (is.null(val)) {
      val <- default_list[[name]]
    }
    result[[name]] <- val
  }

  result
}