get_session_colnames <- function(sessions, col_names = NULL) {
  if (is.null(col_names)) {
    col_names <- .sessions_col_none
  }

  for (col in names(col_names)) {
    if (is.null(col_names[[col]])) {
      for (name in .sessions_col_presets[[col]]) {
        if (name %in% colnames(sessions)) {
          col_names[[col]] <- name
          break
        }
      }
    }
  }
  col_names
}

get_epoch_colnames <- function(epochs, col_names = NULL) {
  if (is.null(col_names)) {
    col_names <- .epochs_col_none
  }

  for (col in names(col_names)) {
    if (is.null(col_names[[col]])) {
      for (name in .epochs_col_presets[[col]]) {
        if (name %in% colnames(epochs)) {
          col_names[[col]] <- name
          break
        }
      }
    }
  }
  col_names
}
