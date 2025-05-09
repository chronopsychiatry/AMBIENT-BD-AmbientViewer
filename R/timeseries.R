#' Plot epoch time series data for a given variable
#'
#' @param epochs The epochs dataframe
#' @param variable The variable to plot (e.g., "temperature_ambient_mean")
#' @param exclude_zero Logical, whether to exclude zero values from the plot (default: FALSE)
#' @param col_names A list to override default column names. This function uses columns:
#' - `timestamp`
#' - `night`
#' @returns A ggplot object
#' @importFrom rlang .data
#' @export
#' @family plot epochs
#' @seealso [plot_timeseries_sessions()] to plot session data.
plot_timeseries <- function(epochs, variable, exclude_zero = FALSE, col_names = NULL) {
  col <- get_epoch_colnames(epochs, col_names)

  if (exclude_zero) {
    epochs <- epochs |>
      dplyr::filter(.data[[variable]] != 0)
  }

  ggplot2::ggplot(
    epochs,
    ggplot2::aes(
      x = time_to_hours(shift_times_by_12h(.data[[col$timestamp]])),
      y = .data[[variable]],
      color = as.factor(.data[[col$night]]),
      group = .data[[col$night]]
    )
  ) +
    ggplot2::geom_point() +
    ggplot2::labs(
      x = "Time",
      y = variable,
      color = "Night of"
    ) +
    ggplot2::theme_minimal(base_size = 16) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
    ) +
    ggplot2::scale_x_continuous(
      limits = c(0, 24),
      breaks = seq(0, 24, by = 2),
      labels = function(x) sprintf("%02d:00", (x + 12) %% 24) # Format as HH:00
    )
}

#' Plot session time series data for a given variable
#'
#' @param sessions The sessions dataframe
#' @param variable The variable to plot (e.g., "time_at_sleep_onset")
#' @param exclude_zero Logical, whether to exclude zero values from the plot (default: FALSE)
#' @param col_names A list to override default column names. This function uses columns:
#' - `night`
#' @returns A ggplot object
#' @export
#' @family plot sessions
#' @seealso [plot_timeseries()] to plot epoch data.
plot_timeseries_sessions <- function(sessions, variable, exclude_zero = FALSE, col_names = NULL) {
  col <- get_session_colnames(sessions, col_names)

  sessions <- sessions |>
    dplyr::filter(!is.na(.data[[variable]]) & .data[[variable]] != "")

  if (exclude_zero) {
    sessions <- sessions |>
      dplyr::filter(.data[[variable]] != 0)
  }

  if (is_iso8601_datetime(sessions[[variable]])) {
    sessions <- sessions |>
      dplyr::mutate(plot_var = time_to_hours(.data[[variable]]))
  } else {
    sessions$plot_var <- sessions[[variable]]
  }

  p <- ggplot2::ggplot(
    sessions,
    ggplot2::aes(
      x = .data[[col$night]],
      y = .data$plot_var
    )
  ) +
    ggplot2::geom_point() +
    ggplot2::labs(
      x = NULL,
      y = variable
    ) +
    ggplot2::theme_minimal(base_size = 16) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
    )

  if (is_iso8601_datetime(sessions[[variable]])) {
    p <- p + ggplot2::scale_y_continuous(
      labels = function(x) sprintf("%02d:%02d", floor(x), round((x %% 1) * 60))
    )
  }
  p
}
