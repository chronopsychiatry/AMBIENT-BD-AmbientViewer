#' Plot epoch time series data for a given variable
#'
#' @param epochs The epochs dataframe
#' @param variable The variable to plot (e.g., "temperature_ambient_mean")
#' @param exclude_zero Logical, whether to exclude zero values from the plot (default: FALSE)
#' @returns A ggplot object
#' @export
#' @family epoch plots
#' @seealso [plot_timeseries_sessions()] to plot session data.
plot_timeseries <- function(epochs, variable, exclude_zero = FALSE) {
  if (exclude_zero) {
    epochs <- epochs |>
      dplyr::filter(.data[[variable]] != 0)
  }

  p <- ggplot2::ggplot(
    epochs,
    ggplot2::aes(
      x = adjusted_time,
      y = .data[[variable]],
      color = as.factor(night),
      group = night
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

  return(p)
}

#' Plot session time series data for a given variable
#'
#' @param sessions The sessions dataframe
#' @param variable The variable to plot (e.g., "time_at_sleep_onset")
#' @param exclude_zero Logical, whether to exclude zero values from the plot (default: FALSE)
#' @returns A ggplot object
#' @export
#' @family session plots
#' @seealso [plot_timeseries()] to plot epoch data.
plot_timeseries_sessions <- function(sessions, variable, exclude_zero = FALSE) {
  sessions <- sessions |>
    dplyr::filter(!is.na(.data[[variable]]) & .data[[variable]] != "")

  if (exclude_zero) {
    sessions <- sessions |>
      dplyr::filter(.data[[variable]] != 0)
  }

  if (is_iso8601_datetime(sessions[[variable]])) {
    sessions <- sessions |>
      dplyr::mutate(
        plot_var = lubridate::hour(as.POSIXct(.data[[variable]], format = "%Y-%m-%dT%H:%M:%OS", tz = "UTC")) +
          lubridate::minute(as.POSIXct(.data[[variable]], format = "%Y-%m-%dT%H:%M:%OS", tz = "UTC")) / 60
      )
  } else {
    sessions$plot_var <- sessions[[variable]]
  }

  p <- ggplot2::ggplot(
    sessions,
    ggplot2::aes(
      x = night,
      y = plot_var
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

  return(p)
}
