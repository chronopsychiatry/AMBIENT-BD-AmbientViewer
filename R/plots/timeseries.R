#' Plot time series data for a given variable
#'
#' @param epochs The epochs dataframe
#' @param variable The variable to plot (e.g., "temperature_ambient_mean")
#' @param exclude_zero Logical, whether to exclude zero values from the plot (default: FALSE)
#' @returns A ggplot object
#' @export
#' @examples
#' plot <- plot_timeseries(epochs, "temperature_ambient_mean", exclude_zero = TRUE)
plot_timeseries <- function(epochs, variable, exclude_zero = FALSE) {
  if (exclude_zero) {
    epochs <- epochs %>%
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
