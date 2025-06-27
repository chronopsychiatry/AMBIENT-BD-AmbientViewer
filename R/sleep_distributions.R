utils::globalVariables(c("variable", "hour", "count"))

#' Plot boxplots for sleep onset, midsleep, and wakeup times (horizontal, colored outline, filled dot outliers)
#'
#' @param sessions The sessions dataframe
#' @param col_names A list to override default column names. This function uses columns:
#' - `time_at_sleep_onset`
#' - `time_at_wakeup`
#' - `time_at_midsleep`
#' @returns A ggplot object with three horizontal boxplots (onset, midsleep, wakeup)
#' @export
sleeptimes_boxplot <- function(sessions, col_names = NULL) {
  col <- get_session_colnames(sessions, col_names)

  plot_data <- tidyr::pivot_longer(
    sessions,
    cols = dplyr::all_of(c(col$time_at_sleep_onset, col$time_at_midsleep, col$time_at_wakeup)),
    names_to = "variable",
    values_to = "time"
  ) |>
    dplyr::mutate(
      time = parse_time(.data$time),
      variable = dplyr::case_when(
        .data$variable == col$time_at_sleep_onset ~ "Sleep Onset",
        .data$variable == col$time_at_midsleep ~ "Midsleep",
        .data$variable == col$time_at_wakeup ~ "Wakeup"
      ),
      hour = time_to_hours(shift_times_by_12h(.data$time))
    )

  box_colors <- c("Sleep Onset" = "purple", "Midsleep" = "cornflowerblue", "Wakeup" = "orange")
  plot_data$variable <- factor(plot_data$variable, levels = c("Wakeup", "Midsleep", "Sleep Onset"))

  ggplot2::ggplot(plot_data, ggplot2::aes(y = variable, x = hour, color = variable)) +
    ggplot2::geom_boxplot(
      fill = "white",
      outlier.shape = 16,
      outlier.size = 2.5,
      outlier.stroke = 0.7,
      size = 1.2,
      staplewidth = 0.5
    ) +
    ggplot2::scale_color_manual(values = box_colors, guide = "none") +
    ggplot2::scale_x_continuous(
      breaks = seq(0, 24, by = 2),
      labels = function(x) sprintf("%02d:00", (x + 12) %% 24),
      expand = ggplot2::expansion(mult = c(0.08, 0.08))
    ) +
    ggplot2::labs(
      title = NULL,
      x = NULL,
      y = NULL
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, size = 18),
      axis.text = ggplot2::element_text(size = 14),
      axis.title = ggplot2::element_text(size = 16),
      aspect.ratio = 1 / 4
    )
}

#' Plot histograms for sleep onset, midsleep, and wakeup times (overlaid, colored fill and outline)
#'
#' @param sessions The sessions dataframe
#' @param col_names A list to override default column names. This function uses columns:
#' - `time_at_sleep_onset`
#' - `time_at_wakeup`
#' - `time_at_midsleep`
#' @param binwidth The width of the bins for the histogram
#' @returns A ggplot object with three overlaid histograms (sleep onset, midsleep, wakeup)
#' @export
sleeptimes_histogram <- function(sessions, col_names = NULL, binwidth = 0.25) {
  plot_data <- prepare_sleeptimes_data(sessions, col_names)
  box_colors <- c("Sleep Onset" = "purple", "Midsleep" = "cornflowerblue", "Wakeup" = "orange")

  ggplot2::ggplot(plot_data, ggplot2::aes(x = hour, color = variable)) +
    ggplot2::geom_histogram(
      ggplot2::aes(y = ggplot2::after_stat(count), fill = variable),
      binwidth = binwidth,
      position = "identity",
      alpha = 0.3,
      linewidth = 0.5
    ) +
    ggplot2::scale_color_manual(values = box_colors, guide = "none") +
    ggplot2::scale_fill_manual(values = box_colors, guide = "none") +
    ggplot2::scale_x_continuous(
      breaks = seq(0, 24, by = 2),
      labels = function(x) sprintf("%02d:00", (x + 12) %% 24),
      expand = ggplot2::expansion(mult = c(0.08, 0.08))
    ) +
    ggplot2::labs(
      title = NULL,
      x = NULL,
      y = "Count"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, size = 18),
      axis.text = ggplot2::element_text(size = 14),
      axis.title = ggplot2::element_text(size = 16),
      aspect.ratio = 1 / 4
    )
}

#' Plot density curves for sleep onset, midsleep, and wakeup times (overlaid, colored fill and outline)
#'
#' @param sessions The sessions dataframe
#' @param col_names A list to override default column names. This function uses columns:
#' - `time_at_sleep_onset`
#' - `time_at_wakeup`
#' - `time_at_midsleep`
#' @param adjust The bandwidth adjustment for the density estimate (default 1)
#' @returns A ggplot object with three overlaid density curves (sleep onset, midsleep, wakeup)
#' @export
sleeptimes_density <- function(sessions, col_names = NULL, adjust = 1) {
  plot_data <- prepare_sleeptimes_data(sessions, col_names)
  box_colors <- c("Sleep Onset" = "purple", "Midsleep" = "cornflowerblue", "Wakeup" = "orange")

  ggplot2::ggplot(plot_data, ggplot2::aes(x = hour, color = variable, fill = variable)) +
    ggplot2::geom_density(
      alpha = 0.5,
      adjust = adjust,
      linewidth = 0
    ) +
    ggplot2::scale_color_manual(values = box_colors, guide = "none") +
    ggplot2::scale_fill_manual(values = box_colors, guide = "none") +
    ggplot2::scale_x_continuous(
      breaks = seq(0, 24, by = 2),
      labels = function(x) sprintf("%02d:00", (x + 12) %% 24),
      expand = ggplot2::expansion(mult = c(0.08, 0.08))
    ) +
    ggplot2::labs(
      title = NULL,
      x = NULL,
      y = "Density"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, size = 18),
      axis.text = ggplot2::element_text(size = 14),
      axis.title = ggplot2::element_text(size = 16),
      aspect.ratio = 1 / 4
    )
}

prepare_sleeptimes_data <- function(sessions, col_names = NULL) {
  col <- get_session_colnames(sessions, col_names)
  plot_data <- tidyr::pivot_longer(
    sessions,
    cols = dplyr::all_of(c(col$time_at_sleep_onset, col$time_at_midsleep, col$time_at_wakeup)),
    names_to = "variable",
    values_to = "time"
  ) |>
    dplyr::mutate(
      time = parse_time(.data$time),
      variable = dplyr::case_when(
        .data$variable == col$time_at_sleep_onset ~ "Sleep Onset",
        .data$variable == col$time_at_midsleep ~ "Midsleep",
        .data$variable == col$time_at_wakeup ~ "Wakeup"
      ),
      hour = time_to_hours(shift_times_by_12h(.data$time))
    )
  plot_data$variable <- factor(plot_data$variable, levels = c("Wakeup", "Midsleep", "Sleep Onset"))
  plot_data
}
