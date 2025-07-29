#' Plot boxplots for sleep onset, midsleep, and wakeup times
#'
#' @param sessions The sessions dataframe
#' @param col_names A list to override default column names. This function uses columns:
#' - `time_at_sleep_onset`
#' - `time_at_wakeup`
#' - `time_at_midsleep`
#' @returns A ggplot object with three horizontal boxplots (onset, midsleep, wakeup)
#' @export
#' @importFrom rlang .data
sleeptimes_boxplot <- function(sessions, col_names = NULL) {
  plot_data <- prepare_sleeptimes_data(sessions, col_names)
  box_colors <- c("Sleep Onset" = "purple", "Midsleep" = "cornflowerblue", "Wakeup" = "orange")

  ggplot2::ggplot(plot_data, ggplot2::aes(y = .data$variable, x = .data$hour, color = .data$variable)) +
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

#' Plot histograms for sleep onset, midsleep, and wakeup times
#'
#' @param sessions The sessions dataframe
#' @param col_names A list to override default column names. This function uses columns:
#' - `time_at_sleep_onset`
#' - `time_at_wakeup`
#' - `time_at_midsleep`
#' @param binwidth The width of the bins for the histogram (default 0.25)
#' @returns A ggplot object with three overlaid histograms (sleep onset, midsleep, wakeup)
#' @export
#' @importFrom rlang .data
sleeptimes_histogram <- function(sessions, col_names = NULL, binwidth = 0.25) {
  plot_data <- prepare_sleeptimes_data(sessions, col_names)
  box_colors <- c("Sleep Onset" = "purple", "Midsleep" = "cornflowerblue", "Wakeup" = "orange")

  ggplot2::ggplot(plot_data, ggplot2::aes(x = .data$hour, color = .data$variable)) +
    ggplot2::geom_histogram(
      ggplot2::aes(y = ggplot2::after_stat(.data$count), fill = .data$variable),
      binwidth = binwidth,
      position = "identity",
      alpha = 0.3,
      linewidth = 0.5
    ) +
    ggplot2::scale_color_manual(values = box_colors, limits = names(box_colors), guide = "none") +
    ggplot2::scale_fill_manual(values = box_colors, limits = names(box_colors)) +
    ggplot2::scale_x_continuous(
      breaks = seq(0, 24, by = 2),
      labels = function(x) sprintf("%02d:00", (x + 12) %% 24),
      expand = ggplot2::expansion(mult = c(0.08, 0.08))
    ) +
    ggplot2::labs(
      title = NULL,
      x = NULL,
      y = "Count",
      fill = NULL
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, size = 18),
      axis.text = ggplot2::element_text(size = 14),
      axis.title = ggplot2::element_text(size = 16),
      legend.text = ggplot2::element_text(size = 14),
      legend.position = "right",
      aspect.ratio = 1 / 4
    )
}

#' Plot density curves for sleep onset, midsleep, and wakeup times with a dashed line showing the median
#'
#' @param sessions The sessions dataframe
#' @param col_names A list to override default column names. This function uses columns:
#' - `time_at_sleep_onset`
#' - `time_at_wakeup`
#' - `time_at_midsleep`
#' @param adjust The bandwidth adjustment for the density estimate (default 1)
#' @returns A ggplot object with three overlaid density curves (sleep onset, midsleep, wakeup)
#' @export
#' @importFrom rlang .data
sleeptimes_density <- function(sessions, col_names = NULL, adjust = 1) {
  plot_data <- prepare_sleeptimes_data(sessions, col_names)
  box_colors <- c("Sleep Onset" = "purple", "Midsleep" = "cornflowerblue", "Wakeup" = "orange")

  medians <- plot_data |>
    dplyr::group_by(.data$variable) |>
    dplyr::summarise(median_hour = stats::median(.data$hour), .groups = "drop")

  density_data <- plot_data |>
    dplyr::group_by(.data$variable) |>
    dplyr::group_modify(~{
      d <- stats::density(.x$hour, adjust = adjust)
      tibble::tibble(hour = d$x, density = d$y)
    }) |>
    dplyr::ungroup()

  median_lines <- density_data |>
    dplyr::left_join(medians, by = "variable") |>
    dplyr::group_by(.data$variable) |>
    dplyr::filter(abs(.data$hour - .data$median_hour) == min(abs(.data$hour - .data$median_hour))) |>
    dplyr::slice(1) |>
    dplyr::ungroup() |>
    dplyr::select("variable", "median_hour", density_at_median = "density")

  ggplot2::ggplot(plot_data, ggplot2::aes(x = .data$hour, color = .data$variable, fill = .data$variable)) +
    ggplot2::geom_density(
      alpha = 0.3,
      adjust = adjust,
      linewidth = 0
    ) +
    ggplot2::geom_segment(
      data = median_lines,
      ggplot2::aes(
        x = .data$median_hour, xend = .data$median_hour,
        y = 0, yend = .data$density_at_median,
        color = .data$variable
      ),
      linetype = "dashed",
      linewidth = 0.7,
      alpha = 0.7,
      inherit.aes = FALSE
    ) +
    ggplot2::scale_color_manual(values = box_colors, limits = names(box_colors), guide = "none") +
    ggplot2::scale_fill_manual(values = box_colors, limits = names(box_colors)) +
    ggplot2::scale_x_continuous(
      limits = range(density_data$hour),
      breaks = seq(0, 24, by = 2),
      labels = function(x) sprintf("%02d:00", (x + 12) %% 24),
      expand = ggplot2::expansion(mult = c(0.08, 0.08))
    ) +
    ggplot2::labs(
      title = NULL,
      x = NULL,
      y = "Density",
      fill = NULL
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, size = 18),
      axis.text = ggplot2::element_text(size = 14),
      axis.title = ggplot2::element_text(size = 16),
      legend.text = ggplot2::element_text(size = 14),
      legend.position = "right",
      aspect.ratio = 1 / 4
    )
}

#' @importFrom rlang .data
prepare_sleeptimes_data <- function(sessions, col_names = NULL) {
  col <- get_session_colnames(sessions, col_names)
  sessions <- remove_sessions_no_sleep(sessions)
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
