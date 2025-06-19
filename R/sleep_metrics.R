#' Calculate Interdaily Stability (IS)
#'
#' This function calculates the Interdaily Stability (IS) metric from a binary awake/asleep variable
#' @param epochs The epochs data frame
#' @param variable The name of the binary sleep variable, where 0 = awake and 1 = asleep
#' @param time_col The name of the column containing timestamps (default is "timestamp")
#' @return The Interdaily Stability (IS) value
#' @export
#' @family sleep metrics
#' @examples
#' interdaily_stability(example_epochs, "is_asleep")
#' @importFrom rlang .data
interdaily_stability <- function(epochs, variable, time_col = "timestamp") {
  values <- epochs[[variable]]
  unique_vals <- unique(stats::na.omit(values))
  if (!all(unique_vals %in% c(0, 1))) {
    cli::cli_abort(c(
      "!" = "Column '{variable}' must contain only binary values (0 and 1), where 0 = awake and 1 = asleep."
    ))
  }

  epochs <- epochs |>
    dplyr::mutate(time = parse_time(.data[[time_col]]),
                  tod = floor(time_to_hours(.data$time)),
                  day = as.Date(.data$time))

  mean_tod <- epochs |>
    dplyr::group_by(tod) |>
    dplyr::summarise(mean_val = mean(.data[[variable]], na.rm = TRUE), .groups = "drop")

  overall_mean <- mean(epochs[[variable]], na.rm = TRUE)

  n_tod <- table(epochs$tod)
  numerator <- sum((mean_tod$mean_val - overall_mean)^2 * as.numeric(n_tod))
  denominator <- sum((epochs[[variable]] - overall_mean)^2, na.rm = TRUE)

  # IS
  numerator / denominator
}

#' Calculate Social Jet Lag
#'
#' This function calculates the Social Jet Lag (SJL) metric as the difference in mid-sleep times
#' between workdays and free days.
#' @param sessions The sessions data frame
#' @param midsleep The name of the column containing mid-sleep times (default is "time_at_midsleep")
#' @param is_workday The name of the column indicating which days are workdays (default is "is_workday")
#' @return The Social Jet Lag (SJL) value in hours
#' @export
#' @family sleep metrics
#' @examples
#' social_jet_lag(example_sessions)
#' @importFrom rlang .data
social_jet_lag <- function(sessions, midsleep = "time_at_midsleep", is_workday = "is_workday") {
  sessions <- sessions |>
    dplyr::mutate(is_workday = as.logical(.data$is_workday)) |>
    dplyr::group_by(is_workday) |>
    dplyr::summarise(
      mean_midsleep = mean_time(.data[[midsleep]], unit = "hour"), .groups = "drop"
    )

  sessions$mean_midsleep[!sessions$is_workday] -
    sessions$mean_midsleep[sessions$is_workday]
}
