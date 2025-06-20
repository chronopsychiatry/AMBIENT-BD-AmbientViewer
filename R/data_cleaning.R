#' @importFrom rlang .data
clean_ggir_sessions <- function(sessions) {
  sessions |>
    dplyr::mutate(
      calendar_date = lubridate::as_date(.data$calendar_date),
      session_start = sub("-.*$", "", .data$start_end_window) |>
        parse_time() |>
        update_date(.data$calendar_date),
      session_end = sub("^[^-]*-", "", .data$start_end_window) |>
        parse_time() |>
        update_date(.data$calendar_date) + lubridate::days(1),
      sleeponset_ts = parse_time(.data$sleeponset_ts),
      wakeup_ts = parse_time(.data$wakeup_ts),
      sleep_period = .data$dur_spt_sleep_min * 60,
      time_in_bed = time_diff(.data$sleeponset_ts, .data$wakeup_ts, unit = "second"),
      is_workday = ifelse(.data$daytype == "WD", TRUE, FALSE)
    ) |>
    dplyr::rowwise() |>
    dplyr::mutate(midsleep_ts = mean_time(c(.data$sleeponset_ts, .data$wakeup_ts))) |>
    dplyr::ungroup()
}

#' @importFrom rlang .data
clean_epochs <- function(epochs) {
  fmt <- epochs$.data_type[1]
  col <- get_epoch_colnames(epochs)

  if (fmt == "ggir") {
    epochs |>
      dplyr::mutate(
        timenum = as.POSIXct(.data[[col$timestamp]], origin = "1970-01-01", tz = "Europe/London"),
        is_asleep = dplyr::if_else(.data[[col$sleep_stage]] == 0, 1, 0) # is_asleep: 0 = awake, 1 = asleep
      )
  } else if (fmt %in% c("somnofy_v1", "somnofy_v2")) {
    epochs |>
      dplyr::mutate(is_asleep = dplyr::if_else(.data[[col$sleep_stage]] %in% c(4, 5), 0, 1))
  }
}
