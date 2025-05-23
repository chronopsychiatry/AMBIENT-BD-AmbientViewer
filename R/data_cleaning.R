#' @importFrom rlang .data
clean_ggir_sessions <- function(sessions) {
  sessions |>
    dplyr::mutate(
      session_id = dplyr::row_number(),
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
    )
}
