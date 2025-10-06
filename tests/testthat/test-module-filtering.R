test_that("filtering module works", {
  shiny::testServer(
    filtering_server,
    args = list(
      sessions = shiny::reactive(example_sessions |> dplyr::mutate(annotation = "")),
      sessions_colnames = shiny::reactive(get_session_colnames(example_sessions)),
      annotations = shiny::reactive(data.frame(id = c("VEhDQRkEEQwuDQAA"), annotation = c("")))
    ),
    {
      session$setInputs(date_range = c("2025-04-03", "2025-04-17"), time_range = c("20", "06"), min_time_in_bed = 2)
      session$flushReact()
      filtered_sessions <- session$getReturned()
      expected_sessions <- example_sessions |>
        dplyr::mutate(annotation = "") |>
        remove_sessions_no_sleep() |>
        set_min_time_in_bed(2, flag_only = TRUE) |>
        set_session_sleep_onset_range("20:00", "06:00", flag_only = TRUE)
      expect_equal(filtered_sessions(), expected_sessions)
    }
  )
})

test_that("get_removed_sessions_table output is correct", {
  filtered_sessions <- example_sessions |>
    dplyr::mutate(annotation = "") |>
    remove_sessions_no_sleep() |>
    set_min_time_in_bed(2, flag_only = TRUE) |>
    set_session_sleep_onset_range("20:00", "06:00", flag_only = TRUE)
  result <- get_removed_sessions_table(filtered_sessions, col_names = get_session_colnames(example_sessions))

  expect_equal(class(result), "data.frame")
  expect_equal(nrow(result), 3)
  expect_equal(ncol(result), 9)
})
