test_that("filtering module works", {
  shiny::testServer(
    filtering_server,
    args = list(sessions = shiny::reactive(example_sessions), sessions_colnames = shiny::reactive(get_session_colnames(example_sessions))),
    {
      session$setInputs(date_range = c("2025-04-03", "2025-04-17"), time_range = c("20", "06"), min_time_in_bed = 2)
      session$flushReact()
      filtered_sessions <- session$getReturned()
      expected_sessions <- example_sessions |>
        remove_sessions_no_sleep() |>
        set_min_time_in_bed(2) |>
        set_session_sleep_onset_range("20:00", "06:00")
      expect_equal(filtered_sessions(), expected_sessions)
    }
  )
})

test_that("get_removed_sessions_table output is correct", {
  filtered_sessions <- example_sessions |>
    remove_sessions_no_sleep() |>
    set_min_time_in_bed(2) |>
    set_session_sleep_onset_range("20:00", "06:00")
  result <- get_removed_sessions_table(example_sessions, filtered_sessions)

  expect_equal(class(result), "data.frame")
  expect_equal(nrow(result), 3)
  expect_equal(ncol(result), 8)
  expect_length(unique(result$night), 1)
})
