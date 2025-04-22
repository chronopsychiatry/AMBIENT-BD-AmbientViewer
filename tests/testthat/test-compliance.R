sessions <- data.frame(
  session_start = c(
    "2025-03-10T19:30:00.000000+00:00",
    "2025-03-11T23:45:00.000000+00:00",
    "2025-03-12T01:15:00.000000+00:00",
    "2025-03-12T03:00:00.000000+00:00",
    "2025-03-12T18:00:00.000000+00:00"
  ),
  time_at_sleep_onset = c(
    "2025-03-10T22:30:00.000000+00:00",
    "",
    "",
    "2025-03-12T05:00:00.000000+00:00",
    ""
  ),
  night = c(
    "2025-03-10",
    "2025-03-11",
    "2025-03-11",
    "2025-03-11",
    "2025-03-12"
  ),
  time_in_bed = c(8 * 60 * 60, 6 * 60 * 60, 7 * 60 * 60, 0.5 * 60 * 60, 1 * 60 * 60),
  sleep_period = c(1736, 0, 0, 26364, 0)
)

test_that("set_time_in_bed works", {
  filtered_sessions <- set_min_time_in_bed(sessions, 2)
  expect_equal(nrow(filtered_sessions), 3)
})

test_that("set_session_start_time_range works for times before midnight", {
  filtered_sessions <- set_session_start_time_range(sessions, "19:00", "23:00")
  expect_equal(nrow(filtered_sessions), 1)
})

test_that("set_session_start_time_range works for midnight spanning range", {
  filtered_sessions <- set_session_start_time_range(sessions, "19:00", "02:00")
  expect_equal(nrow(filtered_sessions), 3)
})

test_that("set_session_sleep_onset_range works for times before midnight", {
  filtered_sessions <- set_session_sleep_onset_range(sessions, "19:00", "23:00")
  expect_equal(nrow(filtered_sessions), 1)
})

test_that("set_session_sleep_onset_range works for midnight spanning range", {
  filtered_sessions <- set_session_sleep_onset_range(sessions, "19:00", "02:00")
  expect_equal(nrow(filtered_sessions), 1)
})

test_that("remove_sessions_no_sleep works", {
  filtered_sessions <- remove_sessions_no_sleep(sessions)
  expect_equal(nrow(filtered_sessions), 2)
})

test_that("get_non_complying_sessions works", {
  non_complying_sessions <- get_non_complying_sessions(sessions)
  expect_equal(nrow(non_complying_sessions), 3)
})
