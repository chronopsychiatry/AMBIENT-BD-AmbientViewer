test_that("get_sessions_summary works correctly with valid input", {
  sessions <- data.frame(
    session_start = c("2025-04-01T10:00:00", "2025-04-01T14:00:00"),
    session_end = c("2025-04-01T12:00:00", "2025-04-01T16:00:00"),
    time_at_sleep_onset = c("2025-04-01T22:00:00", "2025-04-01T23:00:00"),
    time_at_wakeup = c("2025-04-02T06:00:00", "2025-04-02T07:00:00"),
    subject_id = c("subject1", "subject1"),
    device_serial_number = c("device123", "device123")
  )

  result <- get_sessions_summary(sessions)

  expect_equal(result$subject_id, "subject1")
  expect_equal(result$device_id, "device123")
  expect_equal(result$total_sessions, 2)
  expect_equal(result$mean_session_length, 2)
})

test_that("get_sessions_summary handles empty input", {
  sessions <- data.frame(
    session_start = character(),
    session_end = character(),
    time_at_sleep_onset = character(),
    time_at_wakeup = character(),
    subject_id = character(),
    device_serial_number = character()
  )

  result <- get_sessions_summary(sessions)

  expect_equal(nrow(result), 1)
  expect_true(result$subject_id == "")
  expect_true(result$device_id == "")
  expect_equal(result$total_sessions, 0)
  expect_true(is.na(result$mean_session_length))
})
