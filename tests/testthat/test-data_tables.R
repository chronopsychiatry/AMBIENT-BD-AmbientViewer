mock_sessions <- data.frame(
  session_id = 1:6,
  session_start = c("2025-03-03T12:00:00", "2025-03-03T20:00:00", "2025-03-04T12:00:00",
                    "2025-03-05T12:00:00", "2025-03-06T12:00:00", "2025-03-06T14:00:00"),
  session_end = c("2025-03-03T14:00:00", "2025-03-04T17:00:00", "2025-03-04T14:00:00",
                  "2025-03-05T14:00:00", "2025-03-06T14:00:00", "2025-03-06T16:00:00"),
  night = as.Date(c("2025-03-03", "2025-03-03", "2025-03-04",
                    "2025-03-05", "2025-03-06", "2025-03-06"))
)

test_that("get_duplicate_sessions returns duplicates correctly", {
  result <- get_duplicate_sessions(mock_sessions)
  expected <- tibble::tibble(
    session_id = c(1, 2, 5, 6),
    session_start = as.POSIXct(c("2025-03-03T12:00:00", "2025-03-03T20:00:00", "2025-03-06T12:00:00",
                                 "2025-03-06T14:00:00"), format = "%Y-%m-%dT%H:%M:%OS", tz = "UTC"),
    session_end = as.POSIXct(c("2025-03-03T14:00:00", "2025-03-04T17:00:00", "2025-03-06T14:00:00",
                               "2025-03-06T16:00:00"), format = "%Y-%m-%dT%H:%M:%OS", tz = "UTC"),
    night = as.Date(c("2025-03-03", "2025-03-03", "2025-03-06",
                      "2025-03-06")),
    session_duration_hours = c(2, 21, 2, 2)
  )
  expect_equal(result, expected)
})
