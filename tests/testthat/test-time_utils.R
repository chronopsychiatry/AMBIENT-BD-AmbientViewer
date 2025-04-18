test_that("mean_time calculates the correct mean for valid time strings", {
  time_vector <- c("2025-04-08 23:00:00", "2025-04-09 01:00:00")
  result <- mean_time(time_vector)
  expect_equal(result, "00:00")
})

test_that("mean_time handles POSIXct input correctly", {
  time_vector <- as.POSIXct(c("2025-04-08 23:00:00", "2025-04-09 01:00:00"), tz = "UTC")
  result <- mean_time(time_vector)
  expect_equal(result, "00:00")
})

test_that("mean_time handles NA values gracefully", {
  time_vector <- c("2025-04-08 23:00:00", NA, "2025-04-09 01:00:00")
  result <- mean_time(time_vector)
  expect_equal(result, "00:00")
})

test_that("mean_time handles empty input", {
  time_vector <- c()
  result <- mean_time(time_vector)
  expect_equal(result, NA_character_)
})

test_that("mean_time handles single time input", {
  time_vector <- c("2025-04-08 12:00:00")
  result <- mean_time(time_vector)
  expect_equal(result, "12:00")
})

test_that("convert_times_to_mean_angle works correctly", {
  times <- c(0, 43200, 86400)
  angle <- convert_times_to_mean_angle(times, unit = "second")
  expect_true(is.numeric(angle))
  expect_true(angle >= 0 && angle <= 2 * pi)
})

test_that("convert_angle_to_time works correctly", {
  angle <- pi / 2
  time <- convert_angle_to_time(angle, unit = "second")
  expect_true(is.numeric(time))
  expect_equal(time, 21600)
})

test_that("shift_times_by_12h shifts times correctly", {
  times <- c("2025-04-08 00:00:00", "2025-04-08 12:00:00", "2025-04-08 15:30:00")
  shifted_times <- shift_times_by_12h(times)
  expected_times <- c(12, 0, 3.5)
  expect_equal(shifted_times, expected_times)
})

test_that("shift_times_by_12h works with POSIXct data", {
  times <- as.POSIXct(c("2025-04-08 00:00:00", "2025-04-08 12:00:00", "2025-04-08 15:30:00"), tz = "UTC")
  shifted_times <- shift_times_by_12h(times)
  expected_times <- c(12, 0, 3.5)
  expect_equal(shifted_times, expected_times)
})

test_that("get_time_per_day works correctly", {
  expect_equal(get_time_per_day("second"), 86400)
  expect_equal(get_time_per_day("minute"), 1440)
  expect_equal(get_time_per_day("hour"), 24)

  expect_error(get_time_per_day("invalid_unit"),
               "Invalid unit. Use 'second', 'minute', or 'hour'.")
})

test_that("is_iso8601_datetime correctly identifies ISO 8601 datetime strings", {
  valid_datetimes <- c("2025-04-08T09:49:52.888000+01:00", "2025-04-08T10:00:00Z")
  expect_true(is_iso8601_datetime(valid_datetimes))

  mixed_values <- c("2025-04-08T09:49:52.888000+01:00", NA, "2025-04-08T10:00:00Z")
  expect_true(is_iso8601_datetime(mixed_values))

  empty_and_na <- c("", NA, "")
  expect_true(is_iso8601_datetime(empty_and_na))

  invalid_datetimes <- c("not-a-date", "2025-04-08 10:00:00", "08-04-2025T10:00:00")
  expect_false(is_iso8601_datetime(invalid_datetimes))

  mixed_invalid <- c("2025-04-08T09:49:52.888000+01:00", "not-a-date", NA)
  expect_false(is_iso8601_datetime(mixed_invalid))

  empty_vector <- character(0)
  expect_true(is_iso8601_datetime(empty_vector))
})
