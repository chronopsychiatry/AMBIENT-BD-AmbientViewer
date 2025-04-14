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
