test_that("is_iso8601_datetime correctly identifies ISO 8601 datetime strings", {
  # Valid ISO 8601 datetime strings
  valid_datetimes <- c("2025-04-08T09:49:52.888000+01:00", "2025-04-08T10:00:00Z")
  expect_true(is_iso8601_datetime(valid_datetimes))

  # Mixed valid and NA values
  mixed_values <- c("2025-04-08T09:49:52.888000+01:00", NA, "2025-04-08T10:00:00Z")
  expect_true(is_iso8601_datetime(mixed_values))

  # Empty strings and NA values
  empty_and_na <- c("", NA, "")
  expect_true(is_iso8601_datetime(empty_and_na))

  # Invalid datetime strings
  invalid_datetimes <- c("not-a-date", "2025-04-08 10:00:00", "08-04-2025T10:00:00")
  expect_false(is_iso8601_datetime(invalid_datetimes))

  # Mixed valid and invalid datetime strings
  mixed_invalid <- c("2025-04-08T09:49:52.888000+01:00", "not-a-date", NA)
  expect_false(is_iso8601_datetime(mixed_invalid))

  # Completely empty vector
  empty_vector <- character(0)
  expect_true(is_iso8601_datetime(empty_vector))
})
