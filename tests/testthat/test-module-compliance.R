test_that("make_compliance_table output is correct", {
  result <- make_compliance_table(example_sessions)

  expect_equal(class(result), "data.frame")
  expect_equal(nrow(result), 123)
  expect_equal(ncol(result), 8)
  expect_length(unique(result$night), 14)
})
