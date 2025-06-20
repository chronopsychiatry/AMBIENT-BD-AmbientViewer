test_that("interdaily_stability works", {
  result <- interdaily_stability(example_epochs)
  expect_equal(round(result, 2), 0.52)
})

test_that("social_jet_lag works", {
  result <- social_jet_lag(example_sessions)
  expect_equal(round(result, 2), -0.28)
})

test_that("chronotype works", {
  result <- chronotype(example_sessions)
  expect_equal(round(result, 2), 2.65)
})

test_that("composite_phase_deviation works", {
  result <- composite_phase_deviation(example_sessions)
  expect_equal(round(result, 2), 4.25)
})

test_that("sleep_regularity_index works", {
  result <- sleep_regularity_index(example_epochs)
  expect_equal(round(result), 96)
})
