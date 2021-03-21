context("Testing safely_detect_changepoints.R file")

source("objects_for_tests.R")


test_that("safely_detect_changepoints", {
  expect_equal(safely_detect_changepoints(c(2,2,2,2,6,6,6,6,6)), 4)
  expect_equal(safely_detect_changepoints(data = NULL), NULL)
})

test_that("penalty_value_choice", {
  expect_equal(penalty_value_choice("MBIC", 55), 3*log(55))
  expect_equal(penalty_value_choice("AIC", 123), 2)
  expect_equal(penalty_value_choice("BIC", 100), log(100))
  expect_equal(penalty_value_choice("SIC", 99), log(99))
  expect_equal(penalty_value_choice("Hannan-Quinn", 7), 2*log(log(7)))
  expect_warning(penalty_value_choice("unknown", -44), "Wrong penalty - using default one.")
  expect_equal(penalty_value_choice(321, -11), 321)
  expect_warning(penalty_value_choice(-321, 11), "Wrong penalty - using default one.")
})

test_that("cost", {
  expect_error(cost(data = NULL), "No data provided in cost function!")
  expect_error(cost(data = 1:10, u = 8, v = 3), "Wrong indexes u and v!")
  expect_error(cost(data = 1:10, u = -3, v = 3), "Wrong indexes u and v!")
  expect_error(cost(data = 1:10, u = 8, v = 13), "Wrong indexes u and v!")
})

test_that("PELT_algorithm", {
  expect_error(PELT_algorithm(data = NULL), "No data provided in PELT_algorithm function!")
})




