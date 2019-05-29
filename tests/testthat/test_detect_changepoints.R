context("Testing safely_detect_changepoints")

source("objects_for_tests.R")

test_that("safely_detect_changepoints", {
  expect_equal(safely_detect_changepoints(v1), 4)
  expect_is(safely_detect_changepoints(v1), "integer")
})

