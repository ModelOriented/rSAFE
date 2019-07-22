context("Testing safely_transform_continuous.R file")

source("objects_for_tests.R")


test_that("safely_transform_continuous", {
  expect_is(safely_transform_continuous(explainer_rf, "floor"), "list")
  expect_is(safely_transform_continuous(explainer_rf, "floor", penalty = 99999, response_type = "pdp"), "list")
  expect_error(safely_transform_continuous(1:10, "surface"))
  expect_error(safely_transform_continuous(explainer_rf, "strange_variable"), "Wrong variable name!")
  expect_warning(safely_transform_continuous(explainer_rf, "surface", response_type = 'strange_type'), "Wrong type of response - using default one.")
  expect_warning(safely_transform_continuous(explainer_rf, "surface", no_segments = 2.5), "Wrong number of segments - using default one.")
})

test_that("pretty_intervals", {
  expect_error(pretty_intervals(break_points = NULL))
  expect_equal(pretty_intervals(c(2,5)), c("(-Inf, 2]", "(2, 5]", "(5, Inf)"))
  expect_length(pretty_intervals(c(1234.5678, 1234.56789)), 3)
})

