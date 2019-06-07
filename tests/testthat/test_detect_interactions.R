context("Testing safely_detect_interactions.R file")

source("objects_for_tests.R")


test_that("safely_detect_changepoints", {
  expect_equal(safely_detect_interactions(explainer_rf, verbose = FALSE), NULL)
  expect_is(safely_detect_interactions(explainer_rf, verbose = TRUE, inter_threshold = 0.01), "data.frame")
  expect_error(safely_detect_interactions(explainer = 1:10))
  expect_warning(safely_detect_interactions(explainer_rf, inter_param = -1), "Wrong inter_param value - using default one.")
  expect_warning(safely_detect_interactions(explainer_rf, inter_threshold = -1), "Wrong inter_threshold value - using default one.")
})

test_that("interaction_measure", {
  expect_is(interaction_measure(explainer_rf, "floor", "surface", inter_param = 2), "logical")
  expect_error(interaction_measure(1:10, "floor", "surface", inter_param = 2))

})
