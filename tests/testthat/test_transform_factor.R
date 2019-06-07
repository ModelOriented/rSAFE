context("Testing safely_transform_factor.R file")

source("objects_for_tests.R")


test_that("safely_transform_factor", {
  expect_is(safely_transform_factor(explainer_rf, "district"), "list")
  expect_is(safely_transform_factor(explainer_rf_modified, "district"), "list")
  expect_error(safely_transform_factor(1:10, "district"))
  expect_error(safely_transform_factor(explainer_rf, "strange_variable"), "Wrong variable name!")
})
