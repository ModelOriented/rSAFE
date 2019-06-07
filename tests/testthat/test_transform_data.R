context("Testing safely_transform_data.R file")

source("objects_for_tests.R")


test_that("safely_transform_data", {
  expect_is(safely_transform_data(safe_extractor_modified, apartmentsTest[2001:4000,]), "data.frame")
  expect_is(safely_transform_data(safe_extractor, apartmentsTest[2001:4000,]), "data.frame")
  expect_error(safely_transform_data(1:10))
  expect_error(safely_transform_data(safe_extractor, data = NULL))
})
