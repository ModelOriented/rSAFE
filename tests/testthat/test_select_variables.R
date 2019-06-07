context("Testing safely_select_variables.R file")

source("objects_for_tests.R")


test_that("safely_select_variables", {
  expect_is(safely_select_variables(safe_extractor, data1, which_y = 2), "character")
  expect_error(safely_select_variables(1:10))
  expect_error(safely_select_variables(safe_extractor, data = NULL), "No data provided!")
  expect_error(safely_select_variables(safe_extractor, data1), "Specify either y or which_y argument!")
  expect_error(safely_select_variables(safe_extractor, data1, which_y = "strange_variable"))
  expect_is(safely_select_variables(safe_extractor_hr, HR[1:1000,], which_y = "status"), "character")
  expect_is(safely_select_variables(safe_extractor_hr, HR, which_y = "status", class_pred = "strange_class"), "character")
  expect_is(safely_select_variables(safe_extractor_hr, HR, which_y = "status", class_pred = 100), "character")
})

