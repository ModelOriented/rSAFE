context("Testing safe_extraction.R file")

source("objects_for_tests.R")


test_that("safe_extraction", {
  expect_is(safe_extraction(explainer_rf, interactions = TRUE), "safe_extractor")
  expect_error(safe_extraction(1:10))
  expect_warning(safe_extraction(explainer_rf, response_type = "strange_type"), "Wrong type of response - using default one.")
})

test_that("plot.safe_extractor", {
  expect_error(plot(safe_extractor))
  expect_error(plot(safe_extractor, variable = "strange_variable"))
  vdiffr::expect_doppelganger("plot discrete variable",
                              plot(safe_extractor, variable = "district"))
  vdiffr::expect_doppelganger("plot continuous variable",
                              plot(safe_extractor, variable = "construction.year"))
})

test_that("print.safe_extractor", {
  expect_equal(print(safe_extractor), NULL)
  expect_equal(print(safe_extractor_modified), NULL)
  expect_equal(print(safe_extractor, variable = "construction.year"), NULL)
  expect_error(print(safe_extractor, variable = "strange_variable"))
})
