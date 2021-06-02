library(RHRT)
context("calculating avHRT for a HRTList")

data("testdataVariant_HRTObj")

HRTs_temp <- testdataVariant_HRTObj
HRTs_temp@avHRT <- methods::new("avHRT")

test_that("parameter handling: HRTListObj", {
  expect_error(getResults(HRTs_temp), "The average HRT is empty")
})

test_that("output for best case", {
  expect_true(is.character(getResults(testdataVariant_HRTObj)))
  expect_length(getResults(testdataVariant_HRTObj, type = "parameter"), 2)
  expect_true(is.numeric(getResults(testdataVariant_HRTObj, type = "parameter")["TO"]))
  expect_length(getResults(testdataVariant_HRTObj, type = "full"), 4)
})

HRTsNS <- testdataVariant_HRTObj
HRTsNS@avHRT@pTO <- 0.5

test_that("handling of insignificant parameters", {
  expect_equal(getResults(HRTsNS), "NR")
  expect_equal(getResults(HRTsNS, safe = FALSE), "HRT0")
  expect_true(is.character(getResults(HRTsNS, type = "parameter")["TO"]))
  expect_true(is.numeric(getResults(HRTsNS, type = "parameter", safe = FALSE)["TO"]))
  expect_equal(getResults(HRTsNS, type = "full"), getResults(HRTsNS, type = "full", safe = FALSE))
})

HRTsNA <- testdataVariant_HRTObj
HRTsNA@avHRT@TO <- NA_real_

test_that("handling of NA parameters", {
  expect_warning(a <- getResults(HRTsNA), "The HRT parameters contain NA")
  expect_true(is.na(a))
})
