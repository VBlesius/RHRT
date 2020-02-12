library(RHRT)
context("calculating avHRT for a HRTList")

data("testdataHRTs")

HRTs_temp <- HRTs
HRTs_temp@avHRT <- new("avHRT")

test_that("parameter handling: HRTListObj", {
  expect_error(getResults(HRTs_temp), "The average HRT is empty")
})

test_that("output for best case", {
  expect_true(is.character(getResults(HRTs)))
  expect_length(getResults(HRTs, type = "parameter"), 2)
  expect_true(is.numeric(getResults(HRTs, type = "parameter")["TO"]))  
  expect_length(getResults(HRTs, type = "full"), 4)
})

HRTsNS <- HRTs
HRTsNS@avHRT@pTO <- 0.5

test_that("handling of insignificant parameters", {
  expect_equal(getResults(HRTsNS), "NR")
  expect_equal(getResults(HRTsNS, safe = FALSE), "HRT0*")
  expect_true(is.character(getResults(HRTsNS, type = "parameter")["TO"]))
  expect_true(is.numeric(getResults(HRTsNS, type = "parameter", safe=FALSE)["TO"]))
  expect_equal(getResults(HRTs, type = "full"), getResults(HRTs, type = "full", safe = FALSE))
})

HRTsNA <- HRTs
HRTsNA@avHRT@TO <- NA_real_

test_that("handling of NA parameters", {
  expect_warning(getResults(HRTsNA), "The HRT parameters contain NA")
  expect_true(is.na(getResults(HRTsNA)))
})