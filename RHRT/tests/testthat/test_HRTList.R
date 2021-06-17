library(RHRT)
context("HRTList functions")

load(test_path("testdata", "testdataVariant_HRTObj.rda"))
testdataVariant_HRTObj2 <- testdataVariant_HRTObj
testdataVariant_HRTObj2@pos <- NA_real_

test_that("Positions", {
  expect_equal(getPositions(testdataVariant_HRTObj), testdataVariant_HRTObj@pos)
  expect_error(getPositions(testdataVariant_HRTObj2), "There seem to be no HRTs in your HRTList")
})

test_that("Plot", {
  expect_silent(plot(testdataVariant_HRTObj, TT = TRUE))
})
