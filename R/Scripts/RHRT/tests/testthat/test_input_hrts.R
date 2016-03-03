library(RHRT)
context("HRTs in input")

data("testdataRegular")
data("testdataVariant")
data("testdataRegularNoHRT")
data("testdataVariantNoHRT")

test_that("Finding HRTs", {
  expect_equal(length(vectorToHRT(testdataRegular)@HRTs), 5)
  expect_equal(length(vectorToHRT(testdataVariant)@HRTs), 3)
  expect_equal(length(vectorToHRT(rep(700, 1000))@HRTs), 0)
  expect_equal(length(vectorToHRT(testdataVariantNoHRT)@HRTs), 0)
  expect_equal(length(vectorToHRT(testdataRegularNoHRT)@HRTs), 0)
  
  expect_warning(vectorToHRT(rep(700, 1000)), "No HRTs")
  expect_warning(vectorToHRT(testdataVariantNoHRT), "No HRTs")
  expect_warning(vectorToHRT(testdataRegularNoHRT), "No HRTs")
})