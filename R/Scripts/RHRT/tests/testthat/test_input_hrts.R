library(RHRT)
context("HRTs in input")

data("testdataRegular")
data("testdataVariant")
data("testdataVariantNoHRT")

test_that("Finding HRTs", {
  expect_equal(length(vectorToHRT(testdataRegular)@HRTs), 5)
  expect_equal(length(vectorToHRT(testdataVariant)@HRTs), 3)
  expect_equal(length(vectorToHRT(rep(782, 1000))@HRTs), 0)
  expect_equal(length(vectorToHRT(testdataVariantNoHRT)@HRTs), 0)
  
  expect_warning(vectorToHRT(rep(782, 1000)), "No HRTs")
  expect_warning(vectorToHRT(testdataVariantNoHRT), "No HRTs")
})