library(RHRT)
context("HRTs in input")

data("testdataRegular")
data("testdataVariant")
data("testdataVariantNoHRT")

test_that("Finding HRTs", {
  expect_equal(length(vectorToHRT(testdataRegular)@HRTs), 5)
  expect_equal(length(vectorToHRT(testdataVariant)@HRTs), 3)

  expect_error(vectorToHRT(rep(782, 1000)), "No HRTs")
  expect_error(vectorToHRT(testdataVariantNoHRT), "No HRTs")
})