library(RHRT)
context("HRTs in input")

data("testdataRegular")
data("testdataVariant")
data("testdataVariantNoHRT")

test_that("Finding HRTs", {
  expect_length(vectorToHRT(testdataRegular)@HRTs, 5)
  expect_length(vectorToHRT(testdataVariant)@HRTs, 3)
  expect_length(vectorToHRT(rep(782, 1000))@HRTs, 0)
  expect_length(vectorToHRT(testdataVariantNoHRT)@HRTs, 0)
     
  expect_warning(vectorToHRT(rep(782, 1000)), "No HRTs")
  expect_warning(vectorToHRT(rep(2001, 100)), "No HRTs")
  expect_warning(vectorToHRT(testdataVariantNoHRT), "No HRTs")
  
})