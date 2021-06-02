library(RHRT)
context("HRTs in input")

data("testdataRegular")
data("testdataVariant")
data("testdataVariantNoHRT")

data(list("testdataRegular_HRTObj"))
data(list("testdataVariant_HRTObj"))
data(list("testdataVariantNoHRT_HRTObj"))

test_that("Finding HRTs", {
  expect_equal(vectorToHRT(testdataRegular), testdataRegular_HRTObj)
  expect_equal(vectorToHRT(testdataVariant), testdataVariant_HRTObj)

  expect_warning(a <- vectorToHRT(testdataVariantNoHRT), "No or too few HRTs")
  expect_equal(a, testdataVariantNoHRT_HRTObj)

  expect_warning(vectorToHRT(rep(782, 1000)), "No or too few HRTs")
  expect_warning(vectorToHRT(rep(2001, 100)), "No or too few HRTs")
})
