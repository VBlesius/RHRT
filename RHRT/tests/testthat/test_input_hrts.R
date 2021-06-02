library(RHRT)
context("HRTs in input")

load(test_path("testdata", "testdataRegular.rda"))
load(test_path("testdata", "testdataVariant.rda"))
load(test_path("testdata", "testdataVariantNoHRT.rda"))

load(test_path("testdata", "testdataRegular_HRTObj.rda"))
load(test_path("testdata", "testdataVariant_HRTObj.rda"))
load(test_path("testdata", "testdataVariantNoHRT_HRTObj.rda"))

test_that("Finding HRTs", {
  expect_equal(vectorToHRT(testdataRegular), testdataRegular_HRTObj)
  expect_equal(vectorToHRT(testdataVariant), testdataVariant_HRTObj)

  expect_warning(a <- vectorToHRT(testdataVariantNoHRT), "No or too few HRTs")
  expect_equal(a, testdataVariantNoHRT_HRTObj)

  expect_warning(vectorToHRT(rep(782, 1000)), "No or too few HRTs")
  expect_warning(vectorToHRT(rep(2001, 100)), "No or too few HRTs")
})
