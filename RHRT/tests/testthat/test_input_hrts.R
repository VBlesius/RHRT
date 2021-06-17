library(RHRT)
context("vectorToHRT: producing correct output")

load(test_path("testdata", "testdataRegular.rda"))
load(test_path("testdata", "testdataVariant.rda"))
load(test_path("testdata", "testdataVariantNoHRT.rda"))

load(test_path("testdata", "testdataRegular_HRTObj.rda"))
load(test_path("testdata", "testdataVariant_HRTObj.rda"))
load(test_path("testdata", "testdataVariantNoHRT_HRTObj.rda"))

load(test_path("testdata", "testdata_Ann.rda"))

test_that("Finding HRTs", {
  expect_equal(vectorToHRT(testdataRegular), testdataRegular_HRTObj)
  expect_equal(vectorToHRT(testdataVariant), testdataVariant_HRTObj)

  expect_warning(a <- vectorToHRT(testdataVariantNoHRT), "No HRTs")
  expect_equal(a, testdataVariantNoHRT_HRTObj)

  expect_warning(a <- vectorToHRT(testdataVariant, minHRT = 10), "Too few HRTs")
  expect_equal(a, testdataVariantNoHRT_HRTObj)

  expect_warning(vectorToHRT(rep(782, 100)), "No HRTs")
  expect_warning(vectorToHRT(rep(2001, 100)), "No HRTs")

  expect_warning(vectorToHRT(seq(1, 100)), "Your data looks like timestamps")
})

test_that("Finding HRTs via annotator", {
  expect_equal(vectorToHRT(testdataRegular, annotations = testdata_Ann), testdataRegular_HRTObj)
  expect_equal(vectorToHRT(testdataRegular, annotations = list(testdata_Ann)), testdataRegular_HRTObj)
})
