library(RHRT)
context("HRTs in input")

data("testdataRegular")
data("testdataVariant")
data("testdataVariantNoHRT")

data(list("testdataVariant_HRTObj"))
data(list("testdataRegular_HRTObj"))
data(list("testdataVariantNoHRT_HRTObj"))

test_that("Finding HRTs", {
  #expect_equal(vectorToHRT(testdataRegular), testdataRegular_HRTObj)
  #expect_equal(vectorToHRT(testdataVariant), testdataVariant_HRTObj)
  #expect_length(vectorToHRT(rep(782, 1000))@HRTs, 0)
  #expect_equal(vectorToHRT(testdataVariantNoHRT), testdataVariantNoHRT_HRTObj)

  #expect_warning(vectorToHRT(rep(782, 1000)), "No HRTs")
  #expect_warning(vectorToHRT(rep(2001, 100)), "No HRTs")
  #expect_warning(vectorToHRT(testdataVariantNoHRT), "No HRTs")
})
