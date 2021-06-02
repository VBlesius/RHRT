library(RHRT)
context("calculating avHRT for a HRTList")

load(test_path("testdata", "testdataVariant_HRTObj.rda"))

test_that("parameter handling: HRTListObj", {
  expect_error(calcAvHRT(new("HRTList")), "The HRTList does not contain any HRTs")
})

test_that("parameter handling: av", {
  expect_equal(calcAvHRT(testdataVariant_HRTObj, av = mean)@av, mean)
  expect_equal(calcAvHRT(testdataVariant_HRTObj, av = median)@av, median)
  expect_warning(calcAvHRT(testdataVariant_HRTObj, av = sum)@av, "for parameter averaging is unknown, falling back to default")
  expect_warning(calcAvHRT(testdataVariant_HRTObj, av = NA)@av, "for parameter averaging is unknown, falling back to default")
})
