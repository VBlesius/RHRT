library(RHRT)
context("calculating avHRT for a HRTList")

data("testdataHRTs")

test_that("parameter handling: HRTListObj", {
  expect_error(calcAvHRT(new("HRTList")), "The HRTList does not contain any HRTs")
})

test_that("parameter handling: av", {
  expect_equal(calcAvHRT(HRTs, av = mean)@av, mean)
  expect_equal(calcAvHRT(HRTs, av = median)@av, median)
  expect_warning(calcAvHRT(HRTs, av = sum)@av, "for parameter averaging is unknown, falling back to default")
  expect_warning(calcAvHRT(HRTs, av = NA)@av, "for parameter averaging is unknown, falling back to default")
})