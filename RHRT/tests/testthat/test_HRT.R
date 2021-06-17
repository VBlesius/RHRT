library(RHRT)
context("HRT functions")

load(test_path("testdata", "testdataVariant_HRTObj.rda"))

test_that("validity", {
  expect_error(checkValidity(new("HRT")), "One or more interval is not set")
})

hrt <- new("HRT", preRRs = c(800, 800), postRRs = c(800, 800), couplRR = 500, compRR = 1200)
hrt2 <- hrt
hrt2@preRRs <- c(-800, 800)

test_that("TO calculation", {
  expect_equal(calcTO(hrt)@TO, 0)
  expect_warning(calcTO(hrt2), "Turbulence onset can't be calculated")
})

test_that("Plot", {
  expect_silent(plot(testdataVariant_HRTObj@avHRT, TT = TRUE))
})
