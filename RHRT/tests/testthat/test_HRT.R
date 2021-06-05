library(RHRT)
context("HRT functions")

test_that("validity", {
  expect_error(checkValidity(new("HRT")), "One or more interval is not set")
})

hrt <- new("HRT", preRRs = c(800, 800), postRRs = c(800, 800), couplRR = 500, compRR = 1200)

test_that("TO calculation", {
  expect_equal(calcTO(hrt)@TO, 0)
})

hrt@preRRs <- c(-800, 800)

test_that("TO calculation", {
  expect_warning(calcTO(hrt), "Turbulence onset can't be calculated")
})