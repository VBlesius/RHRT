library(RHRT)
context("Input validity")

test_that("vector", {
  expect_error(vectorToHRT(NULL), "NULL")
  expect_error(vectorToHRT(list()), "NULL")
})

test_that("vector", {
  expect_error(vectorToHRT(matrix()), "type vector")
})

test_that("numeric", {
  expect_error(vectorToHRT("String"), "numeric")
  expect_error(vectorToHRT(logical()), "numeric")
  expect_error(vectorToHRT(NA), "numeric")
})

test_that("NA", {
  expect_error(vectorToHRT(c(1, 2, NA)), "NA")
})

test_that("numerical ranges", {
  expect_warning(vectorToHRT(c(0, seq(1:30))), "zero or negative")
  expect_warning(vectorToHRT(c(-1, seq(1:30))), "zero or negative")
})

test_that("length of data", {
  expect_error(vectorToHRT(1), "too short")
  expect_error(vectorToHRT(seq(1:(c_numPreRRs + c_numPostRRs + 1))), "too short")
  expect_error(vectorToHRT(c(1, 2, 3)), "too short")
  expect_error(vectorToHRT(list(1, 2, 3)), "too short")
})

test_that("unit of data", {
  expect_error(vectorToHRT(rep(0.5, 100)), "seconds")
})
