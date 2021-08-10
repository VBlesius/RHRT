###-----------------------------------------------------------------------------
# Part of RHRT: R package to assess Heart Rate Turbulence from RR interval data 
# Copyright (C) 2021 Valeria Blesius

# RHRT is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, version 2 only.

# RHRT is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with RHRT.  If not, see <https://www.gnu.org/licenses/>.
###-----------------------------------------------------------------------------

library(RHRT)
context("vectorToHRT: checking input validity")

load(test_path("testdata", "testdataVariant.rda"))

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

test_that("annotations", {
  expect_error(vectorToHRT(testdataVariant, annotations = list()), "annotation data does not have the right type")
  expect_error(vectorToHRT(testdataVariant, annotations = 0), "Given annotation vector is not alphabetical")
  expect_error(vectorToHRT(testdataVariant, annotations = NA_character_), "Given vector includes NA")
  expect_error(vectorToHRT(testdataVariant, annotations = rep("a", 5)), "lengths of given annotation and RR vectors differ")
  expect_warning(a <- vectorToHRT(testdataVariant, annotations = rep("a", 1000)), "given annotation for PVCs could not be found")
  expect_equal(a, new("HRTList"))
})

test_that("other parameters", {
  expect_error(vectorToHRT(testdataVariant, numPreRRs = 0), "parameter numPreRRs has to be at least 2")
  expect_error(vectorToHRT(testdataVariant, numPostRRs = 0), "parameter numPostRRs has to be at least 5")
})
