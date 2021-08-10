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
