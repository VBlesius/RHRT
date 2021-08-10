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
context("HRTList functions")

load(test_path("testdata", "testdataVariant_HRTObj.rda"))
testdataVariant_HRTObj2 <- testdataVariant_HRTObj
testdataVariant_HRTObj2@pos <- NA_real_

test_that("Positions", {
  expect_equal(getPositions(testdataVariant_HRTObj), testdataVariant_HRTObj@pos)
  expect_error(getPositions(testdataVariant_HRTObj2), "There seem to be no HRTs in your HRTList")
})

test_that("Plot", {
  expect_silent(plot(testdataVariant_HRTObj, TT = TRUE))
})
