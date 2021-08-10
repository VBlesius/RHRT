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
context("calcAvHRT: calculating avHRT for an HRTList")

load(test_path("testdata", "testdataVariant_HRTObj.rda"))
load(test_path("testdata", "testdataVariant.rda"))


test_that("parameter handling: HRTListObj", {
  expect_error(calcAvHRT(new("HRTList")), "The HRTList does not contain any HRTs")
})

test_that("parameter handling: av", {
  expect_equal(calcAvHRT(testdataVariant_HRTObj, av = mean)@av, mean)
  expect_equal(calcAvHRT(testdataVariant_HRTObj, av = median)@av, median)
  expect_warning(calcAvHRT(testdataVariant_HRTObj, av = sum), "for parameter averaging is unknown, falling back to default")
  expect_warning(calcAvHRT(testdataVariant_HRTObj, av = NA), "for parameter averaging is unknown, falling back to default")
})

test_that("parameter handling: others", {
  expect_error(calcAvHRT(testdataVariant_HRTObj, IL = "random"), "Values for normalisation or cut-offs are not numeric")
  expect_error(calcAvHRT(testdataVariant_HRTObj, IL = NA), "Values for normalisation or cut-offs are not numeric")
  expect_warning(calcAvHRT(testdataVariant_HRTObj, orTO = "random"), "for parameter calculation order is unknown")
  expect_warning(calcAvHRT(testdataVariant_HRTObj, orTO = NA), "for parameter calculation order is unknown")
  expect_warning(calcAvHRT(testdataVariant_HRTObj, orTS = "random"), "for parameter calculation order is unknown")
  expect_false(isTRUE(all.equal(testdataVariant_HRTObj@avHRT, calcAvHRT(testdataVariant_HRTObj, orTO = "avBefore"))))
  expect_false(isTRUE(all.equal(testdataVariant_HRTObj@avHRT, calcAvHRT(testdataVariant_HRTObj, orTS = "avAfter"))))
})

test_that("Recalculating TS", {
  expect_equal(testdataVariant_HRTObj@avHRT, calcAvHRT(testdataVariant_HRTObj))
  expect_equal(vectorToHRT(testdataVariant, normIL = 1000)@avHRT, calcAvHRT(testdataVariant_HRTObj, normIL = 1000))
  expect_warning(calcAvHRT(testdataVariant_HRTObj, normIL = 1000), "HRT objects and avHRT do not match anymore")
})
