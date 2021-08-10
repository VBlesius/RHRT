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
context("vectorToHRT: producing correct output")

load(test_path("testdata", "testdataRegular.rda"))
load(test_path("testdata", "testdataVariant.rda"))
load(test_path("testdata", "testdataVariantNoHRT.rda"))

load(test_path("testdata", "testdataRegular_HRTObj.rda"))
load(test_path("testdata", "testdataVariant_HRTObj.rda"))
load(test_path("testdata", "testdataVariantNoHRT_HRTObj.rda"))

load(test_path("testdata", "testdata_Ann.rda"))

test_that("Finding HRTs", {
  expect_equal(vectorToHRT(testdataRegular), testdataRegular_HRTObj)
  expect_equal(vectorToHRT(testdataVariant), testdataVariant_HRTObj)

  expect_warning(a <- vectorToHRT(testdataVariantNoHRT), "No HRTs")
  expect_equal(a, testdataVariantNoHRT_HRTObj)

  expect_warning(a <- vectorToHRT(testdataVariant, minHRT = 10), "Too few HRTs")
  expect_equal(a, testdataVariantNoHRT_HRTObj)

  expect_warning(vectorToHRT(rep(782, 100)), "No HRTs")
  expect_warning(vectorToHRT(rep(2001, 100)), "No HRTs")

  expect_warning(vectorToHRT(seq(1, 100)), "Your data looks like timestamps")
})

test_that("Finding HRTs via annotator", {
  expect_equal(vectorToHRT(testdataRegular, annotations = testdata_Ann), testdataRegular_HRTObj)
  expect_equal(vectorToHRT(testdataRegular, annotations = list(testdata_Ann)), testdataRegular_HRTObj)
})
