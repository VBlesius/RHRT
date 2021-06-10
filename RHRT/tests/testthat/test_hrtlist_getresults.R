library(RHRT)
context("getResults: returning results of an HRTList")

load(test_path("testdata", "testdataVariant_HRTObj.rda"))

HRTs_temp <- testdataVariant_HRTObj
HRTs_temp@avHRT <- methods::new("avHRT")

test_that("argument handling", {
  expect_error(getResults(HRTs_temp), "The average HRT is empty")
  expect_error(getResults(testdataVariant_HRTObj, type = "random"), "given value for 'type' is unknown")
  expect_warning(a <- getResults(testdataVariant_HRTObj, type = "class", num = TRUE), "combination of type 'class' and num 'TRUE' is not possible")
  expect_true(is.na(a))
})

test_that("output of significant results", {
  expect_match(getResults(testdataVariant_HRTObj), "HRT[0-2]")
  expect_match(getResults(testdataVariant_HRTObj, nTS = TRUE), "HRT[0-2]")
  expect_match(getResults(testdataVariant_HRTObj, TT = TRUE), "HRT[A-C]")
  expect_match(getResults(testdataVariant_HRTObj, TT = TRUE, nTS = TRUE), "HRT[A-C]")
  expect_length(getResults(testdataVariant_HRTObj, type = "parameter"), 2)
  expect_length(getResults(testdataVariant_HRTObj, type = "parameter", TT = TRUE), 3)
  expect_true(is.numeric(getResults(testdataVariant_HRTObj, type = "parameter", TT = TRUE)))
  expect_true(is.numeric(getResults(testdataVariant_HRTObj, type = "parameter", nTS = TRUE)))
  expect_length(getResults(testdataVariant_HRTObj, type = "full", TT = TRUE), 6)
  expect_true(is.numeric(getResults(testdataVariant_HRTObj, type = "full", TT = TRUE)))
})

HRTsCrit <- testdataVariant_HRTObj
HRTsCrit@avHRT@TO <- COTO + 100
test_that("output of all classes: TO", {
  expect_match(getResults(HRTsCrit), "HRT1")
  expect_match(getResults(HRTsCrit, TT = TRUE), "HRTB")
})

HRTsCritTS <- HRTsCrit
HRTsCritTS@avHRT@TS <- COTS - 100
test_that("output of all classes: TO+TS", {
  expect_match(getResults(HRTsCritTS), "HRT2")
  expect_match(getResults(HRTsCritTS, TT = TRUE), "HRTB")
})

HRTsCritTT <- HRTsCrit
HRTsCritTT@avHRT@TT <- COTT + 100
test_that("output of all classes: TO + TT", {
  expect_match(getResults(HRTsCritTT), "HRT1")
  expect_match(getResults(HRTsCritTT, TT = TRUE), "HRTB")
})

HRTsCritTT@avHRT@TS <- COTS - 100
test_that("output of all classes: TO + TT", {
  expect_match(getResults(HRTsCritTT), "HRT2")
  expect_match(getResults(HRTsCritTT, TT = TRUE), "HRTC")
})

HRTsNS <- testdataVariant_HRTObj
HRTsNS@avHRT@pTO <- 0.5

test_that("output of insignificant results", {
  expect_equal(getResults(HRTsNS), "NR")
  expect_true(is.character(getResults(HRTsNS, type = "parameter")["TO"]))
  expect_true(is.numeric(a <- getResults(HRTsNS, type = "parameter", num = TRUE)["TO"]))
  expect_true(is.na(a))

  expect_equal(getResults(HRTsNS, safe = FALSE), "HRT0")
  expect_true(is.numeric(getResults(HRTsNS, type = "parameter", safe = FALSE)))
  expect_equal(getResults(HRTsNS, type = "full"), getResults(HRTsNS, type = "full", safe = FALSE))
})

HRTsNA <- testdataVariant_HRTObj
HRTsNA@avHRT@TO <- NA_real_

test_that("output of NA results #1", {
  expect_warning(a <- getResults(HRTsNA), "The HRT parameters contain NA")
  expect_true(is.na(a))
})

HRTsNA <- testdataVariant_HRTObj
HRTsNA@avHRT@pTO <- NA_real_

test_that("output of NA results #2", {
  expect_true(is.character(getResults(HRTsNA, type = "parameter", safe = TRUE)["TO"]))
  expect_true(is.numeric(a <- getResults(HRTsNA, type = "parameter", safe = TRUE, num = TRUE)["TO"]))
  expect_true(is.na(a))
})
