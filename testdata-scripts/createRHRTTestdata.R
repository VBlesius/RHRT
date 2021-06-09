###-----------------------------------------------------------------------------
### Description ###
# Creates Testdata for the RHRT package.
###-----------------------------------------------------------------------------

#-------------------------------------------------------------------------------
### Constants / Variables / data
source("createDummyData.R")
setwd("../RHRT/")

#-------------------------------------------------------------------------------

testdataVariantNoHRT <- createDummyData(1000, 0)
save(testdataVariantNoHRT, file = "tests/testthat/testdata/testdataVariantNoHRT.rda")
testdataVariantNoHRT_HRTObj <- RHRT::vectorToHRT(testdataVariantNoHRT, minHRT = 1)
save(testdataVariantNoHRT_HRTObj, file = "tests/testthat/testdata/testdataVariantNoHRT_HRTObj.rda")

testdataVariant <- createDummyData(1000, 5)
save(testdataVariant, file = "tests/testthat/testdata/testdataVariant.rda")
testdataVariant_HRTObj <- RHRT::vectorToHRT(testdataVariant)
save(testdataVariant_HRTObj, file = "tests/testthat/testdata/testdataVariant_HRTObj.rda")

testdataRegular <- createDummyData(200, 1, sd = 0)
testdataRegular <- rep(testdataRegular, 5)
save(testdataRegular, file = "tests/testthat/testdata/testdataRegular.rda")
testdataRegular_HRTObj <- RHRT::vectorToHRT(testdataRegular)
save(testdataRegular_HRTObj, file = "tests/testthat/testdata/testdataRegular_HRTObj.rda")

testdataLong <- createDummyData(100000, 15, sd = 20)
save(testdataLong, file = "data/testdataLong.rda")
testdataLong_Ann <- createDummyData(100000, 15, ann = TRUE)
save(testdataLong_Ann, file = "data/testdataLong_Ann.rda")
