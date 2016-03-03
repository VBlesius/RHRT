# Load dependencies
library(RHRT)

tdVariant <- unname(testdataVariant)
hrtVar <- vectorToHRT(tdVariant)
plot(hrtVar)
hrtVarParams <- getParamsMean(hrtVar)