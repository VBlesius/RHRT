# Load dependencies
library(RHRT)

tdVariant <- testdataVariant
hrtVar <- vectorToHRT(tdVariant)
plot(hrtVar)
hrtVarParams <- getParamsMean(hrtVar)