# Load input data
args <- commandArgs(TRUE)
suppressWarnings(tryCatch(
  data <- read.table(file.path(getwd(), args[1])),
  error = function(e) { stop("File could not be found! Path incorrect or file not existing! Please try again and pass a valid relative path!", call. = FALSE) }
))
data <- unlist(data*1000)

# Load dependencies
library(methods)
sourcepath <- "/home/etz/Documents/Promotion/"
source(file.path(sourcepath, "R/Scripts/wapply.R"))
source(file.path(sourcepath, "R/Scripts/RHRT_Functions.R"))
source(file.path(sourcepath, "R/Scripts/RHRT_PVC.R"))


# Variable declaration
numPreRRs <- 6 # number of regular RR-intervals before the coupling interval
numPostRRs <- 16 # number of regular RR-intervals after the coupling interval
windowsize <- numPreRRs + numPostRRs + 2 # sums up coupling and compensatory interval


# Pipeline
PVCs <- wapply(data, windowsize, by=1, FUN=checkForPVC)
PVCs <- PVCs[!sapply(PVCs, is.null)] # removes NULL entries
PVCs <- lapply(PVCs, getHRTParameters) # calculates TO and TS
averagedPVC <- calcAveragedPVC()
averagedPVC <- getHRTParameters(averagedPVC)

# Output
cat(length(PVCs), "\t", mean(sapply(PVCs, slot, "TO")), "\t", mean(sapply(PVCs, slot, "TS")), "\n")
cat("Average", "\t", averagedPVC@TO, "\t", averagedPVC@TS, "\n")
