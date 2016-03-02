# Load input data
args <- commandArgs(TRUE)
suppressWarnings(tryCatch(
  data <- read.table(args[1]),
  error = function(e) {
    stop("File could not be found! Path incorrect or file not existing!
      Please try again and pass a valid relative path!", call. = FALSE)
  }
))
data <- unlist(data * 1000)

# Load dependencies
library(RHRT)

# Pipeline
hrts <- getHRTs(data)
hrts <- sapply(hrts, getHRTParams)
averagedHrt <- calcAveragedHRT(hrts)
averagedHrt <- getHRTParams(averagedHrt)

# Output
cat(length(hrts), "\t", mean(sapply(hrts, slot, "TO")), "\t", mean(sapply(hrts, slot, "TS")), "\n")
cat("Average", "\t", averagedHrt@TO, "\t", averagedHrt@TS, "\n")
