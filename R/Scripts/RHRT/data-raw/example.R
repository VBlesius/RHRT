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
hrts <- get_hrts(data)
hrts <- sapply(hrts, get_hrt_params)
averaged_hrt <- calc_averaged_hrt(hrts)
averaged_hrt <- get_hrt_params(averaged_hrt)

# Output
cat(length(hrts), "\t", mean(sapply(hrts, slot, "to")), "\t", mean(sapply(hrts, slot, "ts")), "\n")
cat("Average", "\t", averaged_hrt@to, "\t", averaged_hrt@ts, "\n")
