# Load input data
args <- commandArgs(TRUE)
suppressWarnings(tryCatch(
  data <- read.table(file.path(getwd(), args[1])),
  error = function(e) {
    stop("File could not be found! Path incorrect or file not existing!
      Please try again and pass a valid relative path!", call. = FALSE)
  }
))
data <- unlist(data * 1000)

# Load dependencies
library(methods)
sourcepath <- "/home/etz/Documents/Promotion/"
source(file.path(sourcepath, "R/Scripts/wapply.R"))
source(file.path(sourcepath, "R/Scripts/RHRT_Functions.R"))
source(file.path(sourcepath, "R/Scripts/RHRT_PVC.R"))

# Pipeline
pvcs <- get_pvcs(data)
averaged_pvc <- calc_averaged_pvc()
averaged_pvc <- get_hrt_params(averaged_pvc)

# Output
cat(length(pvcs), "\t", mean(sapply(pvcs, slot, "to")), "\t", mean(sapply(pvcs, slot, "ts")), "\n")
cat("Average", "\t", averaged_pvc@to, "\t", averaged_pvc@ts, "\n")
