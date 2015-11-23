# Load input data
args = commandArgs(TRUE)
data = read.table(args[1])
data = unlist(data*1000)

# Load dependencies
library(methods)
sourcepath = "/home/etz/Documents/Promotion/"
source(file.path(sourcepath, "R/Scripts/wapply.R"))
source(file.path(sourcepath, "R/Scripts/HRT_calc_Functions.R"))
source(file.path(sourcepath, "R/Scripts/PVC.R"))


# Variable declaration
n_RRpre = 6 # number of "normal" RR-intervals before the coupling interval
n_RRpost = 16 # number of "normal" RR-intervals after the coupling interval
windowsize = n_RRpre + n_RRpost + 2 # adds coupling and compensatory interval


# Pipeline
PVCs = wapply(data, windowsize, by = 1, FUN = checkForPVC)
PVCs = PVCs[!sapply(PVCs, is.null)] # removes NULL entries
PVCs = lapply(PVCs, listToPVC) # saves lists as PVC-objects
PVCs = lapply(PVCs, getHRTParameters) #calculates TO and TS

cat(length(PVCs), "\t", mean(sapply(PVCs, slot, "TO")), "\t", mean(sapply(PVCs, slot, "TS")), "\n")
#cat("Anzahl der PVCs: ", length(PVCs))
#cat("TO: ", mean(sapply(PVCs, slot, "TO")), "%")
#cat("TS: ", mean(sapply(PVCs, slot, "TS")), "ms/RR")
