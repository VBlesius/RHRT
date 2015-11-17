path = "/home/etz/Documents/Promotion/"
source(file.path(path, "R/Scripts/PVC.R"))
source(file.path(path, "R/Scripts/HRT_calc_Functions.R"))

# Read in data
# TODO: substitue with data input from command line!
data = read.table(file.path(path , "Data/Testdat_PVC"))
data = unlist(data*1000)


# Variable declaration
n_RRpre = 6 # number of "normal" RR-intervals before the coupling interval
n_RRpost = 16 # number of "normal" RR-intervals after the coupling interval
windowsize = n_RRpre + n_RRpost + 2 # adds coupling and compensatory interval


# Pipeline
PVCs = wapply(data, windowsize, by = 1, FUN = checkForPVC)
PVCs = PVCs[!sapply(PVCs, is.null)] # removes NULL entries
PVCs = lapply(PVCs, listToPVC) # saves lists as PVC-objects
PVCs = lapply(PVCs, getHRTParameters) #calculates TO and TS

cat("Anzahl der PVCs: ", length(PVCs))
cat("TO: ", mean(sapply(PVCs, slot, "TO")), "%")
cat("TS: ", mean(sapply(PVCs, slot, "TS")), "ms/RR")
