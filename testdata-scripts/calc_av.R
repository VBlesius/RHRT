source("../RHRT/R/wapply.R")

# Read in measured data
args <- commandArgs(TRUE)
rrs <- read.table(args[1])

# get PVCs (marked with "V")
PVCIndices <- which(rrs[,2] == "V")
PVCIndicesDiff <- wapply(PVCIndices, 2, 1, FUN = function(x) x[2]-x[1] )
PVCIndices <- PVCIndices[which(PVCIndicesDiff>24)]
hrts <- sapply(PVCIndices, FUN=function(x) rrs[,1][(x-4):(x+20)])

# Means
PVCMean <- rowMeans(hrts, na.rm=TRUE)
RRMean <- mean(rrs[,1])

cat(c(RRMean, PVCMean), sep="\n")