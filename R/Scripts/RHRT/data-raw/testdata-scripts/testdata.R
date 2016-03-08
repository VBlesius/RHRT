args <- commandArgs(TRUE)
meanHrtPath <- args[1]
## arguments:
# -l # (Number of intervals)
# -p # (PVC: TRUE with number of PVCs)
# -sd #.### (Noise: TRUE with sd in seconds)

tdLength = -1 # Number of RRs
tdNumPVC = -1 # Number of PVCs
tdDev = -1 # Standard deviation

get_arg_value = function(x) {
  arg_post = as.numeric(args[which(args==x)+1], call. = FALSE)
  if(!is.numeric(arg_post) || !length(arg_post) || is.na(arg_post)) {
    stop("Argument following ", x, " not existing or no integer!", call. = FALSE)
  } else if (arg_post < 0) {
    stop("Numeric after ", x, " must be positive!", call. = FALSE)
  } else {
    return(arg_post)
  }
}

if(length(args) == 0 || any(args=="-h")) {
  stop("At least one argument must be supplied!
      -h\tshows this manual
      -l\tnum (set number of intervals)
      -p\tnum (adds given number of PVCs)
      -sd\tdev (adds noise with dev as the standard deviation in seconds)\n")
} else if (!any(args=="-l")) {
  stop("You have to set the length of the dataset, try '-h' for help")
}
tdLength = get_arg_value("-l")

if(any(args=="-p")) {
  tdNumPVC = get_arg_value("-p") 
  if (tdNumPVC > tdLength/25){
    stop("Number of PVCs too high! The amount of PVCs multiplied with their length (25 intervals) mustn't exceed the length of the dataset!")
  }
}

if(any(args=="-sd")) {
  tdDev = get_arg_value("-sd")
  if (tdDev > 0.1) cat("Warning! Your given tandard deviation is very high! Reasonable values are less than 0.05.")
}
 
# Read in calculated data
measuredData <- read.table(meanHrtPath)
RRMean <- measuredData[1,]
PVCMean <- measuredData[2:length(measuredData[,1]),]
PVCMeanDiff <- PVCMean-PVCMean[1]

# Calculate test data
Testdata <- vector()

if(tdNumPVC <= 0) {
  Testdata <- rep(RRMean, length.out=tdLength)
} else {
  Testdata <- rep(c(rep(RRMean, tdLength/tdNumPVC-length(PVCMean)), PVCMeanDiff+RRMean), length.out=tdLength)
}

if(tdDev > 0) {
  Testdata <- Testdata + rnorm(tdLength, 0, tdDev)
}

cat(round(Testdata, 3), sep="\n")