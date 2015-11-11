path = "/home/etz/Documents/Promotion/Data/"
require(zoo)

# Read in data
# TODO: substitue with data input from command line!
data = read.table(file.path(path , "Testdat_PVC"))
data = unlist(data*1000)


# Variable declaration
n_RRpre = 6 # number of "normal" RR-intervals before the coupling interval
n_RRpost = 16 # number of "normal" RR-intervals after the coupling interval
windowsize = n_RRpre + n_RRpost + 2 # adds coupling and compensatory interval


# Pipeline
PVCs_all = rollapply(data, windowsize, checkForPVC)

PVCs = which(PVCs_all == TRUE)+n_RRpre
# Stattdessen
## wapply? http://www.r-bloggers.com/wapply-a-faster-but-less-functional-rollapply-for-vector-setups/ 
## RcppRoll package?


# Function declaration

checkForPVC = function(x) {
  
  i_coupl= x[n_RRpre+1] # coupling interval
  i_comp = x[n_RRpre+2] # compensatory interval
  i_pre = x[1:n_RRpre] # preceding intervals
  i_post = x[n_RRpre+3:n_RRpost] # following intervals (after comp. interval)
  i_RRnorm = c(i_pre, i_post) # all RR-intervals that need to be filtered and aren't coupling or comp. interval
  
  ref = mean(i_pre)
  
  is.lessDistant = function(x, distance) {
    diff(x) <= distance
  }
  
  isCouplInt = i_coupl <= ref*0.8
  isCompInt = i_comp >= ref*1.2
  isInRange = all(i_RRnorm > 300 && i_RRnorm < 2000)
  isNotDeviating = all(
   i_RRnorm >= ref*0.8, i_RRnorm <= ref*1.2,
    rollapply(i_pre, 2, is.lessDistant, 200), rollapply(i_post, 2, is.lessDistant, 200))
  
  if (isCouplInt & isCompInt  # checks for PVC
      & isInRange & isNotDeviating) { # checks for arrhythmias and artefacts
    return(TRUE)
  } else {
    return(FALSE)
  }
}

