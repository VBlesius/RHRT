path = "/home/etz/Documents/Promotion/"
require(zoo)
source(file.path(path, "R/Scripts/PVC.R"))

# Read in data
# TODO: substitue with data input from command line!
data = read.table(file.path(path , "Data/Testdat_PVC"))
data = unlist(data*1000)


# Variable declaration
n_RRpre = 6 # number of "normal" RR-intervals before the coupling interval
n_RRpost = 16 # number of "normal" RR-intervals after the coupling interval
windowsize = n_RRpre + n_RRpost + 2 # adds coupling and compensatory interval


# Pipeline


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
    rollapply(i_pre, 2, is.lessDistant, 200), rollapply(i_post, 2, is.lessDistant, 200)
  )
  
  if (isCouplInt & isCompInt  # checks for PVC
      & isInRange & isNotDeviating) { # checks for arrhythmias and artefacts
    tempPVC = PVC(couplI=i_coupl, compI=i_comp, postRR=i_post[1:15])
    return(tempPVC)
  } else {
    return(FALSE) #TODO: delete when good sliding window method is found and functioning!
  }
}
PVCs = rollapply(data, windowsize, checkForPVC)
PVCs = vectorToPVC(as.vector(t(PVCs)))