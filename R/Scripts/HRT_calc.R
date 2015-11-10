path = "/home/etz/Documents/Promotion/Data/"
require(zoo)

data = read.table(file.path(path , "Testdat_PVC"))
data = unlist(data*1000)

PVCs = NULL

rollapply(data, 24, checkForPVC) # 6 RR needed before PVC, 1 compensatory interval, 16 after PVC = 26, see http://www.h-r-t.com/hrt/en/calc.html
# Stattdessen
## wapply? http://www.r-bloggers.com/wapply-a-faster-but-less-functional-rollapply-for-vector-setups/ 
## RcppRoll package?


checkForPVC = function(x) {
  
  i_coupl= x[7] # coupling interval
  i_comp = x[8] # compensatory interval
  i_pre = x[2:6] # preceding intervals
  i_post = x[9:24] # following intervals (after comp. interval)
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
  
  # checks for PVC
  if (isCouplInt & isCompInt) { # Prematurity of 20% & Lengthening to 120 %
      
      # checks for arrhythmias and artefacts
    if (isInRange & isNotDeviating) {
      return(TRUE)
    }
  }
}

