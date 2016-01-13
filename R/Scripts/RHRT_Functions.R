checkForPVC = function(x) {
  
  i_coupl= x[n_RRpre+1] # coupling interval
  i_comp = x[n_RRpre+2] # compensatory interval
  i_pre = x[1:n_RRpre] # preceding intervals
  i_post = x[(n_RRpre+3):length(x)] # following intervals (after comp. interval)
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
    #rollapply(i_pre, 2, is.lessDistant, 200), rollapply(i_post, 2, is.lessDistant, 200)
    wapply(i_pre, 2, by=1, FUN=is.lessDistant, 200), wapply(i_post, 2, by=1, FUN=is.lessDistant, 200)
  )
  
  if (isCouplInt & isCompInt  # checks for PVC
      & isInRange & isNotDeviating) { # checks for arrhythmias and artefacts
    tempPVC = PVC(couplI=i_coupl, compI=i_comp, preRR=i_pre[-(n_RRpre-2):0], postRR=i_post[1:15])
    return(tempPVC)
    #return(list(i_pre=i_pre[-(n_RRpre-2):0], i_coupl=i_coupl, i_comp=i_comp, i_post=i_post[1:15]))
  }
}