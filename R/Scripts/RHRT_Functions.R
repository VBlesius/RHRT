## Checks number of RR-intervals for PVC-criteria published by Schmidt et. al (see http://www.h-r-t.com/hrt/en/calc.html)

checkForPVC <- function(x) {
  
  # Defines coupling, compensatory, preceding and following intervals and sums up regular intervals
  couplRR <- x[numPreRRs+1]
  compenRR <- x[numPreRRs+2]
  preRRs <- x[1:numPreRRs]
  postRRs <- x[(numPreRRs+3):length(x)]
  regRR <- c(preRRs, postRRs)
  
  # Reference interval
  ref <- mean(preRRs)
  
  ## Filtering methods
  # checks for PVC
  isCouplInterv <- couplRR <= ref*0.8
  isCompenInterv <- compenRR >= ref*1.2
  # checks for arrhythmias and artefacts
  isInRange <- all(regRR > 300 && regRR < 2000)
  isNotDeviating <- all(
    regRR >= ref*0.8, regRR <= ref*1.2,
    wapply(preRRs, 2, by=1, FUN=function(x) diff(x)<=200), wapply(postRRs, 2, by=1, FUN=function(x) diff(x)<=200)
  )
  
  # Checks for criteria and saves PVC as object
  if (isCouplInterv & isCompenInterv & isInRange & isNotDeviating) {
    tempPVC <- PVC(couplRR=couplRR, compRR=compenRR, preRRs=preRRs[-(numPreRRs-2):0], postRRs=postRRs[1:15])
    return(tempPVC)
  }
}