#' Converts a vector to HRTList
#' 
#' Description
#' 
#' @param input Numeric vector
#' @return HRTList HRTList object
#'
vectorToHRT <- function(input) {
  
}
  

#-------------------------------------------------------------------------------
# Checks data input for compatibility
checkInput <- function(input) {
  
}

#-------------------------------------------------------------------------------
#' Finds HRTs
#'
#' Scans for HRTs in a vector of RR-intervals and returns a list of HRT objects.
#' The HRT criteria used were published by Schmidt et al.
#' (more information can be found in the vignette.)
#'
#' @param intervals Numeric vector
#' @return List of HRT objects
#' 
getHRTs <- function(intervals) {
  numPreRRs <- 6 # number of regular RR-intervals before the coupling interval
  numPostRRs <- 16 # number of regular RR-intervals after the coupling interval
  windowsize <- numPreRRs + numPostRRs + 2 # sums up coupling and compensatory interval

  hrts <- wapply(intervals, windowsize, by = 1, FUN = checkForHRT)
  hrts <- hrts[!sapply(hrts, is.null)] # removes NULL entries
  return(hrts)
}

#-------------------------------------------------------------------------------
# Checks specified number of RR-intervals for HRT criteria
# and returns a HRT object
checkForHRT <- function(x) {
  numPreRRs <- 6 # number of regular RR-intervals before the coupling interval # TODO: remove redundancy!!!
  # Defines coupling, compensatory, preceding and following intervals
  # and sums up regular intervals
  couplRR <- x[numPreRRs + 1]
  compenRR <- x[numPreRRs + 2]
  preRRs <- x[1:numPreRRs]
  postRRs <- x[(numPreRRs + 3):length(x)]
  regRR <- c(preRRs, postRRs)

  # Reference interval
  ref <- mean(preRRs)

  ## Filtering methods
  # checks for HRT
  isCouplRR <- couplRR <= ref * 0.8
  isCompenRR <- compenRR >= ref * 1.2
  # checks for arrhythmias and artefacts
  isInRange <- all(regRR > 300 && regRR < 2000)
  isNotDeviating <- all(
    regRR >= ref * 0.8, regRR <= ref * 1.2,
    wapply(preRRs, 2, by = 1, FUN = function(x) diff(x) <= 200),
    wapply(postRRs, 2, by = 1, FUN = function(x) diff(x) <= 200)
  )

  # Checks for criteria and saves HRT as object
  if (isCouplRR & isCompenRR & isInRange & isNotDeviating) {
    tempHRT <- new("HRT", couplRR = couplRR,
                    compenRR = compenRR,
                    preRRs = preRRs[- (numPreRRs - 2):0],
                    postRRs = postRRs[1:15])
    return(tempHRT)
  }
}