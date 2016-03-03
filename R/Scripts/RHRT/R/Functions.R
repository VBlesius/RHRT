#' Finds HRTs
#'
#' Scans for HRTs in a vector of RR-intervals and returns a list of HRT objects.
#' The HRT criteria used were published by Schmidt et al.
#' (more information can be found in the vignette.)
#'
#' @param intervals Numeric vector
#' @return List of HRT objects
#' 
#' @export
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

#-------------------------------------------------------------------------------
#' Creates an averaged HRT
#'
#' For each index the mean of the intervals across all HRTs in a list
#' is calculated and the averaged HRT returned.
#'
#' @details
#' Use the averaged HRT for calculating TS since averaging eliminates other
#' RR variability. TO is commonly first calculated of the single HRTs and then
#' averaged. (See "Heart Rate Turbulence: Standards of Measurement,
#' Physiological Interpretation, and Clinical Use, Axel Bauer et al.,
#' Journal of the American College of Cardiology, Volume 52, Issue 17,
#' Pages 1353-1365")
#'
#' @param hrts List of HRT objects
#' @return The averaged HRT object
#' 
#' @export
calcAveragedHRT <- function(hrts) {
  couplRR <- mean(sapply(hrts, slot, "couplRR"))
  compenRR <- mean(sapply(hrts, slot, "compenRR"))
  preRRs <- rowMeans(sapply(hrts, slot, "preRRs"))
  postRRs <- rowMeans(sapply(hrts, slot, "postRRs"))

  tempHRT <- new("HRT", couplRR = couplRR, compenRR = compenRR,
                  preRRs = preRRs, postRRs = postRRs)
  return(tempHRT)
}
