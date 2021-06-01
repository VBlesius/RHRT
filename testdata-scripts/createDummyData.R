###-----------------------------------------------------------------------------
### Description ###
# Functions to create dummy data (intervals and annotations) including VPCSs.
# Used to create testdata for the RHRT package.
###-----------------------------------------------------------------------------

#-------------------------------------------------------------------------------
### Constants / Variables

# loads constants used for the RHRT package
# needed for c_numPreRRs, c_numPostRRs and c_normIL
source("../RHRT/R/constants.R")

# template of a standard VPCS (without preRRs) as percentages to rearrange 
# normal intervals
HRTPattern <- c(0.6, 1.4, # couplI and compI
                rep(0.9, 2), # first two parameters, fast HR increase
                seq(0.9, 1.1, length.out = 6), # slow HR decrease
                seq(1.1, 1, length.out = 7)) # return to original HR

#-------------------------------------------------------------------------------
### Functions

#' Creates interval data
#'
#' Wrapper function for rnorm to create a number of intervals with noise
#'
#' @param n numeric, number of intervals to be created
#' @inheritParams createDummyData
#' @return 
createIntervals <- function(n, avRR, sd) {
  rnorm(n, mean = avRR, sd = sd)
}

#' Creates VPCSs
#'
#' Calls createIntervals to 
#'
#' @param n numeric, number of intervals to be created
#' @inheritParams createDummyData
#' @return 
createVPCS <- function(numPreRRs, numPostRRs, avRR, sd) {
  VPCS <- c(
    createIntervals(numPreRRs, avRR, sd),
    createIntervals(17, avRR, sd)*HRTPattern,
    if(numPostRRs > 15) createIntervals(numPostRRs-15, avRR, sd)
  )
  return(VPCS)
}

#' Creates dummy interval or annotation data
#'
#' This function creates 
#' 1) either a number of intervals including a given number of valid VPCSs that 
#' can be found by RHRT's vectorToHRT function, or
#' 2) a corresponding annotation vector with "N" used for sinus intervals and 
#' "V" for PVCs.
#' The parameter "ann" switches between interval and annotation data. If you
#' want to create a corresponding annotation vector to your interval data, keep
#' all other parameters and just set ann = TRUE.
#'
#' @param length numeric, number of intervals to be created overall
#' @param numVPCs numeric, number of VPCSs to be created
#' @param numPreRRs numeric, number of regular intervals before the coupling interval
#' @param numPostRRs numeric, number of regular intervals after the compensatory interval
#' @param avRR numeric, mean length of intervals (in ms)
#' @param sd numeric, standard deviation of the intervals (used for rnorm)
#' @param ann Boolean, should a vector of annotations be created? 
#' 
#' @return numeric or character vector
createDummyData <- function(length, numVPCSs, numPreRRs = c_numPreRRs, numPostRRs = c_numPostRRs, avRR = c_normIL, sd = avRR/100, ann = FALSE) {
  
  lengthVPCS <- numPreRRs+numPostRRs+2
  if(length < numVPCSs*lengthVPCS) {
    warning("The given length of the dummy data is to short for the amount of VPCSs to be included! Returning NULL!")
    return(NULL)
  }
  
  numIntervalsBetween <- (length-lengthVPCS*numVPCSs) %/% (numVPCSs+1) # calculates the number of intervals needed between the VPCSs
  modu <- (length-lengthVPCS*numVPCSs) %% (numVPCSs+1) # calculates modulo to fill the whole length of intervals
  
  if(ann) {
    res <- c( 
      rep(
        c(rep("N", numIntervalsBetween+numPreRRs), "V", rep("N", numPostRRs+1)),
        numVPCSs),
      rep("N", numIntervalsBetween+modu))
    return(res)
  }
  
  res <- c( sapply(seq(1,numVPCSs), function(x) c(createIntervals(numIntervalsBetween, avRR, sd), createVPCS(numPreRRs, numPostRRs, avRR, sd))), # normal intervals + VPCS
            createIntervals(numIntervalsBetween+modu, avRR, sd) # remaining normal intervals
  )
  return(res)
}
