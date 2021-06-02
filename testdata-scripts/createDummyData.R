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
#' Multiplies a part of a given numeric vector with HRTPattern. The first index
#' is numPreRRs + 2, because RHRT needs one interval more than preRRs to find
#' a VPCS (some filter rules need preceding intervals).
#'
#' @param n numeric, number of intervals to be created
#' @inheritParams createDummyData
#' @return 
createVPCS <- function(chunk, numPreRRs) {
  iStart <- numPreRRs+2
  iEnd <- iStart+16 #length of HRTPattern -1
  chunk[iStart:iEnd] <- chunk[iStart:iEnd]*HRTPattern
  return(chunk)
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
  
  # Test whether parameters fit together
  lengthVPCS <- numPreRRs+numPostRRs+2
  if(length < numVPCSs*(lengthVPCS+1)) {
    warning("The given length of the dummy data is to short for the amount of VPCSs to be included! The length must be bigger than numVPCSs*(lengthVPCS+1). Returning NULL!")
    return(NULL)
  }
 
  # creates basic data
   if(ann) {
    data <- rep("N", length)
  } else {
    data <- createIntervals(length, avRR = avRR, sd = sd)
  }

  # returns data without any VPCs
  if(numVPCSs < 1) return(data)
  
  # cuts data into chunks
  lengthChunks <- length/numVPCSs
  chunks <- split(data, ceiling(seq_along(data)/lengthChunks))
  
  # adds VPC to every chunk
  if(ann) {
    chunks <- lapply(chunks, replace, 7, "V")
  } else {
    chunks <- lapply(chunks, createVPCS, numPreRRs)
  }
  
  res <- Reduce(c, chunks)
  return(res)
}
