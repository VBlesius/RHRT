#' Convert a vector to HRTList
#' 
#' Scans for heart rate turbulence in a vector of RR-intervals and returns an
#' HRTList object including all found HRT objects.
#' The HRT criteria used were published by Schmidt et al.
#' (more information can be found in the vignette.)
#' 
#' @param input Numeric vector
#' @param annotations Alphabetical vector
#' @param PVCAnn Character
#' @param numPreRRs Numeric
#' @param numPostRRs Numeric 
#' @inheritParams calcHRTParams
#' @return HRTList HRTList object
#' 
#' @export
vectorToHRT <- function(input, annotations = NULL, PVCAnn = "V", normIL = c_normIL, numPreRRs = c_numPreRRs, numPostRRs = c_numPostRRs) {
    numPreRRs <- numPreRRs + 1
    numPostRRs <- numPostRRs + 1
    numSeq <- numPreRRs + numPostRRs + 2
  
      if (is.list(input)) 
        input <- unlist(input)
    checkInput(input, numSeq)
    
    if(!is.null(annotations)) {
      if (is.list(annotations)) 
        annotations <- unlist(annotations)
      checkAnnotations(annotations, input, PVCAnn)
    }

    tempHRTList <- getHRTs(input, annotations, PVCAnn, numPreRRs, numPostRRs, numSeq)
    if (length(tempHRTList@HRTs) == 0) {
        warning("No HRTs found in your data!")
    } else {
        tempHRTList@IL <- mean(input)
        tempHRTList@HRTs <- lapply(tempHRTList@HRTs, calcHRTParams, IL = tempHRTList@IL, normIL)
        tempHRTList@avHRT <- calcAvHRT(tempHRTList)
        tempHRTList@RMSSD <- sqrt(mean(diff(input)^2))
    }
    return(tempHRTList)
}

# -------------------------------------------------------------------------------
#' Checks data input for compatibility
#' 
#' @param input Numeric vector
#'
checkInput <- function(input, numSeq) {
    if (is.null(input)) {
        stop("Given data is NULL! Please make sure your input is of type vector and not empty.")
    }
    if (!is.vector(input)) {
        stop("Given data has not the right type! Please make sure your input is of type vector.")
    }
    if (!is.numeric(input)) {
        stop("Given RR vector is not numeric!")
    }
    if (NA %in% input) {
        stop("Given vector includes NA! Please make sure to remove them before using vectorToHRT!")
    }
    if (any(sapply(input, `<=`, 0))) {
        stop("Given vector includes zero or negative values! Is your data incorrect?")
    }
  
    input = input[2:length(input)]
  
    if (length(input) < numSeq) {
        stop(paste("Your vector is too short! Please consider the number of intervals has to be at least ", 
            numSeq, "."))
    }
    if (mean(input) < 1 || mean(input) > 2000) {
        stop("Did you consider the unit of your data has to be milliseconds? Please adapt your data and try again.")
    }
}

# -------------------------------------------------------------------------------
#' Checks annotations for compatibility
#' 
#' @param annotations Alphabetical vector
#' @param input Numeric vector
#' @param PVCAnn Character
#'
checkAnnotations <- function(annotations, input, PVCAnn) {
  if (is.null(annotations)) {
    stop("Given data is NULL! Please make sure your annotations are of type vector and not empty.")
  }
  if (!is.vector(annotations)) {
    stop("Given annotations do not have the right type! Please make sure your annotations are of type vector.")
  }
  if (!is.character(annotations)) {
    stop("Given annotation vector is not alphabetical!")
  }
  if (NA %in% annotations) {
    stop("Given vector includes NA! Please make sure to remove them before using vectorToHRT!")
  }
  if(length(annotations) != length(input)) {
    stop("The lengths of given annotation and RR vectors differ!")
  }
  if(!PVCAnn %in% annotations) {
    warning("The given annotation for PVCs could not be found in your annotation vector!")
  }
}

# -------------------------------------------------------------------------------
#' Finds HRTs
#'
#' Scans for HRTs in the given vector and returns an HRTList object.
#'
#' @param intervals Numeric vector
#' @param annotations Alphabetical vector
#' @param PVCAnn Character
#' @return HRTListObj
#' 
getHRTs <- function(intervals, annotations = NULL, PVCAnn = "V", numPreRRs = c_numPreRRs, numPostRRs = c_numPostRRs, numSeq) {
  
  hrts <-
    if (is.null(annotations)) {
      wapply(intervals, numSeq, by = 1, FUN = checkForHRT, numPreRRs, numPostRRs)
    } else {
      PVCIndices <- which(annotations == PVCAnn)
      PVCIndices <- PVCIndices[PVCIndices > numPreRRs & PVCIndices < length(annotations)-numPostRRs]
      sapply(PVCIndices, function(PVCIndex) checkForHRT(intervals[(PVCIndex-numPreRRs):(PVCIndex+numPostRRs+1)], numPreRRs, numPostRRs))
    }
  
  indices <- which(sapply(hrts, is.null) != TRUE)
  pos <- 
    if (is.null(annotations)) {
      indices + numPreRRs
    }  else {
      PVCIndices[indices]
    }
  
  tempHRTList <- new("HRTList")
  tempHRTList@pos <- pos
  tempHRTList@HRTs <- hrts[indices]  # removes NULL entries
  return(tempHRTList)
}

# -------------------------------------------------------------------------------
#' Checks RR-intervals for HRT criteria and returns an HRT object
#'
#' @param intervals Numeric vector
#' @return HRT A single HRT object
#' 
checkForHRT <- function(intervals, numPreRRs = c_numPreRRs, numPostRRs = c_numPostRRs) {
    # Defines    
    # coupling,
    # compensatory,
    # preceding
    # and
    # following
    # intervals
    # and
    # sums
    # up
    # regular
    # intervals
    couplRR <- intervals[numPreRRs + 1]
    compRR <- intervals[numPreRRs + 2]
    preRRs <- intervals[1:numPreRRs]
    postRRs <- intervals[(numPreRRs + 3):length(intervals)]
    regRR <- c(preRRs, postRRs)
    
    # Reference
    # interval
    ref <- mean(preRRs)
    
    ## Filtering
    ## methods
    ## checks
    ## for
    ## HRT
    isCouplRR <- couplRR <= ref * 0.8
    isCompRR <- compRR >= ref * 1.2
    # checks
    # for
    # arrhythmias
    # and
    # artefacts
    isInRange <- all(regRR > 300 && regRR < 2000)
    
    if(isCouplRR & isCompRR & isInRange) {
    
      isNotDeviating <- all(
        regRR >= ref * 0.8,
        regRR <= ref * 1.2,
        diff(preRRs) <= 200,
        diff(postRRs) <= 200
      )
    
      # Saves
      # HRT
      # as
      # object
      if (isNotDeviating) {
          tempHRT <- new("HRT", couplRR = couplRR, compRR = compRR, 
              preRRs = tail(preRRs, numPreRRs-1), postRRs = postRRs[1:(numPostRRs-1)])
          return(tempHRT)
      }
    }
}