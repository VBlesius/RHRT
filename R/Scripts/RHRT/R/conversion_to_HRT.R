#' Convert a vector to HRTList
#' 
#' Scans for heart rate turbulence in a vector of RR-intervals and returns an
#' HRTList object including all found HRT objects.
#' The HRT criteria used were published by Schmidt et al.
#' (more information can be found in the vignette.)
#' 
#' @param input Numeric vector
#' @return HRTList HRTList object
#' 
#' @export
vectorToHRT <- function(input) {
    if (is.list(input)) 
        input <- unlist(input)
    checkInput(input)
    
    tempHRTList <- getHRTs(input)
    if (length(tempHRTList@HRTs) == 0) {
        warning("No HRTs found in your data!")
    } else {
        tempHRTList@HRTs <- lapply(tempHRTList@HRTs, calcHRTParams)
        tempHRTList <- calcAvHRT(tempHRTList)
    }
    return(tempHRTList)
}

# -------------------------------------------------------------------------------
#' Checks data input for compatibility
#' 
#' @param input Numeric vector
#'
checkInput <- function(input) {
    if (is.null(input)) {
        stop("Given data is NULL! Please make sure your input is of type vector and not empty.")
    }
    if (!is.vector(input)) {
        stop("Given data has not the right type! Please make sure your input is of type vector.")
    }
    if (!is.numeric(input)) {
        stop("Given vector is not numeric!")
    }
    if (any(sapply(input, is.na))) {
        stop("Given vector includes NA! Please make sure to remove them before using vectorToHRT!")
    }
    if (any(sapply(input, `<=`, 0))) {
        stop("Given vector includes zero or negative values! Is your data incorrect?")
    }
  
    input = input[2:length(input)]
  
    if (length(input) < numSeq) {
        warning(paste("Your vector is too short! Please consider the number of intervals has to be at least ", 
            numSeq, "."))
    }
    if (mean(input) < 1 || mean(input) > 1000) {
        warning("Did you consider the unit of your data has to be seconds? Please adapt your data and try again.")
    }
}

# -------------------------------------------------------------------------------
#' Finds HRTs
#'
#' Scans for HRTs in the given vector and returns a HRTList object.
#'
#' @param intervals Numeric vector
#' @return HRTList
#' 
getHRTs <- function(intervals) {
    
    hrts <- wapply(intervals, numSeq, by = 1, FUN = checkForHRT)
    indices <- which(sapply(hrts, is.null) != TRUE)
    
    tempHRTList <- new("HRTList")
    tempHRTList@pos <- indices + numPreRRs
    tempHRTList@HRTs <- hrts[indices]  # removes NULL entries
    return(tempHRTList)
}

# -------------------------------------------------------------------------------
#' Checks specified number of RR-intervals for HRT criteria
#' and returns a HRT object
#'
#' @param intervals Numeric vector
#' @return HRT A single HRT object
#' 
checkForHRT <- function(intervals) {
    # Defines    # coupling,
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
    compenRR <- intervals[numPreRRs + 2]
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
    isCompenRR <- compenRR >= ref * 1.2
    # checks
    # for
    # arrhythmias
    # and
    # artefacts
    isInRange <- all(regRR > 300 && regRR < 2000)
    isNotDeviating <- all(regRR >= ref * 0.8, regRR <= ref * 
        1.2, wapply(preRRs, 2, by = 1, FUN = function(x) diff(x) <= 
        200), wapply(postRRs, 2, by = 1, FUN = function(x) diff(x) <= 
        200))
    
    # Checks
    # for
    # criteria
    # and
    # saves
    # HRT
    # as
    # object
    if (isCouplRR & isCompenRR & isInRange & isNotDeviating) {
        tempHRT <- new("HRT", couplRR = couplRR, compenRR = compenRR, 
            preRRs = preRRs[-(numPreRRs - 2):0], postRRs = postRRs[1:15])
        return(tempHRT)
    }
} 
