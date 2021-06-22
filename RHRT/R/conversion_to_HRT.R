#' Convert a vector to HRTList
#'
#' Scans for heart rate turbulence in a vector of RR-intervals and returns an
#' HRTList object including all found HRT objects.
#' The HRT criteria used were published by Schmidt et al.
#' (more information can be found in the vignette.)
#'
#' @param input (Numeric vector) RR intervals in ms
#' @param annotations (Character vector) Annotations matching input
#' @param PVCAnn (Character) Character that marks a VPC in the annotations
#' @param normHallstrom (Boolean) Should the normalisation of Hallstrom be used?
#' @param numPreRRs (Numeric) Number of RRs before the coupling interval that are used for filtering
#' @param numPostRRs (Numeric) Number of RRs after the compensatory interval that are used for filtering
#' @param inputName (String) Name of the data
#' @param minHRT (Numeric) Minimal number of HRTs that are needed to create an HRTList object
#' @param cleaning (Boolean) Should the input be roughly cleaned from artefacts before calculating IL and RMSSD?
#' @inheritParams calcHRTParams
#' @return (HRTList) An HRTList object
#' 
#' @examples
#' # You can use annotations to give the VPC indices
#' # Without annotation data RHRT will find VPCs based on common filtering criteria
#' vectorToHRT(testdataLong, annotations = testdataLong_Ann, PVCAnn = "V")
#' 
#' # Find HRTs with a broader range of sinus beats before and after the VPCs
#' vectorToHRT(testdataLong, inputName = "Dummy Measurement", numPreRRs = 10, numPostRRs = 20)
#'  
#' # Adjust the normalisation parameters
#' vectorToHRT(testdataLong, testdataLong_Ann, normHallstrom = FALSE, normIL = 900)
#'
#' @export
vectorToHRT <- function(input, annotations = NULL, PVCAnn = "V",
                        normIL = c_normIL, normHallstrom = TRUE,
                        numPreRRs = c_numPreRRs, numPostRRs = c_numPostRRs,
                        inputName = as.character(NA),
                        minHRT = 5, cleaning = TRUE) {
  # checking and setting num parameters
  if (numPreRRs < 2) stop("The parameter numPreRRs has to be at least 2, because two intervals are needed before the coupling interval to calculate TO.")
  if (numPostRRs < 5) stop("The parameter numPostRRs has to be at least 5, because five intervals are needed after the compensatory interval to calculate TS.")

  numPreRRs <- numPreRRs + 1
  numPostRRs <- numPostRRs + 1
  numSnippet <- numPreRRs + numPostRRs + 2

  # checking input and annotations
  label <- if (is.na(inputName) || is.null(inputName)) NULL else c(inputName, ": ") # needed for error messages in checkInput and checkAnnotations

  if (is.list(input)) {
    input <- unlist(input)
  }
  checkInput(input, numSnippet, label)

  if (!is.null(annotations)) {
    if (is.list(annotations)) {
      annotations <- unlist(annotations)
    }
    checkAnnotations(annotations, input, PVCAnn, label)
  }

  # finds VPCS, calculates HRT and creates a HRTList
  inputCleaned <- if (cleaning) cleanInput(input) else input
  tempHRTList <- getHRTs(input, annotations, PVCAnn, numPreRRs, numPostRRs, numSnippet)
  numFound <- length(tempHRTList@HRTs)
  if (numFound == 0) {
    warning("No HRTs found in your data!")
    tempHRTList <- methods::new("HRTList")
  } else if (numFound < minHRT) {
    warning(paste("Too few HRTs found in your data:", numFound, "! To calculate HRT anyhow, try again with lower 'minHRT'."))
    tempHRTList <- methods::new("HRTList")
  } else {
    tempHRTList@IL <- mean(inputCleaned)
    tempHRTList@HRTs <- lapply(tempHRTList@HRTs, calcHRTParams, IL = tempHRTList@IL, normIL)
    tempHRTList@RMSSD <- sqrt(mean(diff(inputCleaned)^2))
    tempHRTList@avHRT <- calcAvHRT(tempHRTList, normHallstrom = normHallstrom, normIL = normIL)
  }
  tempHRTList@name <- inputName
  return(tempHRTList)
}

# -------------------------------------------------------------------------------
#' Checks data input for compatibility
#'
#' @param numSnippet (Numeric) number of RRs in the the HRT snippet
#' @param label (Character) Name of the data given and formatted for output
#' @inheritParams vectorToHRT
#' @return No return value, possibly throws errors/warnings
#'
checkInput <- function(input, numSnippet, label) {
  if (is.null(input)) {
    stop(c(label, "Given data is NULL! Please make sure your input is of type vector and not empty."))
  }
  if (!is.vector(input)) {
    stop(c(label, "Given data has not the right type! Please make sure your input is of type vector."))
  }
  if (!is.numeric(input)) {
    stop(c(label, "Given RR vector is not numeric!"))
  }
  if (NA %in% input) {
    stop(c(label, "Given vector includes NA! Please make sure to remove them before using vectorToHRT!"))
  }
  if (any(sapply(input, `<=`, 0))) {
    warning(c(label, "Given vector includes zero or negative values! Is your data incorrect?"))
  }
  if (mean(input) < 1) {
    stop(c(label, "Did you consider the unit of your data has to be milliseconds? Please adapt your data and try again."))
  }
  if (length(input) < numSnippet) {
    stop(c(
      label, "Your vector is too short! Please consider the number of intervals has to be at least ",
      numSnippet, "."
    ))
  }
  if (all(input == sort(input))) {
    warning("Your data looks like timestamps. Please set the parameter 'timestamps' accordingly or vectorToHRT can't find VPCSs.")
  }
}

# -------------------------------------------------------------------------------
#' Cleans data input for further checks or calculation
#'
#' @inheritParams checkInput
#' @return (numeric vector) Input vector without possible bias
#'
cleanInput <- function(input) {
  inputNew <- input[input > 300 & input < 2000]
  inputNew <- utils::tail(inputNew, -1) # removes first interval
  inputNew <- inputNew[abs(diff(inputNew)) <= utils::head(inputNew, -1) * 0.2] # removes all intervals that differ more than 20 % of their own value from the next interval
  return(inputNew)
}

# -------------------------------------------------------------------------------
#' Checks annotations for compatibility
#'
#' @inheritParams vectorToHRT
#' @inheritParams checkInput
#' @return No return value, possibly throws errors/warnings
#'
checkAnnotations <- function(annotations, input, PVCAnn, label) {
  if (!is.vector(annotations)) {
    stop(c(label, "Given annotation data does not have the right type! Please make sure your annotations are of type vector."))
  }
  if (!is.character(annotations)) {
    stop(c(label, "Given annotation vector is not alphabetical!"))
  }
  if (NA %in% annotations) {
    stop(c(label, "Given vector includes NA! Please make sure to remove them before using vectorToHRT!"))
  }
  if (length(annotations) != length(input)) {
    stop(c(label, "The lengths of given annotation and RR vectors differ!"))
  }
  if (!PVCAnn %in% annotations) {
    warning(c(label, "The given annotation for PVCs could not be found in your annotation vector."))
  }
}

# -------------------------------------------------------------------------------
#' Finds HRTs
#'
#' Scans for HRTs in the given vector and returns an HRTList object.
#'
#' @param intervals (Numeric vector) RR intervals in ms
#' @param numSnippet (Numeric) Number of RRs in the HRT snippet
#' @inheritParams vectorToHRT
#' @return (HRTList) HRTList with only pos and HRTs set
#'
getHRTs <- function(intervals, annotations = NULL, PVCAnn = "V", numPreRRs = c_numPreRRs, numPostRRs = c_numPostRRs, numSnippet) {
  hrts <-
    if (is.null(annotations)) {
      roll(intervals, numSnippet, checkForHRT, numPreRRs, numPostRRs)
    } else {
      PVCIndices <- which(annotations == PVCAnn)
      PVCIndices <- PVCIndices[PVCIndices > numPreRRs & PVCIndices < length(annotations) - numPostRRs]
      sapply(PVCIndices, function(PVCIndex) checkForHRT(intervals[(PVCIndex - numPreRRs):(PVCIndex + numPostRRs + 1)], numPreRRs, numPostRRs))
    }

  indices <- which(sapply(hrts, is.null) != TRUE)
  pos <-
    if (is.null(annotations)) {
      indices + numPreRRs
    } else {
      PVCIndices[indices]
    }

  tempHRTList <- methods::new("HRTList")
  tempHRTList@pos <- pos
  tempHRTList@HRTs <- hrts[indices] # removes NULL entries
  return(tempHRTList)
}

# -------------------------------------------------------------------------------
#' Checks RR-intervals for HRT criteria and returns an HRT object
#'
#' @inheritParams getHRTs
#' @return (HRT) A single HRT object
#'
checkForHRT <- function(intervals, numPreRRs = c_numPreRRs, numPostRRs = c_numPostRRs) {
  # Defines coupling, compensatory, preceding and following intervals and sums up regular intervals
  couplRR <- intervals[numPreRRs + 1]
  compRR <- intervals[numPreRRs + 2]
  preRRs <- intervals[1:numPreRRs]
  postRRs <- intervals[(numPreRRs + 3):length(intervals)]
  regRR <- c(preRRs, postRRs)

  # Reference interval
  ref <- mean(preRRs)

  # Filtering methods to check for HRT
  isCouplRR <- couplRR <= ref * 0.8
  isCompRR <- compRR >= ref * 1.2
  # checks for arrhythmias and artefacts
  isInRange <- all(regRR > 300 & regRR < 2000)

  if (isCouplRR & isCompRR & isInRange) {
    isNotDeviating <- all(
      regRR >= ref * 0.8,
      regRR <= ref * 1.2,
      diff(preRRs) <= 200,
      diff(postRRs) <= 200
    )

    # Saves HRT as HRT object
    if (isNotDeviating) {
      tempHRT <- methods::new("HRT",
        couplRR = couplRR, compRR = compRR,
        preRRs = utils::tail(preRRs, numPreRRs - 1), postRRs = postRRs[1:(numPostRRs - 1)]
      )
      return(tempHRT)
    }
  }
}
