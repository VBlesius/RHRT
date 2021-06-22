#' S4 class to represent a list of HRT objects
#'
#' This class specifies an object to save all HRT objects of a given vector. It
#' also saves an averaged HRT for calculation of the averaged HRT parameters and
#' plotting of all HRTs in a single plot.
#'
#' @slot name (Character) Name of the vector if given
#' @slot IL (Numeric) Arithmetic mean of the overall interval length of the vector
#' @slot pos (Numeric vector) Positions of premature ventricular complexes in
#'     given input
#' @slot HRTs (List) All HRT objects
#' @slot avHRT (avHRT object) The average of all HRTs
#' @slot RMSSD (Numeric) Square root of the mean of the squared successive
#' differences between adjacent intervals of the whole measurement
#'
#' @note After using \code{vectorToHRT} all slots in the resulting HRTList
#' object are set. Please do not set them manually since many functions of the
#' HRTList class rely on valid values assigned to the needed slots.
#'
#' @name HRTList
#'
#' @include HRT.R
#' @include avHRT.R
setClass("HRTList",
  slots = list(
    name = "character",
    IL = "numeric",
    pos = "numeric",
    HRTs = "list",
    avHRT = "avHRT",
    RMSSD = "numeric"
  )
)

#-------------------------------------------------------------------------------
#' @param .Object (Character) The name of the class
#' @param name (Character) Name of the vector if given
#' @param IL (Numeric) Arithmetic mean of the overall interval length of the vector
#' @param pos (Numeric vector) Positions of premature ventricular complexes in
#'     given input
#' @param HRTs (List) All HRT objects
#' @param avHRT (avHRT object) The average of all HRTs
#' @param RMSSD (Numeric) Square root of the mean of the squared successive
#' differences between adjacent intervals of the whole measurement
#'
#' @return (HRTList) A new HRTList object
#'
#' @rdname HRTList
#' @importFrom methods initialize
#' @importFrom methods new
#' @export
setMethod(
  "initialize", "HRTList",
  function(.Object, name = NA_character_, IL = NA_real_, pos = NA_real_,
           HRTs = list(), avHRT = new("avHRT"), RMSSD = NA_real_) {
    .Object@name <- name
    .Object@IL <- IL
    .Object@pos <- pos
    .Object@HRTs <- HRTs
    .Object@avHRT <- avHRT
    .Object@RMSSD <- RMSSD

    return(.Object)
  }
)

# -------------------------------------------------------------------------------
#' Get positions of PVCs
#'
#' Returns the positions of all ventricular premature complexes (VPCs) and
#' accordingly the coupling intervals that were found in the given vector
#' when the HRTList was created.
#'
#' @param HRTListObj (HRTList object)
#' 
#' @return No return value, possibly throws errors/warnings
#' 
#' @rdname getPositions
setGeneric("getPositions", function(HRTListObj) {
  standardGeneric("getPositions")
})
#' @rdname getPositions
#' @export
setMethod("getPositions", "HRTList", function(HRTListObj) {
  checkValidity(HRTListObj, pos = TRUE)
  return(HRTListObj@pos)
})

# -------------------------------------------------------------------------------
#' Get averaged HRT parameters
#'
#' Returns the HRT parameters of the HRTList. Turbulence onset is calculated for
#' each HRT object and then averaged, turbulence slope is calculated via
#' averaging the intervals of all HRT objects to one HRT object and then
#' estimating the maximal slope.
#'
#' @param HRTListObj HRTList object
#' @param type (String) Determining the amount of output: 'class' gives the HRT class, 'parameter' the parameter values and 'full' additionally the p-values describing parameter reliability
#' @param TT (Boolean) Should TT be given?
#' @param nTS (Boolean) Should the normalised TS (nTS) be given or used for the determination of the HRT class?
#' @param safe (Boolean) Should all values be given regardless of reliability checks? Note, that 'safe' is ignored when the type is 'full'.
#' @param pmax (Numeric) The significance level
#' @param num (Boolean) Should the results be numeric? This forces the results to stay numeric, but sets not reliable values as NA, if 'safe' is TRUE. Forced numeric values cannot be combined with type 'class'.
#' @inheritParams calcAvHRT
#' @return (Named vector, character or numeric) Either HRT classes, HRT parameter values and/or p-values
#'
#' @examples
#' # You need an HRTList
#' hrtl <- vectorToHRT(testdataLong, testdataLong_Ann)
#' 
#' # Get the HRT classes of your HRTList
#' getResults(hrtl)
#' getResults(hrtl, TT = TRUE)
#' 
#' # Get the HRT parameter values of your HRTList
#' getResults(hrtl, type = "parameter", TT = TRUE)
#'
#' @rdname getResults
setGeneric("getResults", function(HRTListObj, type = "class", TT = FALSE, nTS = FALSE, safe = TRUE, pmax = 0.05, num = FALSE, coTO = COTO, coTS = COTS, coTT = COTT) {
  standardGeneric("getResults")
})
#' @rdname getResults
#' @export
setMethod("getResults", "HRTList", function(HRTListObj, type = "class", TT = FALSE, nTS = FALSE, safe = TRUE, pmax = 0.05, num = FALSE, coTO = COTO, coTS = COTS, coTT = COTT) {
  checkValidity(HRTListObj, av = TRUE)

  # checks whether the string given in type is viable
  types <- c("class", "parameter", "full")
  if (!type %in% types) stop(paste0("The given value for 'type' is unknown! Please choose one of the following: ", paste(types, collapse = ", ")))

  # sets needed variables
  av <- HRTListObj@avHRT
  paramNames <- c("TO", if (nTS) "nTS" else "TS", if (TT) "TT")
  pNames <- c("pTO", if (nTS) "pnTS" else "pTS", if (TT) "pTT")
  paramValues <- stats::setNames(c(av@TO, if (nTS) av@nTS else av@TS, if (TT) av@TT), paramNames)
  pValues <- stats::setNames(c(av@pTO, if (nTS) av@pnTS else av@pTS, if (TT) av@pTT), pNames)

  # sets up needed checking function
  isSignificant <- function(p) {
    return(p <= pmax)
  }
  isRisky <- function(val, param) {
    if(param == "TO") return(val > coTO)
    if(param == "TS" || param == "nTS") return(val < coTS)
    if(param == "TT") return(val > coTT)
  }
  concludeResults <- function(val, p) {
    # checks for NA
    if (is.na(val)) {
      return(NA_real_)
    }
    if (is.na(p) && safe) {
      if (num) {
        return(NA_real_)
      } else {
        return("NR")
      }
    }
    # if neither val nor p are NA:
    if (!safe || isSignificant(p)) {
      return(val)
    } else if (num) {
      return(NA_real_)
    } else {
      return("NR")
    }
  }

  # Returning results depending on "type"
  ## full
  if (type == "full") {
    return(c(paramValues, pValues))
  }

  ## saves results as "NR" or NA if not reliable in safe mode, in every other case saves values
  results <- unlist(mapply(concludeResults, paramValues, pValues, SIMPLIFY = TRUE))

  ## parameter
  if (type == "parameter") {
    return(results)
  }

  ## class
  if (type == "class") {
    if (num) {
      warning("The combination of type 'class' and num 'TRUE' is not possible: Returning NA.")
      return(NA_real_)
    }

    if (any(is.na(paramValues))) {
      warning("The HRT parameters contain NA, thus the HRT class cannot be determined: Returning NA.")
      return(NA)
    }
    if (safe && "NR" %in% results) {
      return("NR")
    }

    sig <- sapply(pValues, isSignificant)
    risky <- mapply(isRisky, paramValues, paramNames, SIMPLIFY = TRUE)

    if (TRUE %in% risky && FALSE %in% risky) {
      if (TT) {
        class <- "HRTB"
      } else {
        class <- "HRT1"
      }
    } else if (unique(risky)) {
      if (TT) {
        class <- "HRTC"
      } else {
        class <- "HRT2"
      }
    } else {
      if (TT) {
        class <- "HRTA"
      } else {
        class <- "HRT0"
      }
    }

    return(class)
  }
})

# -------------------------------------------------------------------------------
#' Extracts all values of a special slot out of a HRTList
#'
#' Extracts all values of the given slot in each HRT of the HRTList
#' and returns them in a list
#'
#' @param HRTListObj HRTList object
#' @param sl (Character) Value of a slot saved by an HRT object
#' @return (numeric vector or list) Vector or list of the numerics stored in the given slot
#'
#' @examples
#' # You need an HRTList
#' hrtl <- vectorToHRT(testdataLong, testdataLong_Ann)
#' 
#' # Get all TOs of the HRTs in your HRTList
#' getHRTParams(hrtl, "TO")
#' 
#' # You can access all slots in the HRTs
#' getHRTParams(hrtl, "intercept")
#' 
#' # If you access slots that include more than one numeric, the function returns a list
#' getHRTParams(hrtl, "preRRs")
#' 
#' @rdname getHRTParams
setGeneric("getHRTParams", function(HRTListObj, sl) {
  standardGeneric("getHRTParams")
})
#' @rdname getHRTParams
#' @export
setMethod("getHRTParams", "HRTList", function(HRTListObj, sl) {
  checkValidity(HRTListObj)
  Params <- lapply(HRTListObj@HRTs, methods::slot, sl)
  if(length(Params[[1]]) == 1) {
    return(unlist(Params))
  } else {
    return(Params)
  }
})


# -------------------------------------------------------------------------------
#' Calculate an avHRT object
#'
#' For each index the average of the intervals across all HRTs in the HRTList
#' is calculated and the averaged HRT returned. The type of averaging, the order
#' of HRT parameter assessment and interval lengths for normalising TS can be passed.
#'
#' @details
#' To eliminate other RR variability TS is commonly assessed after averaging the
#' VPCSs. TO is commonly first calculated from the single VPCS and then
#' averaged. (See 'Heart Rate Turbulence: Standards of Measurement,
#' Physiological Interpretation, and Clinical Use, Axel Bauer et al.,
#' Journal of the American College of Cardiology, Volume 52, Issue 17,
#' Pages 1353-1365')
#'
#' @param HRTListObj HRTList object
#' @param av (Function) Type of averaging the VPCSs, either mean or median
#' @param orTO (Character) Order in which TO was calculated,
#' either "avAfter" (assessment of parameter and averaging)
#' or "avBefore" (averaging of the VPCSs and assessment of parameter)
#' @param orTS (Character) Order in which TS was calculated,
#' either "avAfter" (assessment of parameter and averaging)
#' or "avBefore" (averaging of the VPCSs and assessment of parameter)
#' @inheritParams calcHRTParams
#' @param normHallstrom (Boolean) Should the normalisation of Hallstrom be used?
#' @param coTO (Numeric) Cut-off value for TO
#' @param coTS (Numeric) Cut-off value for TS and nTS
#' @param coTT (Numeric) Cut-off value for TT
#' @return (avHRT) The avHRT object of the given HRTList
#'
#' @examples
#' # You need an HRTList
#' hrtl <- vectorToHRT(testdataLong, testdataLong_Ann)
#' 
#' # Recalculate the avHRT with different normalisation
#' calcAvHRT(hrtl, normIL = 1000, normHallstrom = FALSE)
#' 
#' # Recalculate the avHRT based on a different calculation order
#' calcAvHRT(hrtl, orTO = "avBefore", orTS = "avAfter")
#' 
#' # Set custom parameter cut-offs for the reliability check
#' ## You should keep in mind to give the same cut-offs when calling getResults()
#' calcAvHRT(hrtl, coTO = 0.022, coTS = 1.42, coTT = 12)
#'
#' @importFrom methods slot
#' @importFrom stats t.test
#' @rdname calcAvHRT
setGeneric("calcAvHRT", function(HRTListObj, av = mean, orTO = "avAfter", orTS = "avBefore", IL = HRTListObj@IL, normIL = c_normIL, normHallstrom = TRUE, coTO = COTO, coTS = COTS, coTT = COTT) {
  standardGeneric("calcAvHRT")
})
#' @rdname calcAvHRT
#' @export
setMethod("calcAvHRT", "HRTList", function(HRTListObj, av = mean, orTO = "avAfter", orTS = "avBefore", IL = HRTListObj@IL, normIL = c_normIL, normHallstrom = TRUE, coTO = COTO, coTS = COTS, coTT = COTT) {
  checkValidity(HRTListObj)

  # sets the type of averaging
  if (!identical(av, mean) && !identical(av, stats::median)) {
    warning(paste("Function", as.character(substitute(av)), "for parameter averaging is unknown, falling back to default."))
    av <- mean
    rowAv <- rowMeans
  } else if (identical(av, mean)) {
    rowAv <- rowMeans
  } else if (identical(av, stats::median)) {
    rowAv <- matrixStats::rowMedians
  }

  # checks the calculation order and sets it to default if necessary
  if (is.na(orTO) || orTO != "avAfter" && orTO != "avBefore") {
    warning(paste("Value", orTO, "for parameter calculation order is unknown, falling back to default. Please use 'avAfter' or 'avBefore'."))
    orTO <- "avAfter"
  }
  if (is.na(orTS) || orTS != "avAfter" && orTS != "avBefore") {
    warning(paste("Value", orTS, "for parameter calculation order is unknown, falling back to default. Please use 'avAfter' or 'avBefore'."))
    orTS <- "avBefore"
  }

  # checks remaining parameters
  if (any(!is.finite(c(IL, normIL, coTO, coTS, coTT)))) stop("Values for normalisation or cut-offs are not numeric.")

  # calculates the mean intervals
  couplRR <- av(sapply(HRTListObj@HRTs, slot, "couplRR"))
  compRR <- av(sapply(HRTListObj@HRTs, slot, "compRR"))
  preRRs <- rowAv(sapply(HRTListObj@HRTs, slot, "preRRs"))
  postRRs <- rowAv(sapply(HRTListObj@HRTs, slot, "postRRs"))

  # initializes the avHRT object
  avHRT <- methods::new("avHRT",
    couplRR = couplRR, compRR = compRR,
    preRRs = preRRs, postRRs = postRRs, av = av, orTO = orTO, orTS = orTS,
    nRMSSD = HRTListObj@RMSSD * normIL / IL
  )

  # calculates parameters for every VPC separately
  TOs <- getHRTParams(HRTListObj, "TO")
  TSs <- getHRTParams(HRTListObj, "TS")
  TTs <- getHRTParams(HRTListObj, "TT")
  # calculates nTS if different IL or normIL are given
  if (IL != HRTListObj@IL || normIL != c_normIL) {
    hrts <- lapply(HRTListObj@HRTs, calcTS, normalising = TRUE, IL = IL, normIL = normIL)
    nTSs <- sapply(hrts, slot, "nTS")
    nintercepts <- sapply(hrts, slot, "nintercept")
    warning("The IL or normIL given is different to the ones used to calculate
            the HRT objects. Therefore, the values of nTS and nintercept of the
            HRT objects and avHRT do not match anymore.")
  } else {
    nTSs <- getHRTParams(HRTListObj, "nTS")
    nintercepts <- getHRTParams(HRTListObj, "nintercept")
  }


  notconstant <- function(x) !length(unique(x)) == 1
  # calculates p-values
  if (notconstant(TOs)) avHRT@pTO <- t.test(TOs, alternative = "less", mu = coTO)$p.value
  if (notconstant(TSs)) avHRT@pTS <- t.test(TSs, alternative = "greater", mu = coTS)$p.value
  if (notconstant(TTs)) avHRT@pTT <- t.test(TTs, alternative = "less", mu = coTT)$p.value
  if (notconstant(nTSs)) avHRT@pnTS <- t.test(nTSs, alternative = "greater", mu = coTS)$p.value

  # calculates TO, first in default order, secondly from an averaged tachogram
  if (orTO == "avAfter") {
    avHRT@TO <- av(TOs)
  }
  if (orTO == "avBefore") {
    avHRT <- calcTO(avHRT)
  }

  # calculates TS, first for every VPC seperately, secondly in default order
  # additionally saves TT and intercept

  # Function to normalise TS after Hallstrom
  hallstromise <- function(ts, avHRT, HRTListObj) {
    k <- length(avHRT@postRRs)
    numVPCs <- length(HRTListObj@pos)
    TSk <- 0.02475 * (k - 2)^0.9449 * avHRT@nRMSSD / sqrt(numVPCs)
    nTS <- avHRT@nTS - TSk
  }

  # sets averaged parameters
  if (orTS == "avAfter") {
    # TS+intercept
    avHRT@TS <- av(TSs)
    avHRT@intercept <- av(getHRTParams(HRTListObj, "intercept"))

    # TT
    avHRT@TT <- av(TTs)

    # normalised TS+intercept if normalising parameters are given
    nTS <- av(nTSs)
    if (normHallstrom) avHRT@nTS <- hallstromise(nTS, avHRT, HRTListObj)
    avHRT@nintercept <- av(nintercepts)
  }
  if (orTS == "avBefore") {
    avHRT <- calcTS(avHRT)
    avHRT <- calcTS(avHRT, normalising = TRUE, IL, normIL)
    if (normHallstrom) avHRT@nTS <- hallstromise(avHRT@nTS, avHRT, HRTListObj)
  }
  return(avHRT)
})

# -------------------------------------------------------------------------------
#' Plot an HRTList object
#'
#' Plots RR-intervals saved in the HRT objects, especially the avHRT object,
#' and marks the HRT parameters.
#'
#' @param x HRTList
#' @param cropped (Boolean) Should the plot be cut to focus on the HRT parameters?
#' To show all points use FALSE.
#' @param TT (Boolean) Should Turbulence timing be marked?
#' @param pch (Numeric) Plotting character, for other options see graphics::var
#' @param xlab (Character) Label for the x axis
#' @param ylab (Character) Label for the y axis
#' @param paramsLegend (Boolean) Should the parameter values of the HRT be plotted?
#' @param colTO (Character) Colour used to highlight TO
#' @param colTS (Character) Colour used to highlight TS
#' @param colTT (Character) Colour used to highlight TT
#' @param ... Other arguments in tag = value form
#' 
#' @return No return value
#' 
#' @examples
#' # You need an HRTList
#' hrtl <- vectorToHRT(testdataLong, testdataLong_Ann)
#' 
#' # Plot your HRTList and zoom out
#' plot(hrtl, cropped = FALSE)
#' 
#' # Include TT and customise it
#' plot(hrtl, TT = TRUE, colTT = "green", pch = 7)
#' 
#' # Use standard graphics parameters
#' ## Note: Some parameters are used inside the function and cannot be set
#' plot(hrtl, TT = TRUE, main = "Example plot", bty = "n", cex.lab = 1.2)
#' 
#' @note Please note that some graphics parameters (par) cannot be modified,
#'  since they are needed to be set inside the function.
#'
#' @export
setMethod("plot", "HRTList", function(x,
                                      cropped = TRUE,
                                      TT = FALSE,
                                      pch = 20,
                                      xlab = "# of RR interval",
                                      ylab = "length of RR interval (ms)",
                                      paramsLegend = TRUE,
                                      colTO = "#ec2023",
                                      colTS = "#006AFF",
                                      colTT = "#6800DE",
                                      ...) {
  plot(x@avHRT,
    cropped = cropped, TT = TT, pch = pch, xlab = xlab, ylab = ylab,
    paramsLegend = paramsLegend, colTO = colTO, colTS = colTS, colTT = colTT,
    add = FALSE, ...
  )
  avPivot <- utils::tail(x@avHRT@preRRs, n = 1)

  lapply(x@HRTs, function(y) {
    rrs <- getRRs(y)
    pivot <- utils::tail(y@preRRs, n = 1)
    diff <- avPivot - pivot
    rrs <- rrs + diff
    graphics::lines(seq(1:length(rrs)), rrs, col = "grey")
  })

  plot(x@avHRT,
    cropped = cropped, TT = TT, pch = pch, xlab = xlab, ylab = ylab,
    paramsLegend = paramsLegend, colTO = colTO, colTS = colTS, colTT = colTT,
    add = TRUE, ...
  )
})

# -------------------------------------------------------------------------------
#' Check for HRTList class
#'
#' @param x HRTList
#' @param av (Boolean) Should avHRT be checked?
#' @param pos (Boolean) Should pos be checked?
#' @return No return value, possibly throws errors
#'
#' @rdname checkValidity
setMethod("checkValidity", "HRTList", function(x, av = FALSE, pos = FALSE) {
  if (length(x@HRTs) == 0 || (length(x@HRTs) == 1 && is.na(x@HRTs[[1]]@couplRR))) {
    stop("The HRTList does not contain any HRTs")
  }

  if (av) {
    if (identical(x@avHRT, methods::new("avHRT"))) {
      stop("The average HRT is empty. Make sure to calculate it and try again.")
    }
  }

  if (pos) {
    if (any(is.na(x@pos)) || length(x@pos) == 0) {
      stop("There seem to be no HRTs in your HRTList.")
    }
  }
})
