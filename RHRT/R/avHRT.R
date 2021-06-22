#' S4 class to represent an avHRT object
#'
#' This class extends the HRT class. An avHRT is the average of an HRTList and
#' saves the way in which it was calculated.
#'
#' @slot av (Function) Type of averaging, either mean or median
#' @slot orTO (Character) Order in which TO was calculated,
#' either "avAfter" (assessment of parameter and averaging)
#' or "avBefore" (averaging of the VPCSs and assessment of parameter)
#' @slot orTS (Character) Order in which TS was calculated,
#' either "avAfter" (assessment of parameter and averaging)
#' or "avBefore" (averaging of the VPCSs and assessment of parameter)
#' @slot pTO (Numeric) p-value of t-test checking the validity of TO
#' @slot pTS (Numeric) p-value of t-test checking the validity of TS
#' @slot pTT (Numeric) p-value of t-test checking the validity of TT
#' @slot pnTS (Numeric) p-value of t-test checking the validity of normalised TS
#' @param nRMSSD (Numeric) RMSSD normalised to HR
#'
#' @name avHRT
#'
#' @importFrom methods setMethod
#' @include HRT.R
setClass("avHRT",
  contains = "HRT",
  slots = list(
    av = "function",
    orTO = "character",
    orTS = "character",
    pTO = "numeric",
    pTS = "numeric",
    pTT = "numeric",
    pnTS = "numeric",
    nRMSSD = "numeric"
  ),
  validity = function(object) {
    if (
      any(
        !identical(av, mean) && !identical(av, stats::median),
        length(object@orTO) != 1,
        length(object@orTS) != 1
      )) {
      stop("The given numbers for the avHRT object are incorrect!")
    }
  }
)

#-------------------------------------------------------------------------------
#' @param .Object The name of the class
#' @param av (Function) Type of averaging, either mean or median
#' @param orTO (Character) Order in which TO was calculated,
#' either "avAfter" (assessment of parameter and averaging)
#' or "avBefore" (averaging of the VPCSs and assessment of parameter)
#' @param orTS (Character) Order in which TS was calculated,
#' either "avAfter" (assessment of parameter and averaging)
#' or "avBefore" (averaging of the VPCSs and assessment of parameter)
#' @param pTO (Numeric) p-value of t-test checking the validity of TO
#' @param pTS (Numeric) p-value of t-test checking the validity of TS
#' @param pTT (Numeric) p-value of t-test checking the validity of TT
#' @param pnTS (Numeric) p-value of t-test checking the validity of normalised TS
#' @param nRMSSD (Numeric) RMSSD normalised to HR
#' @inheritParams HRT
#' 
#' @return (avHRT) A new avHRT object
#'
#' @rdname avHRT
#' @importFrom methods initialize
#' @export
setMethod(
  "initialize", "avHRT",
  function(.Object, av = mean, orTO = "avAfter", orTS = "avBefore",
           pTO = NA_real_, pTS = NA_real_, pTT = NA_real_, pnTS = NA_real_,
           nRMSSD = NA_real_,
           couplRR = NA_real_, compRR = NA_real_,
           preRRs = NA_real_, postRRs = NA_real_) {
    .Object@av <- av
    .Object@orTO <- orTO
    .Object@orTS <- orTS
    .Object@pTO <- pTO
    .Object@pTS <- pTS
    .Object@pTT <- pTT
    .Object@pnTS <- pnTS
    .Object@nRMSSD <- nRMSSD

    .Object <- methods::callNextMethod(.Object, couplRR, compRR, preRRs, postRRs)

    return(.Object)
  }
)
