#' S4 class to represent a list of HRT objects
#'
#' This class specifies an object to save all HRT objects of a given vector. It 
#' also saves an averaged HRT for calculation of the averaged HRT parameters and
#' plotting of all HRTs in a single plot.
#'
#' @slot pos Numeric vector, Positions of premature ventricular complexes in 
#'     given input
#' @slot HRTs List, all HRT objects
#' @slot avHRT HRT, averaged HRT
#' 
#' @note After using \code{vectorToHRT} all slots in the resulting HRTList 
#' object are set. Please do not set them manually since many functions of the 
#' HRTList class rely on valid values assigned to the needed slots.
#' Valid values are positive numerics for the intervals (consider: the unit 
#' should be seconds) and numerics (either positive or negative) for the HRT
#' parameters and ab-line coefficients. 
#' 
#' @export
setClass("HRTList",
         slots = list(
           pos = "vector",
           HRTs = "list",
           avHRT = "HRT"),
)

#-------------------------------------------------------------------------------
#' Get positions of PVCs
#'
#' Returns the positions of all premture ventricular complexes (PVC) and 
#' accordingly the coupling intervals that were found in the given vector 
#' when the HRTList was created.
#' 
#' @param HRTListObj HRTList object
#' 
#' @rdname getPositions
setGeneric("getPositions", function(HRTListObj) {
  standardGeneric("getPositions")
})
#' @rdname getPositions
#' @export
setMethod("getPositions", "HRTList", function(HRTListObj) {
 return(HRTListObj@pos)
})

#-------------------------------------------------------------------------------
#' Get averaged HRT parameters
#'
#' Returns the HRT parameters of the HRTList. Turbulence onset is calculated for
#' each HRT object and then averaged, turbulence slope is calculated via 
#' averaging the intervals of all HRT objects to one HRT object and then 
#' estimating the maximal slope.
#' 
#' @param HRTListObj HRTList object
#' @return Named numeric
#' 
#' @rdname getHRTParamsMean
setGeneric("getHRTParamsMean", function(HRTListObj) {
  standardGeneric("getHRTParamsMean")
})
#' @rdname getHRTParamsMean
#' @export
setMethod("getHRTParamsMean", "HRTList", function(HRTListObj) {
  ts <- HRTListObj@avHRT@TS
  to <- mean(sapply(HRTListObj@HRTs, slot, "TO"))
  paramsMean <- setNames(c(ts, to), c("TS", "TO"))
  return(paramsMean)
})

#-------------------------------------------------------------------------------
#' Get all HRT parameters
#'
#' Returns the HRT parameters of each HRt object in the HRTList.
#' 
#' @param HRTListObj HRTList object
#' @return Matrix
#'
#' @rdname getHRTParamsAll
setGeneric("getHRTParamsAll", function(HRTListObj) {
  standardGeneric("getHRTParamsAll")
})
#' @rdname getHRTParamsAll
#' @export
setMethod("getHRTParamsAll", "HRTList", function(HRTListObj) {
  TS <- lapply(HRTListObj@HRTs, slot, "TS")
  TO <- lapply(HRTListObj@HRTs, slot, "TO")
  params <- cbind(TS, TO)
  return(params)
})

#-------------------------------------------------------------------------------
#' Calculate an averaged HRT object
#'
#' For each index the mean of the intervals across all HRTs in the HRTList
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
#' @param HRTListObj HRTList object
#' @return HRTObj
#' 
#' @rdname calcAvHRT
setGeneric("calcAvHRT", function(HRTListObj) {
  standardGeneric("calcAvHRT")
})
#' @rdname calcAvHRT
#' @export
setMethod("calcAvHRT", "HRTList", function(HRTListObj) {
  
  couplRR <- mean(sapply(HRTListObj@HRTs, slot, "couplRR"))
  compenRR <- mean(sapply(HRTListObj@HRTs, slot, "compenRR"))
  preRRs <- rowMeans(sapply(HRTListObj@HRTs, slot, "preRRs"))
  postRRs <- rowMeans(sapply(HRTListObj@HRTs, slot, "postRRs"))
  
  avHRT <- new("HRT", couplRR = couplRR, compenRR = compenRR,
                 preRRs = preRRs, postRRs = postRRs)
  HRTListObj@avHRT <- calcHRTParams(avHRT)
  
  return(HRTListObj)
})

#-------------------------------------------------------------------------------
#' Plot an HRT object
#' 
#' Plots RR-intervals saved in the HRT object and marks
#' turbulence onset and turbulence slope.
#' 
#' @param x A HRTList
#' @param cropped The detail of the plot. The default cuts off CPI
#'  and CMI and focuses on the HRT parameters. To show all points use FALSE.
#' @param ... Other arguments in tag = value form
#' 
#' @export
setMethod("plot", "HRTList", function(x, cropped = TRUE, ...) {
  plot(x@avHRT, cropped = cropped, ...)
  
  lapply(x@HRTs, function(y) {
    rrs <- getRRs(y)
    lines(seq(1:length(rrs)), rrs, col="grey")
  })
  cat("") # preventing NULL return
})