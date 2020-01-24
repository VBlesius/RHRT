#' S4 class to represent a list of HRT objects
#'
#' This class specifies an object to save all HRT objects of a given vector. It 
#' also saves an averaged HRT for calculation of the averaged HRT parameters and
#' plotting of all HRTs in a single plot.
#'
#' @slot IL Numeric, Arithmetic mean of the overall interval length of the vector
#' @slot pos Numeric vector, Positions of premature ventricular complexes in 
#'     given input
#' @slot HRTs List, all HRT objects
#' @slot avHRT avHRT object, the average of all HRTs
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
           IL = "numeric",
           pos = "vector",
           HRTs = "list", 
           avHRT = "avHRT"))

# -------------------------------------------------------------------------------
#' Get positions of PVCs
#'
#' Returns the positions of all ventricular premture complexes (VPCs) and 
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

# -------------------------------------------------------------------------------
#' Get averaged HRT parameters
#'
#' Returns the HRT parameters of the HRTList. Turbulence onset is calculated for
#' each HRT object and then averaged, turbulence slope is calculated via 
#' averaging the intervals of all HRT objects to one HRT object and then 
#' estimating the maximal slope.
#' 
#' @param HRTListObj HRTList object
#' @param avTO Function, Type of averaging for TO, either mean or median
#' @param avTS Function, Type of averaging for TS, either mean or median
#' @param orTO Numeric, Order in which TO was calculated, 
#' either 1 (assessment of parameter and averaging)
#' or 2 (averaging of the VPCSs and assessment of parameter)
#' @param orTS Numeric, Order in which TS was calculated,
#' either 1 (assessment of parameter and averaging)
#' or 2 (averaging of the VPCSs and assessment of parameter)
#' @return Named numeric, HRT parameters TO, TS and TT
#' 
#' @rdname getResults
setGeneric("getResults", function(HRTListObj, type = 1, TT = FALSE, pmax = 0.05, avTO = mean, avTS = mean, orTO = 1, orTS = 2) {
    standardGeneric("getResults")
})
#' @rdname getResults
#' @export
setMethod("getResults", "HRTList", function(HRTListObj, type = 1, pmax = 0.05, avTO = mean, avTS = mean, orTO = 1, orTS = 2) {
  
  if (identical(avTO, HRTListObj@avHRT@av) && identical(orTO, HRTListObj@avHRT@orTO)){
    to <- HRTListObj@avHRT@TO
  } else {
    tempAvHRT <- calcAvHRT(HRTListObj, av = avTO, orTO = orTO)
    to <- tempAvHRT@TO
  }
  
  if (identical(avTS, HRTListObj@avHRT@av) && identical(orTS, HRTListObj@avHRT@orTS)){
    ts <- HRTListObj@avHRT@TS
    tt <- HRTListObj@avHRT@TT
  } else {
    tempAvHRT <- calcAvHRT(HRTListObj, av = avTS, orTS = orTS)
    ts <- tempAvHRT@TS
    tt <- tempAvHRT@TT
  }
 
  paramsMean <- setNames(c(to, ts, tt), c("TO", "TS", "TT"))
  return(paramsMean)
})

# -------------------------------------------------------------------------------
#' Extracts all values of a special slot out of a HRTList
#' 
#' Extracts all values of the given slot in each HRT of the HRTList
#' and returns them in a list
#' 
#' @param HRTListObj HRTList object
#' @param sl Character, choose a slot saved by an HRT object
#' @return List
#'
#' @rdname getHRTParams
setGeneric("getHRTParams", function(HRTListObj, sl) {
  standardGeneric("getHRTParams")
})
#' @rdname getHRTParams
#' @export
setMethod("getHRTParams", "HRTList", function(HRTListObj, sl) {
  Params <- lapply(HRTListObj@HRTs, slot, sl)
  return(Params)
})

#' # -------------------------------------------------------------------------------
#' #' Get all turbulence onset values
#' #'
#' #' Returns the TO values of each HRT object ordered by record in the HRTList.
#' #' 
#' #' @param HRTListObj HRTList object
#' #' @return List
#' #'
#' #' @rdname getTOs
#' setGeneric("getTOs", function(HRTListObj) {
#'   standardGeneric("getTOs")
#' })
#' #' @rdname getTOs
#' #' @export
#' setMethod("getTOs", "HRTList", function(HRTListObj) {
#'   TO <- getHRTParams(HRTListObj, "TO")
#'   return(TO)
#' })
#' 
#' # -------------------------------------------------------------------------------
#' #' Get all turbulence slope values
#' #'
#' #' Returns the TS values of each HRT object ordered by record in the HRTList.
#' #' 
#' #' @param HRTListObj HRTList object
#' #' @param allParams Boolean, Should TS and intercept be given?
#' #' @return List
#' #'
#' #' @rdname getTSs
#' setGeneric("getTSs", function(HRTListObj, allParams = FALSE) {
#'   standardGeneric("getTSs")
#' })
#' #' @rdname getTSs
#' #' @export
#' setMethod("getTSs", "HRTList", function(HRTListObj, allParams = FALSE) {
#'   if (allParams) {
#'     TS <- list(
#'       getHRTParams(HRTListObj, "TS"),
#'       getHRTParams(HRTListObj, "intercept")
#'       )
#'   } else {
#'     TS <- getHRTParams(HRTListObj, "TS")
#'   }
#'   return(TS)
#' })
#' 
#' # -------------------------------------------------------------------------------
#' #' Get all turbulence timing values
#' #'
#' #' Returns the TT values of each HRT object ordered by record in the HRTList.
#' #' 
#' #' @param HRTListObj HRTList object
#' #' @return List
#' #'
#' #' @rdname getTTs
#' setGeneric("getTTs", function(HRTListObj) {
#'   standardGeneric("getTTs")
#' })
#' #' @rdname getTTs
#' #' @export
#' setMethod("getTTs", "HRTList", function(HRTListObj) {
#'   TT <- getHRTParams(HRTListObj, "TT")
#'   return(TT)
#' })

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
#' @param av Function, Type of averaging the VPCSs, either mean or median
#' @inheritParams getHRTParamsMean
#' @inheritParams calcHRTParams
#' @param coTO Numeric, cut-off value for TO
#' @param coTS Numeric, cut-off value for TS and nTS
#' @param coTT Numeric, cut-off value for TT
#' @return avHRTObj
#' 
#' @rdname calcAvHRT
setGeneric("calcAvHRT", function(HRTListObj, av = mean, orTO = 1, orTS = 2, IL = HRTListObj@IL, normIL = c_normIL, coTO = COTO, coTS = COTS, coTT = COTT) {
    standardGeneric("calcAvHRT")
})
#' @rdname calcAvHRT
#' @export
setMethod("calcAvHRT", "HRTList", function(HRTListObj, av = mean, orTO = 1, orTS = 2, IL = HRTListObj@IL, normIL = c_normIL, coTO = COTO, coTS = COTS, coTT = COTT) {

  # sets the type of averaging
    if (!identical(av, mean) && !identical(av, median)) {
      warning(paste("Function", av, "for parameter averaging is unknown, falling back to default."))
      av <- mean
    } else if (identical(av, mean)) {
      rowAv <- rowMeans
    } else if (identical(av, median)) {
      rowAv <- matrixStats::rowMedians
    }
  
  # checks the calculation order and sets it to default if necessary
  if (orTO != 1 && orTO != 2) {
    warning(paste("Value", orTO, "for parameter calculation order is unknown, falling back to default."))
    orTO <- 1
  }
  if (orTS != 1 && orTS != 2) {
    warning(paste("Value", orTS, "for parameter calculation order is unknown, falling back to default."))
    orTS <- 2
  }
  
  # calculates the mean intervals
    couplRR <- av(sapply(HRTListObj@HRTs, slot, "couplRR"))
    compRR <- av(sapply(HRTListObj@HRTs, slot, "compRR"))
    preRRs <- rowAv(sapply(HRTListObj@HRTs, slot, "preRRs"))
    postRRs <- rowAv(sapply(HRTListObj@HRTs, slot, "postRRs"))
    
  # initializes the avHRT object
    avHRT <- new("avHRT", couplRR = couplRR, compRR = compRR,
        preRRs = preRRs, postRRs = postRRs, av = av, orTO = orTO, orTS = orTS)
  
  # calculates parameters for every VPC seperately
    TOs <- unlist(getHRTParams(HRTListObj, "TO"))
    TSs <- unlist(getHRTParams(HRTListObj, "TS"))
    TTs <- unlist(getHRTParams(HRTListObj, "TT"))
    if (IL != HRTListObj@IL || normIL != c_normIL)
      HRTListObj@HRTs <- lapply(HRTListObj@HRTs, calcTS, IL, normIL)
    nTSs <- unlist(getHRTParams(HRTListObj, "nTS"))
    nintercepts <- unlist(getHRTParams(HRTListObj, "nintercept"))
    
    notconstant <- function(x) !length(unique(x)) == 1
    # calculates p-values
    if(notconstant(TOs)) avHRT@pTO <- t.test(TOs, alternative = "less", mu = coTO)$p.value
    if(notconstant(TSs)) avHRT@pTS <- t.test(TSs, alternative = "greater", mu = coTS)$p.value
    if(notconstant(TTs)) avHRT@pTT <- t.test(TTs, alternative = "less", mu = coTT)$p.value
    if(notconstant(nTSs)) avHRT@pnTS <- t.test(nTSs, alternative = "greater", mu = coTS)$p.value    
    
  # calculates TO, first in default order, secondly from an averaged tachogram
    if (orTO == 1) {
      avHRT@TO <- av(TOs)
      }
    if (orTO == 2) {
      avHRT <- calcTO(avHRT)
    }
  
  # calculates TS, first for every VPC seperately, secondly in default order
  # additionally saves TT and intercept 

    # sets averaged parameters
    if (orTS == 1) {
      # TS+intercept
      avHRT@TS <- av(TSs)
      avHRT@intercept <- av(unlist(getHRTParams(HRTListObj, "intercept")))
      
      # TT
      avHRT@TT <- av(TTs)

      # normalised TS+intercept if normalising parameters are given
      avHRT@nTS <- av(nTSs)
      avHRT@nintercept <- av(nintercepts)
    }
    if (orTS == 2) {
      avHRT <- calcTS(avHRT)
      avHRT <- calcTS(avHRT, normalising = TRUE, IL, normIL)
    }
    
    return(avHRT)
})

# -------------------------------------------------------------------------------
#' Plot an HRT object
#' 
#' Plots RR-intervals saved in the HRT object and marks HRT parameters.
#' 
#' @param x HRTList
#' @param cropped Boolean, Should the plot be cut to focuse on the HRT parameters?
#' To show all points use FALSE.
#' @param showTT Boolean, Should Turbulence timing be marked?
#' @param ... Other arguments in tag = value form
#' 
#' @export
setMethod("plot", "HRTList", function(x, cropped = TRUE, showTT = FALSE, ...) {
    plot(x@avHRT, cropped = cropped, showTT = showTT, ...)
    
    lapply(x@HRTs, function(y) {
        rrs <- getRRs(y)
        lines(seq(1:length(rrs)), rrs, col = "grey")
    })
    
    plot(x@avHRT, add = TRUE, cropped = cropped, showTT = showTT, ...)
}) 
