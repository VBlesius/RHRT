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
#' @export
setClass("HRTList", 
         contains = c("HRT", "avHRT"),
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
#' @rdname getHRTParamsMean
setGeneric("getHRTParamsMean", function(HRTListObj, avTO = mean, avTS = mean, 
                                        orTO = 1, orTS = 2) {
    standardGeneric("getHRTParamsMean")
})
#' @rdname getHRTParamsMean
#' @export
setMethod("getHRTParamsMean", "HRTList", function(HRTListObj, avTO = mean, avTS = mean, orTO = 1, orTS = 2) {
  
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
#' @rdname extractHRTParams
setGeneric("extractHRTParams", function(HRTListObj, sl) {
  standardGeneric("extractHRTParams")
})
#' @rdname extractHRTParams
#' @export
setMethod("extractHRTParams", "HRTList", function(HRTListObj, sl) {
  Params <- lapply(HRTListObj@HRTs, slot, sl)
  return(Params)
})

# -------------------------------------------------------------------------------
#' Get all turbulence onset values
#'
#' Returns the TO values of each HRT object ordered by record in the HRTList.
#' 
#' @param HRTListObj HRTList object
#' @return List
#'
#' @rdname getTOs
setGeneric("getTOs", function(HRTListObj) {
  standardGeneric("getTOs")
})
#' @rdname getTOs
#' @export
setMethod("getTOs", "HRTList", function(HRTListObj) {
  TO <- extractHRTParams(HRTListObj, "TO")
  return(TO)
})

# -------------------------------------------------------------------------------
#' Get all turbulence slope values
#'
#' Returns the TS values of each HRT object ordered by record in the HRTList.
#' 
#' @param HRTListObj HRTList object
#' @param allParams Boolean, Should TS and intercept be given?
#' @return List
#'
#' @rdname getTSs
setGeneric("getTSs", function(HRTListObj, allParams = FALSE) {
  standardGeneric("getTSs")
})
#' @rdname getTSs
#' @export
setMethod("getTSs", "HRTList", function(HRTListObj, allParams = FALSE) {
  if (allParams) {
    TS <- list(
      extractHRTParams(HRTListObj, "TS"),
      extractHRTParams(HRTListObj, "intercept")
      )
  } else {
    TS <- extractHRTParams(HRTListObj, "TS")
  }
  return(TS)
})

# -------------------------------------------------------------------------------
#' Get all turbulence timing values
#'
#' Returns the TT values of each HRT object ordered by record in the HRTList.
#' 
#' @param HRTListObj HRTList object
#' @return List
#'
#' @rdname getTTs
setGeneric("getTTs", function(HRTListObj) {
  standardGeneric("getTTs")
})
#' @rdname getTTs
#' @export
setMethod("getTTs", "HRTList", function(HRTListObj) {
  TT <- extractHRTParams(HRTListObj, "TT")
  return(TT)
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
#' @param av Function, Type of averaging the VPCSs, either mean or median
#' @inheritParams getHRTParamsMean
#' @inheritParams calcHRTParams
#' @return avHRTObj
#' 
#' @rdname calcAvHRT
setGeneric("calcAvHRT", function(HRTListObj, av = mean, orTO = 1, orTS = 2, IL = HRTListObj@IL, normIL = c_normIL) {
    standardGeneric("calcAvHRT")
})
#' @rdname calcAvHRT
#' @export
setMethod("calcAvHRT", "HRTList", function(HRTListObj, av = mean, orTO = 1, orTS = 2, IL = HRTListObj@IL, normIL = c_normIL) {

    if (!identical(av, mean) && !identical(av, median)) {
      warning(paste("Function", av, "for parameter averaging is unknown, falling back to default."))
      av <- mean
    } else if (identical(av, mean)) {
      rowAv <- rowMeans
    } else if (identical(av, median)) {
      rowAv <- matrixStats::rowMedians
    }
  
  if (orTO != 1 && orTO != 2) {
    warning(paste("Value", orTO, "for parameter calculation order is unknown, falling back to default."))
    orTO <- 1
  }
  if (orTS != 1 && orTS != 2) {
    warning(paste("Value", orTS, "for parameter calculation order is unknown, falling back to default."))
    orTS <- 2
  }
  
    couplRR <- av(sapply(HRTListObj@HRTs, slot, "couplRR"))
    compRR <- av(sapply(HRTListObj@HRTs, slot, "compRR"))
    preRRs <- rowAv(sapply(HRTListObj@HRTs, slot, "preRRs"))
    postRRs <- rowAv(sapply(HRTListObj@HRTs, slot, "postRRs"))
    
    avHRT <- new("avHRT", couplRR = couplRR, compRR = compRR,
        preRRs = preRRs, postRRs = postRRs, av = av, orTO = orTO, orTS = orTS)
    
    if (orTO == 1) {
      TOs <- unlist(getTOs(HRTListObj))
      avHRT@TO <- av(TOs)
      }
    if (orTO == 2) {
      avHRT <- calcTO(avHRT)
    }
    
    if (orTS == 1) {
      TSParams <- getTSs(HRTListObj, TRUE)
      avHRT@TS <- av(unlist(TSParams[[1]]))
      avHRT@intercept <- av(unlist(TSParams[[2]]))
      
      TTs <- unlist(getTTs(HRTListObj))
      avHRT@TT <- av(TTs)
      
      if (IL != HRTListObj@IL || normIL != c_normIL)
          HRTListObj@HRTs <- lapply(HRTListObj@HRTs, calcTS, IL, normIL)
      nTSs <- unlist(extractHRTParams(HRTListObj, "nTS"))
      avHRT@nTS <- av(nTSs)
      
      nintercepts <- unlist(extractHRTParams(HRTListObj, "nintercept"))
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
