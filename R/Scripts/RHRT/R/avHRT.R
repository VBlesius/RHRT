#' S4 class to represent an avHRT object
#'
#' This class extends the HRT class. An avHRT is the average of an HRTList and
#' saves the way in which it was calculated.
#' 
#' @slot av Function, Type of averaging, either mean or median
#' @slot orTO Numeric, Order in which TO was calculated, 
#' either 1 (assessment of parameter and averaging)
#' or 2 (averaging of the VPCSs and assessment of parameter)
#' @slot orTS Numeric, Order in which TS was calculated,
#' either 1 (assessment of parameter and averaging)
#' or 2 (averaging of the VPCSs and assessment of parameter)
#' 
#' @name avHRT
#' 
#' @importFrom methods setMethod 
#' @include HRT.R
#' @export
setClass("avHRT",
         contains = "HRT",
         slots = list(
           av = "function",
           orTO = "numeric",
           orTS = "numeric"),

         validity = function(object) {
           if(any(!identical(av, mean) && !identical(av, median),
                  length(object@orTO) != 1,
                  length(object@orTS) != 1)) {
             stop("The given numbers for the avHRT object are incorrect!")
           }
         }
)

#-------------------------------------------------------------------------------
#' @param .Object The name of the class
#' @param av Function, Type of averaging, either mean or median
#' @param orTO Numeric, Order in which TO was calculated, 
#' either 1 (assessment of parameter and averaging)
#' or 2 (averaging of the VPCSs and assessment of parameter)
#' @param orTS Numeric, Order in which TS was calculated,
#' either 1 (assessment of parameter and averaging)
#' or 2 (averaging of the VPCSs and assessment of parameter)
#' @inheritParams HRT
#' 
#' @rdname avHRT
#' @export
setMethod("initialize", "avHRT",
          function(.Object, av=mean, orTO=NA_real_, orTS=NA_real_,
                   couplRR=NA_real_, compRR=NA_real_,
                   preRRs=NA_real_, postRRs=NA_real_, ...) {
            .Object@av <- av
            .Object@orTO <- orTO
            .Object@orTS <- orTS
            
            .Object <- callNextMethod(.Object, couplRR, compRR, preRRs, postRRs, ...)
            
            return(.Object)
          }
)