#' S4 class to represent a avHRT object
#'
#' This class specifies a object to save the lengths of intervals surrounding a
#' premature ventricular beat. It saves the HRT parameters turbulence onset
#' and turbulence slope after calculation as well as the coefficients of an
#' ab-line used for the plot.
#'
#' @slot av Function, type of averaging
#' @slot orTO Numeric, order in which TO was calculated
#' @slot orTS Numeric, order in which TO was calculated

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
#' @param av Function
#' @param orTO Numeric
#' @param orTS Numeric
#' 
#' @rdname avHRT
#' @export
setMethod("initialize", "avHRT",
          function(.Object, av=mean, orTO=NA_real_, orTS=NA_real_,
                   couplRR=NA_real_, compRR=NA_real_,
                   preRRs=NA_real_, postRRs=NA_real_) {
            .Object@av <- av
            .Object@orTO <- orTO
            .Object@orTS <- orTS
            
            .Object <- callNextMethod(.Object, couplRR, compRR, preRRs, postRRs)
            
            return(.Object)
          }
)