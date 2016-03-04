#' S4 class to represent a HRT object
#'
#' This class specifies a object to save the lengths of intervals surrounding a
#' premature ventricular beat. It saves the HRT parameters turbulence onset
#' and turbulence slope after calculation as well as the coefficients of an
#' ab-line used for the plot.
#'
#' @slot couplRR Numeric, Coupling interval
#' @slot compenRR Numeric, Compensatory interval
#' @slot preRRs Numeric vector, Preceding 6 intervals
#' @slot postRRs Numeric vector, Following 16 intervals
#' @slot TO Numeric, Turbulence onset
#' @slot TS Numeric, Turbulence slope
#' @slot ablineCoefficients Numeric vector, Intercept and slope of ab-line
#' 
#' @rdname HRT
#' 
#' @importFrom methods setMethod 
#' @export
setClass("HRT",
         slots = list(
           couplRR = "numeric",
           compenRR = "numeric",
           preRRs = "vector",
           postRRs = "vector",
           TO = "numeric",
           TS = "numeric",
           ablineCoefficients = "vector"),

         validity = function(object) {
           if(any(length(object@couplRR) != 1,
                  length(object@compenRR) != 1,
                  length(object@preRRs) != numPreRRs,
                  length(object@postRRs) != numPostRRs)) {
             stop("The number of given intervals for the HRT object is incorrect!")
           }
         }
)

#-------------------------------------------------------------------------------
#' @param .Object The name of the class
#' @param couplRR Numeric, Coupling interval
#' @param compenRR Numeric, Compensatory interval
#' @param preRRs Numeric vector, Preceding 6 intervals
#' @param postRRs Numeric vector, Following 16 intervals
#' 
#' @rdname HRT
#' @export
setMethod("initialize", "HRT", 
          function(.Object, couplRR=NA_real_, compenRR=NA_real_,
                   preRRs=NA_real_, postRRs=NA_real_) {
            .Object@couplRR <- couplRR
            .Object@compenRR <- compenRR
            .Object@preRRs <- preRRs
            .Object@postRRs <- postRRs
            .Object@TO <- NA_real_
            .Object@TS <- NA_real_
            .Object@ablineCoefficients <- NA_real_
            
            #validObject(.Object)
            return(.Object)
            }
)

#-------------------------------------------------------------------------------
#' Calculate HRT parameters
#' 
#' Calculates the HRT parameters turbulence onset (TO) and turbulence slope (TS)
#' and the ab-line parameters of TS and saves them in the corresponding slots.
#' 
#' @param HRTObj The HRT object of which the parameters should be calculated
#' 
#' @rdname calcHRTParams
setGeneric("calcHRTParams", function(HRTObj) {
  standardGeneric("calcHRTParams")
})
#' @rdname calcHRTParams
setMethod("calcHRTParams", "HRT", function(HRTObj) {
  checkValidity(HRTObj, "intervals")
  
  preRRs <- HRTObj@preRRs
  postRRs <- HRTObj@postRRs
  
  # Calculate TO
  if(sum(preRRs) == 0) {
    warning("The sum of the intervals preceding the coupling interval is zero! Turbulence onset can't be calculated!")
    HRTObj@TO <- NA_real_
  } else {
    HRTObj@TO <- ( (sum(postRRs[1:2]) - sum(preRRs) ) / sum(preRRs) ) * 100
  }
  
  # Calculate TS
  slopes <- wapply(postRRs, 5, by = 1, FUN = function(y)
    return(lm(y ~ seq(1,5))$coefficients[2])
  )
  HRTObj@TS <- max(slopes, na.rm = TRUE)
  
  # Calculate coefficients for regression line in plot
  index <- which.max(slopes)
  model <- lm(postRRs[index:(index + 4)]~seq(1, 5))
  
  slope <- model$coefficients[2]
  intercept <- model$coefficients[1] - (slope * (3 + index))
  # 3 = #preRRs + #irregularRRs - 1
  
  HRTObj@ablineCoefficients <- c(intercept, slope)
  
  return(HRTObj)
})

#-------------------------------------------------------------------------------
#' Returns all intervals in right order
#' 
#' @param HRTObj HRT object
#' 
#' @rdname getRRs
setGeneric("getRRs", function(HRTObj) {
  standardGeneric("getRRs")
})
#' @rdname getRRs
setMethod("getRRs", "HRT", function(HRTObj) {
  return(c(HRTObj@preRRs,
           HRTObj@couplRR,
           HRTObj@compenRR,
           HRTObj@postRRs))
})

#-------------------------------------------------------------------------------
#' Plot an HRT object
#' 
#' Plots RR-intervals saved in the HRT object and marks
#' turbulence onset and turbulence slope.
#' 
#' @param x A single HRT object
#' @param cropped The detail of the plot. The default cuts off CPI
#'  and CMI and focuses on the HRT parameters. To show all points use FALSE.
#' @param type type of the plot, for other options see graphics::plot.xy
#' @param pch plotting character, for other options see graphics::var
#' @param xlab label for the x axis
#' @param ylab label for the x axis
#' @param ... Other arguments in tag = value form. See graphics::par for more information.
#' 
#' @note Please note that the argument xaxt and ylim can't be set, 
#'  since the axis as well as the ranges of the y axis are set by the function.
#' 
#' @export
setMethod("plot", "HRT", function(x, cropped = TRUE,
                                  type = "o",
                                  pch = 20,
                                  xlab = "# of RR interval",
                                  ylab = "length of RR interval (ms)",
                                  ...) {

  rrs <- getRRs(x)

  plot(seq(1:length(rrs)), rrs,
       xaxt = "n",
       ylim = if(cropped)
         c( min(c(x@preRRs, x@postRRs)),
            max(c(x@preRRs, x@postRRs))),
       type = type,
       pch = pch,
       xlab = xlab,
       ylab = ylab,
       ...
  )
  
  axis(1, at = seq(1:length(rrs)), labels = seq(-2, length(rrs) - 3, 1))
  legend("bottomright", c("Turbulence onset", "Turbulence slope"),
         lty = c(3), pch = c(19, NA), col = c("red", "blue"))
  
  # Turbulence onset
  points(c(1, 6), c(rrs[1], rrs[6]), col = "red", pch = 19)
  arrows(1, rrs[1], 6, rrs[1], lty = 3, col = "red", code = 0)
  arrows(6, rrs[1], 6, rrs[6], lty = 3, col = "red", code = 2)
  
  # Turbulence slope
  abline(coef = x@ablineCoefficients, lty = 3, col = "blue")
  
})

#-------------------------------------------------------------------------------
#' Checks whether slots are set
#' 
#' @param HRTObj HRT object
#' @param type Which slots should be checked? 
#'     (Only intervals or parameters also?)
#' 
#' @rdname checkValidity
setGeneric("checkValidity", function(HRTObj, type = "full") {
  standardGeneric("checkValidity")
})
#' @rdname checkValidity
setMethod("checkValidity", "HRT", function(HRTObj, type = "full") {
  if(anyNA(getRRs(HRTObj))) {
    stop("One or more interval is not set (NA)! Please make sure you have initialized the HRT object correctly!")
  }
  if(type == "full" && 
     anyNA(c(HRTObj@TO, HRTObj@TS, HRTObj@ablineCoefficients))) {
      stop("One or more HRT parameter of the given object is not set (NA)! Did you calculate the parameters? Use calcHRTParams first and try again!")
    }
})