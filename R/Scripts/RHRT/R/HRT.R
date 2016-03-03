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

#' Calculate HRT parameters
#' 
#' Calculates the HRT parameters turbulence onset (TO) and turbulence slope (TS)
#' and the ab-line parameters of TS and saves them in the corresponding slots.
#' 
#' @param HRTObj The HRT object of which the parameters should be calculated
#' 
#' @rdname getHRTParams
#' @export
setGeneric("getHRTParams", def = function(HRTObj) {
standardGeneric("getHRTParams")
})

#' @rdname getHRTParams
setMethod("getHRTParams", "HRT", function(HRTObj) {
  checkValidity(HRTObj, "intervals")
  
  preRRs <- HRTObj@preRRs
  postRRs <- HRTObj@postRRs
  
  # Calculate TO
  HRTObj@TO <- ( (sum(postRRs[1:2]) - sum(preRRs) ) / sum(preRRs) ) * 100
  
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

setGeneric("getRRs", function(HRTObj) {
  standardGeneric("getRRs")
})
setMethod("getRRs", "HRT", function(HRTObj) {
  return(c(HRTObj@preRRs,
           HRTObj@couplRR,
           HRTObj@compenRR,
           HRTObj@postRRs))
})

#' Plot a HRT object
#' 
#' Plots RR-intervals saved in the HRT object and marks
#' turbulence onset and turbulence slope.
#' 
#' @param x Either a single HRT object or a list of HRT objects
#' @param type The type of the plot. The default is "cropped" which cuts off CPI
#'  and CMI and focuses on the HRT parameters. Else the plot shows all intervals.
#' 
#' @export
setMethod("plot", "HRT", function(x, size = "cropped") {
  checkValidity(x)
  
  rrs <- getRRs(x)
  
  plot(seq(1:length(rrs)), rrs,
       "o", pch = 20,
       xlab = "# of RR interval",
       ylab = "length of RR interval (ms)",
       xaxt = "n",
       ylim = if(size != "full")
         c(mean(rrs) - sd(rrs) / 2, mean(rrs) + sd(rrs) / 2)
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

setGeneric("checkValidity", function(HRTObj, type = "full") {
  standardGeneric("checkValidity")
})
setMethod("checkValidity", "HRT", function(HRTObj, type = "full") {
  if(anyNA(getRRs(HRTObj))) {
    stop("One or more interval is not set (NA)! Please make sure you have initialized the HRT object correctly!")
  }
  if(type == "full" && 
     anyNA(c(HRTObj@TO, HRTObj@TS, HRTObj@ablineCoefficients))) {
      stop("One or more HRT parameter of the given object is not set (NA)! Did you calculate the parameters? Use getHRTParams first and try again!")
    }
})
