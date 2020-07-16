#' S4 class to represent an HRT object
#'
#' This class specifies an object to save the lengths of intervals surrounding a
#' premature ventricular beat. It saves the HRT parameters turbulence onset (TO),
#' slope (TS) and timing (TT) after calculation as well as the coefficients of an
#' ab-line used for the plot.
#' TS is saved after common calculation and after normalising.
#'
#' @slot couplRR Numeric, Coupling interval
#' @slot compRR Numeric, Compensatory interval
#' @slot preRRs Numeric vector, Preceding 6 intervals
#' @slot postRRs Numeric vector, Following 16 intervals
#' @slot TO Numeric, Turbulence onset
#' @slot TS Numeric, Turbulence slope
#' @slot TT Numeric, Turbulence timing
#' @slot intercept Numeric, Intercept of regression line of TS
#' @slot nTS Numeric, Normalised Turbulence slope
#' @slot nintercept Numeric, Intercept of regression line of nTS

#' 
#' @name HRT
#' 
#' @importFrom methods setMethod 
setClass("HRT",
         slots = list(
           couplRR = "numeric",
           compRR = "numeric",
           preRRs = "vector",
           postRRs = "vector",
           TO = "numeric",
           TS = "numeric",
           TT = "numeric",
           intercept = "numeric",
           nTS = "numeric",
           nintercept = "numeric"),

         validity = function(object) {
           if(any(length(object@couplRR) != 1,
                  length(object@compRR) != 1)) {
             stop("The number of given intervals for the HRT object is incorrect!")
           }
         }
)

#-------------------------------------------------------------------------------
#' @param .Object The name of the class
#' @param couplRR Numeric, Coupling interval
#' @param compRR Numeric, Compensatory interval
#' @param preRRs Numeric vector, Preceding intervals
#' @param postRRs Numeric vector, Following intervals
#' @param ... Other values to be saved in the slots of the HRT object 
#' given in tag = value form
#' 
#' @rdname HRT
#' @export
setMethod("initialize", "HRT",
          function(.Object, couplRR=NA_real_, compRR=NA_real_,
                   preRRs=NA_real_, postRRs=NA_real_, ...) {
            .Object@couplRR <- couplRR
            .Object@compRR <- compRR
            .Object@preRRs <- preRRs
            .Object@postRRs <- postRRs
            .Object@TO <- NA_real_
            .Object@TS <- NA_real_
            .Object@TT <- NA_real_
            .Object@intercept <- NA_real_
            .Object@nTS <- NA_real_
            .Object@nintercept <- NA_real_

            return(.Object)
            }
)

#-------------------------------------------------------------------------------
#' Calculate HRT parameters
#' 
#' Calculates all HRT parameters needed for an HRT object 
#' and saves them in the corresponding slots.
#' 
#' This method is a wrapper for the methods calcTO and calcTS.
#' 
#' @param HRTObj HRT, The HRT object of which the parameters should be calculated
#' @param IL Numeric, The overall arithmetic mean of the interval length of the 
#' measurement to normalise TS
#' @param normIL Numeric, The interval length to which TS should be normalised
#' 
#' @rdname calcHRTParams
setGeneric("calcHRTParams", function(HRTObj, IL = c_normIL, normIL = c_normIL) {
  standardGeneric("calcHRTParams")
})
#' @rdname calcHRTParams
setMethod("calcHRTParams", "HRT", function(HRTObj, IL = c_normIL, normIL = c_normIL) {
  checkValidity(HRTObj)

  HRTObj <- calcTO(HRTObj)
  HRTObj <- calcTS(HRTObj)
  HRTObj <- calcTS(HRTObj, normalising = TRUE, IL, normIL)

  return(HRTObj)
})

#-------------------------------------------------------------------------------
#' Calculate TO parameters
#' 
#' Calculates the TO parameters and saves it in the corresponding slot
#' @param HRTObj HRT, The HRT object, for which TO should be calculated
#' 
#' @rdname calcTO
setGeneric("calcTO", function(HRTObj) {
  standardGeneric("calcTO")
})
#' @rdname calcTO
#' @export
setMethod("calcTO", "HRT", function(HRTObj) {
  checkValidity(HRTObj)
  
  preRRs <- HRTObj@preRRs
  postRRs <- HRTObj@postRRs
  
  if(sum(preRRs) == 0) {
    warning("The sum of the intervals preceding the coupling interval is zero! Turbulence onset can't be calculated!")
    HRTObj@TO <- NA_real_
  } else {
    HRTObj@TO <- ( (sum(postRRs[1:2]) - sum(tail(preRRs, 2)) ) / sum(tail(preRRs,2)) ) * 100
  }
  
  return(HRTObj)
})

#-------------------------------------------------------------------------------
#' Calculate TS parameters
#' 
#' Calculates all TS parameters (TS itself, its index TT (turbulence timing)
#' and the intercept for the plot) and saves them in the corresponding slots.
#' Can also calculate normalised TS and intercept.
#' 
#' @param HRTObj HRT, The HRT object, for which TS should be calculated
#' @param normalising Boolean, Should the normalised TS be calculated?
#' @inheritParams calcHRTParams
#' 
#' @rdname calcTS
setGeneric("calcTS", function(HRTObj, normalising = FALSE, IL = c_normIL, normIL = c_normIL) {
  standardGeneric("calcTS")
})
#' @rdname calcTS
#' @export
setMethod("calcTS", "HRT", function(HRTObj, normalising = FALSE, IL = c_normIL, normIL = c_normIL) {
  checkValidity(HRTObj)
  
  postRRs <- HRTObj@postRRs
  if (normalising) postRRs <- postRRs*normIL/IL
  
  # Calculate TS
  ## Formula for the slope: (n * sum(xy) - (sum(x))(sum(y))) / (n x sum(x)^2 - (sum(x))^2)
  x <- seq(1,5)
  n <- 5
  slopes <- wapply(postRRs, 5, by = 1, FUN = function(y) {
    return((n*sum(x*y)-sum(x)*sum(y)) / (n*sum(x^2)-sum(x)^2))
  })
  TS_temp <- max(slopes, na.rm = TRUE)
  
  # Calculate intercept for regression line in plot
  ## Formula for the intercept: mean(y) - slope*mean(x)
  slope <- TS_temp
  index <- which.max(slopes)
  TS_intervals <- postRRs[index:(index + 4)]
  intercept <- mean(c(min(TS_intervals), max(TS_intervals))) - slope*(mean(x)+3+index)
  # The intercept has to be adapted for the plot, which also shows preRRS, coupling interval and compensatory interval, so it has to be "moved" by 4 "steps"
  
  if (normalising) {
    HRTObj@nTS <- slope
    HRTObj@nintercept <- intercept
  } else {
    HRTObj@TS <- slope
    HRTObj@TT <- index
    HRTObj@intercept <- intercept
  }

  return(HRTObj)
})

#-------------------------------------------------------------------------------
#' Returns the VPCS intervals in right order
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
           HRTObj@compRR,
           HRTObj@postRRs))
})

#-------------------------------------------------------------------------------
#' Plot an HRT object
#' 
#' Plots RR-intervals saved in the HRT object and marks
#' turbulence onset and turbulence slope.
#' 
#' @param x HRT, A HRT object
#' @param cropped Boolean, Should the plot be cut to focuse on the HRT parameters?
#' To show all points use FALSE.
#' @param add Boolean, Should the given HRT be added to a plot?
#' @param showTT Boolean, Should Turbulence timing be marked?
#' @param type Character, Type of the plot, for other options see graphics::plot.xy
#' @param pch Numeric, Plotting character, for other options see graphics::var
#' @param xlab Character, Label for the x axis
#' @param ylab Character Label for the x axis
#' @param ... Other arguments in tag = value form. See graphics::par for more information.
#' 
#' @note Please note that the argument xaxt and ylim can't be set, 
#'  since the axis as well as the ranges of the y axis are set by the function.
#' 
#' @export
setMethod("plot", "HRT", function(x, cropped = TRUE, add = FALSE, showTT = FALSE,
                                  type = "o",
                                  pch = 20,
                                  xlab = "# of RR interval",
                                  ylab = "length of RR interval (ms)",
                                  ...) {
  rrs <- getRRs(x)

  if(!add) {
    ymin <- min(c(x@preRRs, x@postRRs))
    ymax <- max(c(x@preRRs, x@postRRs))
    ydiff <- ymax - ymin
    
    plot(seq(1:length(rrs)), rrs,
         xaxt = "n",
         ylim = if(cropped)
           c(ymin-ydiff*0.3,
             ymax+ydiff*0.3),
         type = type,
         pch = pch,
         xlab = xlab,
         ylab = ylab,
         ...)
  } else {
    lines(seq(1:length(rrs)), rrs)
  }

  axis(1, at = seq(1:length(rrs)), labels = c(-2, -1, "couplRR", "compRR", seq(1:(length(rrs)-4))), las=2)
  if (showTT) {
    legend("bottomright", c("Turbulence onset", "Turbulence slope", "Turbulence Timing"),
           lty = c(3), pch = c(19, NA), col = c("red", "blue", "chartreuse3")) 
   } else {
     legend("bottomright", c("Turbulence onset", "Turbulence slope"),
            lty = c(3), pch = c(19, NA), col = c("red", "blue"))
   }

  # Turbulence onset
  points(c(1,2,5,6), c(rrs[1:2], rrs[5:6]), bg= "red", pch = 21)
  #arrows(1, rrs[1], 6, rrs[1], lty = 3, col = "red", code = 0)
  #arrows(6, rrs[1], 6, rrs[6], lty = 3, col = "red", code = 2)

  # Turbulence slope
  TTcorr <- x@TT+4
  points(seq(TTcorr,TTcorr+4), c(rrs[TTcorr:(TTcorr+4)]), col = "blue", pch = 21)
  abline(coef = c(x@intercept, x@TS), lty = 3, col = "blue")
  
  # Turbulence timing
  if (showTT) {
    points(TTcorr, rrs[TTcorr], col = "chartreuse3", cex = 3, pch = 8)
  }

})

#-------------------------------------------------------------------------------
#' Checks whether slots are set
#' 
#' @param x Object to be checked
#' @param ... Other parameters
#' 
#' @rdname checkValidity
setGeneric("checkValidity", function(x,...) {
  standardGeneric("checkValidity")
})
#' @rdname checkValidity
setMethod("checkValidity", "HRT", function(x) {
  if(anyNA(getRRs(x))) {
    stop("One or more interval is not set (NA)! Please make sure you have initialized the HRT object correctly!")
  }
})