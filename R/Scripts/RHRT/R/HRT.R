#' S4 class to represent a HRT object
#'
#' This class specifies a object to save the lengths of intervals surrounding a
#' premature ventricular beat. It saves the HRT parameters turbulence onset
#' and turbulence slope after calculation as well as the coefficients of an
#' ab-line used for the plot.
#'
#' @slot couplRR Numeric, Coupling interval
#' @slot compRR Numeric, Compensatory interval
#' @slot preRRs Numeric vector, Preceding 6 intervals
#' @slot postRRs Numeric vector, Following 16 intervals
#' @slot TO Numeric, Turbulence onset
#' @slot TS Numeric, Turbulence slope
#' @slot TT Numeric, Turbulence timing
#' @slot intercept Numeric, Intercept of regression line of TS
#' @slot nTS Numeric, normalised Turbulence slope
#' @slot nintercept Numeric, Intercept of regression line of nTS

#' 
#' @name HRT
#' 
#' @importFrom methods setMethod 
#' @export
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
                  length(object@compRR) != 1,
                  length(object@preRRs) != numPreRRs,
                  length(object@postRRs) != numPostRRs)) {
             stop("The number of given intervals for the HRT object is incorrect!")
           }
         }
)

#-------------------------------------------------------------------------------
#' @param .Object The name of the class
#' @param couplRR Numeric, Coupling interval
#' @param compRR Numeric, Compensatory interval
#' @param preRRs Numeric vector, Preceding 6 intervals
#' @param postRRs Numeric vector, Following 16 intervals
#' 
#' @rdname HRT
#' @export
setMethod("initialize", "HRT",
          function(.Object, couplRR=NA_real_, compRR=NA_real_,
                   preRRs=NA_real_, postRRs=NA_real_) {
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
#' Calculates the HRT parameters turbulence onset (TO) and turbulence slope (TS)
#' and the ab-line parameters of TS and saves them in the corresponding slots.
#' 
#' @param HRTObj The HRT object of which the parameters should be calculated
#' @param IL The overall arithmetic mean interval of the interval length of the 
#' measurement to normalise TS
#' @param normIL The interval length to which TS should be normalised
#' 
#' @rdname calcHRTParams
setGeneric("calcHRTParams", function(HRTObj, IL = 800, normIL = 800) {
  standardGeneric("calcHRTParams")
})
#' @rdname calcHRTParams
setMethod("calcHRTParams", "HRT", function(HRTObj, IL = 800, normIL = 800) {
  checkValidity(HRTObj, "intervals")

  HRTObj <- calcTO(HRTObj)
  HRTObj <- calcTS(HRTObj)
  HRTObj <- calcTS(HRTObj, normalising = TRUE, IL, normIL)

  return(HRTObj)
})

#-------------------------------------------------------------------------------
#' Calculate TO parameters
#' 
#' Calculates the TO parameters and saves it in the corresponding slot
#' @param HRTObj The HRT object, for which TO should be calculated
#' 
#' @rdname calcTO
setGeneric("calcTO", function(HRTObj) {
  standardGeneric("calcTO")
})
#' @rdname calcTO
#' @export
setMethod("calcTO", "HRT", function(HRTObj) {
  checkValidity(HRTObj, "intervals")
  
  preRRs <- HRTObj@preRRs
  postRRs <- HRTObj@postRRs
  
  if(sum(preRRs) == 0) {
    warning("The sum of the intervals preceding the coupling interval is zero! Turbulence onset can't be calculated!")
    HRTObj@TO <- NA_real_
  } else {
    HRTObj@TO <- ( (sum(postRRs[1:2]) - sum(preRRs) ) / sum(preRRs) ) * 100
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
#' @param HRTObj The HRT object, for which TS should be calculated
#' @param normalising text
#' @param IL test
#' @param normIL text
#' 
#' @rdname calcTS
setGeneric("calcTS", function(HRTObj, normalising = FALSE, IL = 800, normIL = 800) {
  standardGeneric("calcTS")
})
#' @rdname calcTS
#' @export
setMethod("calcTS", "HRT", function(HRTObj, normalising = FALSE, IL = 800, normIL = 800) {
  checkValidity(HRTObj, "intervals")
  
  postRRs <- HRTObj@postRRs
  if (normalising) postRRs <- postRRs*normIL/IL
  
  # Calculate TS
  ## Formula for the slope: (n * Σxy - (Σx)(Σy)) / (n x Σx^2 - (Σx)^2)
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
           HRTObj@compRR,
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
#' @param add Should the given HRT be added to a plot?
#' @param showTT Should Turbulence timing be shown?
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