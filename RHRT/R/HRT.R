#' S4 class to represent an HRT object
#'
#' This class specifies an object to save the lengths of intervals surrounding a
#' premature ventricular beat. It saves the HRT parameters turbulence onset (TO),
#' slope (TS) and timing (TT) after calculation as well as the coefficients of an
#' ab-line used for the plot.
#' TS is saved after common calculation and after normalising.
#'
#' @slot couplRR (Numeric) Coupling interval
#' @slot compRR (Numeric) Compensatory interval
#' @slot preRRs (Numeric vector) Preceding intervals
#' @slot postRRs (Numeric vector) Following intervals
#' @slot TO (Numeric) Turbulence onset
#' @slot TS (Numeric) Turbulence slope
#' @slot TT (Numeric) Turbulence timing
#' @slot intercept (Numeric) Intercept of regression line of TS
#' @slot nTS (Numeric) Normalised Turbulence slope
#' @slot nintercept (Numeric) Intercept of regression line of nTS
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
    nintercept = "numeric"
  ),
  validity = function(object) {
    if (any(
      length(object@couplRR) != 1,
      length(object@compRR) != 1
    )) {
      stop("The number of given intervals for the HRT object is incorrect!")
    }
  }
)

#-------------------------------------------------------------------------------
#' @param .Object (Character) The name of the class
#' @param couplRR (Numeric) Coupling interval
#' @param compRR (Numeric) Compensatory interval
#' @param preRRs (Numeric vector) Preceding intervals
#' @param postRRs (Numeric vector) Following intervals
#'
#' @rdname HRT
#' @importFrom methods initialize
#' @return (HRT) A new HRT object
#' 
#' @export
setMethod(
  "initialize", "HRT",
  function(.Object, couplRR = NA_real_, compRR = NA_real_,
           preRRs = NA_real_, postRRs = NA_real_) {
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
#' @param HRTObj (HRT) The HRT object of which the parameters should be calculated
#' @param IL (Numeric) The overall arithmetic mean of the interval length of the
#' measurement to normalise TS
#' @param normIL (Numeric) The interval length to which TS should be normalised
#' 
#' @return (HRT) An HRT object with (re)calculated HRT parameters
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
#' @param HRTObj (HRT) The HRT object, for which TO should be calculated
#' 
#' @return (HRT) An HRT object with (re)calculated TO
#' 
#' @rdname calcTO
setGeneric("calcTO", function(HRTObj) {
  standardGeneric("calcTO")
})
#' @rdname calcTO
setMethod("calcTO", "HRT", function(HRTObj) {
  checkValidity(HRTObj)

  preRRs <- utils::tail(HRTObj@preRRs, 2)
  postRRs <- HRTObj@postRRs[1:2]

  if (sum(preRRs) == 0) {
    warning("The sum of the two intervals preceding the coupling interval is zero! Turbulence onset can't be calculated!")
    HRTObj@TO <- NA_real_
  } else {
    HRTObj@TO <- ((sum(postRRs) - sum(preRRs)) / sum(preRRs)) * 100
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
#' @param HRTObj (HRT) The HRT object, for which TS should be calculated
#' @param normalising (Boolean) Should the normalised TS be calculated?
#' @inheritParams calcHRTParams
#' 
#' @return (HRT) An HRT object with (re)calculated TS+intercept or nTS+nintercept
#'
#' @rdname calcTS
setGeneric("calcTS", function(HRTObj, normalising = FALSE, IL = c_normIL, normIL = c_normIL) {
  standardGeneric("calcTS")
})
#' @rdname calcTS
setMethod("calcTS", "HRT", function(HRTObj, normalising = FALSE, IL = c_normIL, normIL = c_normIL) {
  checkValidity(HRTObj)

  postRRs <- HRTObj@postRRs
  if (normalising) postRRs <- postRRs * normIL / IL

  # Calculate TS
  ## Formula for the slope: (n * sum(xy) - (sum(x))(sum(y))) / (n x sum(x)^2 - (sum(x))^2)
  x <- seq(1, 5)
  n <- 5
  slopes <- roll(postRRs, 5, function(y) {
    return((n * sum(x * y) - sum(x) * sum(y)) / (n * sum(x^2) - sum(x)^2))
  })
  TS_temp <- max(unlist(slopes), na.rm = TRUE)

  # Calculate intercept for regression line in plot
  ## Formula for the intercept: mean(y) - slope*mean(x)
  n_preRRs <- length(HRTObj@preRRs)
  slope <- TS_temp
  index <- which.max(slopes)
  TS_intervals <- postRRs[index:(index + n_preRRs - 1)]
  intercept <- mean(c(min(TS_intervals), max(TS_intervals))) - slope * (mean(x) + n_preRRs + 1 + index)
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
#' @param HRTObj HRT
#' 
#' @return (numeric vector) All VPCS intervals
#'
#' @rdname getRRs
setGeneric("getRRs", function(HRTObj) {
  standardGeneric("getRRs")
})
#' @rdname getRRs
#' @export
setMethod("getRRs", "HRT", function(HRTObj) {
  return(c(
    HRTObj@preRRs,
    HRTObj@couplRR,
    HRTObj@compRR,
    HRTObj@postRRs
  ))
})

#-------------------------------------------------------------------------------
#' Plot an HRT object
#'
#' Plots RR-intervals saved in the HRT object and marks
#' turbulence onset and turbulence slope.
#'
#' @param x (HRT) A HRT object
#' @param cropped (Boolean) Should the plot be cut to focus on the HRT parameters?
#' To show all points use FALSE.
#' @param TT (Boolean) Should Turbulence timing be marked?
#' @param pch (Numeric) Plotting character, for other options see graphics::var
#' @param xlab (Character) Label for the x axis
#' @param ylab (Character) Label for the y axis
#' @param paramsLegend (Boolean) Should the parameter values of the HRT be plotted?
#' @param colTO (Character) Colour used to highlight TO
#' @param colTS (Character) Colour used to highlight TS
#' @param colTT (Character) Colour used to highlight TT
#' @param add (Boolean) Should the given HRT be added to a plot?
#' @param ... Other arguments in tag = value form. See graphics::par for more information.
#'
#' @return No return value
#' 
#' @examples
#' # You need an HRT object
#' hrt <- vectorToHRT(testdataLong, testdataLong_Ann)@HRTs[[1]]
#' 
#' # Plot your HRT and zoom out
#' plot(hrt, cropped = FALSE)
#' 
#' # Include TT and customise it
#' plot(hrt, TT = TRUE, colTT = "green", pch = 7)
#' 
#' # Use standard graphics parameters
#' ## Note: Some parameters are used inside the function and cannot be set
#' plot(hrt, TT = TRUE, main = "Example plot", bty = "n", cex.lab = 1.2)
#' 
#' @note Please note that some graphics parameters (par) cannot be modified,
#'  since they are needed to be set inside the function.
#'
#' @import graphics
#' @export
setMethod("plot", "HRT", function(x,
                                  cropped = TRUE,
                                  TT = FALSE,
                                  pch = 20,
                                  xlab = "# of RR interval",
                                  ylab = "length of RR interval (ms)",
                                  paramsLegend = TRUE,
                                  colTO = "#ec2023",
                                  colTS = "#006AFF",
                                  colTT = "#6800DE",
                                  add = FALSE,
                                  ...) {
  rrs <- getRRs(x)
  n_preRRs <- length(x@preRRs)
  pchParams <- if (pch == 20) 21 else pch

  if (!add) {
    ymin <- min(c(x@preRRs, x@postRRs))
    ymax <- max(c(x@preRRs, x@postRRs))
    ydiff <- ymax - ymin

    plot(seq(1:length(rrs)), rrs,
      xaxt = "n",
      ylim = if (cropped) {
        c(
          ymin - ydiff * 0.3,
          ymax + ydiff * 0.3
        )
      },
      type = "o",
      pch = pch,
      xlab = xlab,
      ylab = ylab,
      ...
    )
  } else {
    lines(seq(1:length(rrs)), rrs)
  }

  axis(1,
    at = seq(1:length(rrs)), las = 2,
    labels = c(seq(-n_preRRs, -1), "couplRR", "compRR", seq(1:(length(rrs) - n_preRRs - 2)))
  )

  legend("bottomright",
    c(paste("TO", if(paramsLegend) {round(x@TO, 2)}),
      paste("TS", if(paramsLegend) {round(x@TS, 2)}),
      if (TT) {paste("TT", if(paramsLegend) {round(x@TT, 2)})}),
    lty = c(0, 3, 0),
    pch = c(pchParams),
    col = c(colTO, colTS, colTT),
    pt.bg = c(colTO, 0, 0),
    pt.cex = c(1, 1, 2)
  )

  # Turbulence onset
  to_indices <- c(n_preRRs - 1, n_preRRs, n_preRRs + 3, n_preRRs + 4)
  points(c(to_indices), c(rrs[to_indices]), bg = colTO, col = colTO, pch = pchParams)

  # Turbulence slope
  TTcorr <- x@TT + n_preRRs + 2
  points(seq(TTcorr, TTcorr + 4), c(rrs[TTcorr:(TTcorr + 4)]), col = colTS, pch = pchParams)
  abline(coef = c(x@intercept, x@TS), lty = 3, col = colTS)

  # Turbulence timing
  if (TT) {
    points(TTcorr, rrs[TTcorr], col = colTT, cex = 2, pch = pchParams)
  }
})

#-------------------------------------------------------------------------------
#' Checks whether slots are set
#'
#' @param x (HRT) Object to be checked
#' @param ... Other parameters
#' 
#' @return No return value, possibly throws errors/warnings
#'
#' @rdname checkValidity
setGeneric("checkValidity", function(x, ...) {
  standardGeneric("checkValidity")
})
#' @rdname checkValidity
setMethod("checkValidity", "HRT", function(x) {
  if (anyNA(getRRs(x))) {
    stop("One or more interval is not set (NA)! Please make sure you have initialized the HRT object correctly!")
  }
})
