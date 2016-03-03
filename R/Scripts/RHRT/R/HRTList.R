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
#' @rdname HRTList
#' 
#' @export
setClass("HRTList",
         slots = list(
           pos = "vector",
           HRTs = "list",
           avHRT = "HRT"),
)

setGeneric("getPositions", function(HRTListObj) {
  standardGeneric("getPositions")
})
setMethod("getPositions", "HRTList", function(HRTListObj) {
 
})

setGeneric("getParamsMean", function(HRTListObj) {
  standardGeneric("getParamsMean")
})
setMethod("getParamsMean", "HRTList", function(HRTListObj) {
  
})

setGeneric("getParamsAll", function(HRTListObj) {
  standardGeneric("getParamsAll")
})
setMethod("getParamsAll", "HRTList", function(HRTListObj) {
  
})

setGeneric("calcAvHRT", function(HRTListObj) {
  standardGeneric("calcAvHRT")
})
setMethod("calcAvHRT", "HRTList", function(HRTListObj) {
  
})

#' Plot a HRT object
#' 
#' Plots RR-intervals saved in the HRT object and marks
#' turbulence onset and turbulence slope.
#' 
#' @param x A HRTList
#' @param size The detail of the plot. The default is "cropped" which cuts off CPI
#'  and CMI and focuses on the HRT parameters. Else the plot shows all intervals.
#' 
#' @note Make sure you have calculated HRT parameters first!
#' 
#' @export
setMethod("plot", "HRTlist", function(x, size = "cropped") {
  plot(getHRTParams(x@avHRT), size=size)

  lapply(x@HRTs, function(y) {
    rrs <- getRRs(y)
    lines(seq(1:length(rrs)), rrs, col="grey")
  })
  
})