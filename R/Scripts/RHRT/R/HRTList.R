#' S4 class to represent a list of HRT objects
#'
#' This class specifies an object to save all HRT objects of a given vector. It 
#' also saves an averaged HRT for calculation of the averaged HRT parameters and
#' plotting of all HRTs in a single plot.
#'
#' @slot HRTs list, all HRT objects
#' @slot avHRT HRT, averaged HRT
#' 
#' @rdname HRTList
#' 
#' @export
setClass("HRTList",
         slots = list(
           HRTs = "list",
           avHRT = "HRT"),
)