#' Intervals without noise
#'
#' Interval data based on a dataset provided by Physionet (NSRDB). This dataset
#' includes 5 HRTs that fit the HRT filter rules. For more information see the
#' vignette.
#'
#' @format A numeric vector of length 1000.
"testdataRegular"

#' Intervals with noise
#'
#' Interval data based on a dataset provided by Physionet (NSRDB). This dataset
#' includes 3 HRTs that fit the HRT filter rules. For more information see the
#' vignette
#'
#' @format A numeric vector of length 1000.
"testdataVariant"

#' Intervals with noise and without HRT
#'
#' Interval data based on a dataset provided by Physionet (NSRDB). This dataset
#' does not include any HRTs. For more information see the vignette.
#'
#' @format A numeric vector of length 1000.
"testdataVariantNoHRT" 

#' Valid HRTList object
#'
#' A HRTList object based on a dataset provided by Physionet (NSRDB). This object
#' includes 3 HRTs that result in a significant HRTA class.
#'
#' @format A HRTList with 3 HRTs
"HRTs"