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

#' Intervals without noise
#'
#' HRTList Object that was generated from testdataRegular and includes 5
#' identical HRTs. For more information see the vignette.
#'
#' @format An HRTList object.
"testdataRegular_HRTObj"

#' Intervals with noise
#'
#' HRTList Object that was generated from testdataVariant and includes 3 HRTs
#' based on data from Physionet (NSRDB). For more information see the vignette.
#'
#' @format An HRTList object.
"testdataVariant_HRTObj"

#' Intervals with noise and without HRT
#'
#' HRTList Object that was generated from testdataVariantNoHRT. This HRTList
#' does not include any HRT. For more information see the vignette.
#'
#' @format  An empty HRTList object.
"testdataVariantNoHRT_HRTObj"

#' Long term data
#'
#' Interval data provided by Physionet (001 of NSR2DB). This dataset
#' contains a full long term measurement with several HRTs.
#'
#' @format A numeric vector.
"testdataLong"

#' Long term data annotations
#'
#' Interval data provided by Physionet (001 of NSR2DB). This dataset
#' contains the annotations matching testdataLong.
#'
#' @format A vector of characters.
"testdataLong_Ann"
