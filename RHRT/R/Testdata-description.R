#' Intervals without noise
#'
#' Artificial dummy interval data: This dataset includes 5 identical VPCSs 
#' that fit the HRT filter rules and was created without noise.
#'
#' @format A numeric vector of length 1000.
"testdataRegular"

#' Intervals with noise
#'
#' Artificial dummy interval data: This dataset includes 5 different VPCSs 
#' that fit the HRT filter rules.
#'
#' @format A numeric vector of length 1000.
"testdataVariant"

#' Intervals with noise and without HRT
#'
#' Artificial dummy interval data: This dataset does not include any VPCSs.
#'
#' @format A numeric vector of length 1000.
"testdataVariantNoHRT"

#' Intervals without noise
#'
#' HRTList Object that was generated from testdataRegular and includes 5
#' identical HRTs.
#'
#' @format An HRTList object.
"testdataRegular_HRTObj"

#' Intervals with noise
#'
#' HRTList Object that was generated from testdataVariant and includes 5 HRTs.
#'
#' @format An HRTList object.
"testdataVariant_HRTObj"

#' Intervals with noise and without HRT
#'
#' HRTList Object that was generated from testdataVariantNoHRT. This HRTList
#' does not include any HRT.
#'
#' @format  An empty HRTList object.
"testdataVariantNoHRT_HRTObj"

#' Long term data
#'
#' Artificial dummy interval data: This dataset represents a long-term
#' measurement and includes 15 VPCSs that fit the HRT filter rules.
#'
#' @format A numeric vector.
"testdataLong"

#' Long term data annotations
#'
#' Artificial dummy interval data: This dataset contains the annotations 
#' matching testdataLong.
#'
#' @format A vector of characters.
"testdataLong_Ann"
