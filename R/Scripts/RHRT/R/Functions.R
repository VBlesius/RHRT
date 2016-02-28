#' Finds HRTs
#'
#' Scans for HRTs in a vector of RR-intervals and returns a list of HRT-Objects.
#' The HRT-criteria used were published by Schmidt et al.
#' (see \code{\url{http://www.h-r-t.com/hrt/en/calc.html}})
#'
#' @param Numeric vector
#' @return List of hrt-objects
#' @export
get_hrts <- function(intervals) {
  num_pre_rrs <- 6 # number of regular RR-intervals before the coupling interval
  num_post_rrs <- 16 # number of regular RR-intervals after the coupling interval
  windowsize <- num_pre_rrs + num_post_rrs + 2 # sums up coupling and compensatory interval

  hrts <- wapply(intervals, windowsize, by = 1, FUN = check_for_hrt)
  hrts <- hrts[!sapply(hrts, is.null)] # removes NULL entries
  return(hrts)
}

#–––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––
# Checks specified number of RR-intervals for HRT-criteria
# and returns a hrt-object
check_for_hrt <- function(x) {
  num_pre_rrs <- 6 # number of regular RR-intervals before the coupling interval # TODO: remove redundancy!!!
  # Defines coupling, compensatory, preceding and following intervals
  # and sums up regular intervals
  coupl_rr <- x[num_pre_rrs + 1]
  compen_rr <- x[num_pre_rrs + 2]
  pre_rrs <- x[1:num_pre_rrs]
  post_rrs <- x[(num_pre_rrs + 3):length(x)]
  reg_rr <- c(pre_rrs, post_rrs)

  # Reference interval
  ref <- mean(pre_rrs)

  ## Filtering methods
  # checks for HRT
  is_coupl_interv <- coupl_rr <= ref * 0.8
  is_compen_interv <- compen_rr >= ref * 1.2
  # checks for arrhythmias and artefacts
  is_in_range <- all(reg_rr > 300 && reg_rr < 2000)
  is_not_deviating <- all(
    reg_rr >= ref * 0.8, reg_rr <= ref * 1.2,
    wapply(pre_rrs, 2, by = 1, FUN = function(x) diff(x) <= 200),
    wapply(post_rrs, 2, by = 1, FUN = function(x) diff(x) <= 200)
  )

  # Checks for criteria and saves HRT as object
  if (is_coupl_interv & is_compen_interv & is_in_range & is_not_deviating) {
    temp_hrt <- hrt(coupl_rr = coupl_rr,
                    compen_rr = compen_rr,
                    pre_rrs = pre_rrs[- (num_pre_rrs - 2):0],
                    post_rrs = post_rrs[1:15])
    return(temp_hrt)
  }
}

#–––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––
#' Creates an averaged HRT
#'
#' For each index the mean of the intervals across all HRTs in a list
#' is calculated and the averaged HRT returned.
#'
#' @details
#' Use the averaged HRT for calculating TS since averaging eliminates other
#' RR variability. TO is commonly first calculated of the single HRTs and then
#' averaged. (See "Heart Rate Turbulence: Standards of Measurement,
#' Physiological Interpretation, and Clinical Use, Axel Bauer et al.,
#' Journal of the American College of Cardiology, Volume 52, Issue 17,
#' Pages 1353-1365")
#'
#' @param List of hrt-objects
#' @return The averaged hrt
#' @export
calc_averaged_hrt <- function(hrts) {
  coupl_rr <- mean(sapply(hrts, slot, "coupl_rr"))
  compen_rr <- mean(sapply(hrts, slot, "compen_rr"))
  pre_rrs <- rowMeans(sapply(hrts, slot, "pre_rrs"))
  post_rrs <- rowMeans(sapply(hrts, slot, "post_rrs"))

  temp_hrt <- hrt(coupl_rr = coupl_rr, compen_rr = compen_rr,
                  pre_rrs = pre_rrs, post_rrs = post_rrs)
  return(temp_hrt)
}
