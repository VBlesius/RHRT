#' @title Finds PVCs
#' @description
#' Scans for PVCs in a vector of RR-intervals and returns a list of PVC-Objects.
#' The PVC-criteria used were published by Schmidt et. al
#' (see \code{\link{http://www.h-r-t.com/hrt/en/calc.html}})
#' @param vector
#' @return List of pvc-objects
get_pvcs <- function(intervals) {
  num_pre_rrs <- 6 # number of regular RR-intervals before the coupling interval
  num_post_rrs <- 16 # number of regular RR-intervals after the coupling interval
  windowsize <- num_pre_rrs + num_post_rrs + 2 # sums up coupling and compensatory interval

  pvcs <- wapply(intervals, windowsize, by = 1, FUN = check_for_pvc)
  pvcs <- pvcs[!sapply(pvcs, is.null)] # removes NULL entries
  return(pvcs)
}

#––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––
# Checks specified number of RR-intervals for PVC-criteria
# and returns a pvc-object
check_for_pvc <- function(x) {
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
  # checks for PVC
  is_coupl_interv <- coupl_rr <= ref * 0.8
  is_compen_interv <- compen_rr >= ref * 1.2
  # checks for arrhythmias and artefacts
  is_in_range <- all(reg_rr > 300 && reg_rr < 2000)
  is_not_deviating <- all(
    reg_rr >= ref * 0.8, reg_rr <= ref * 1.2,
    wapply(pre_rrs, 2, by = 1, FUN = function(x) diff(x) <= 200),
    wapply(post_rrs, 2, by = 1, FUN = function(x) diff(x) <= 200)
  )

  # Checks for criteria and saves PVC as object
  if (is_coupl_interv & is_compen_interv & is_in_range & is_not_deviating) {
    temp_pvc <- pvc(coupl_rr = coupl_rr,
                    compen_rr = compen_rr,
                    pre_rrs = pre_rrs[- (num_pre_rrs - 2):0],
                    post_rrs = post_rrs[1:15])
    return(temp_pvc)
  }
}

#' @title Creates an averaged PVC
#' @description For each index the mean of the intervals across all PVCs in a
#' list is calculated and the averaged PVC returned.
#' @param List of pvc-objects
#' @return The averaged pvc
calc_averaged_pvc <- function(pvcs) {
  coupl_rr <- mean(sapply(pvcs, slot, "coupl_rr"))
  compen_rr <- mean(sapply(pvcs, slot, "compen_rr"))
  pre_rrs <- rowMeans(sapply(pvcs, slot, "pre_rrs"))
  post_rrs <- rowMeans(sapply(pvcs, slot, "post_rrs"))

  temp_pvc <- pvc(coupl_rr = coupl_rr, compen_rr = compen_rr,
                  pre_rrs = pre_rrs, post_rrs = post_rrs)
  return(temp_pvc)
}
