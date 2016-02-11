## Checks number of RR-intervals for PVC-criteria published by Schmidt et. al (see http://www.h-r-t.com/hrt/en/calc.html)

check_for_pvc <- function(x) {

  # Defines coupling, compensatory, preceding and following intervals
  # and sums up regular intervals
  coupl_rr <- x[numpre_rrs+1]
  compen_rr <- x[numpre_rrs+2]
  pre_rrs <- x[1:numpre_rrs]
  post_rrs <- x[(numpre_rrs+3):length(x)]
  reg_rr <- c(pre_rrs, post_rrs)

  # Reference interval
  ref <- mean(pre_rrs)

  ## Filtering methods
  # checks for PVC
  is_coupl_interv <- coupl_rr <= ref*0.8
  is_compen_interv <- compen_rr >= ref*1.2
  # checks for arrhythmias and artefacts
  is_in_range <- all(reg_rr > 300 && reg_rr < 2000)
  is_not_deviating <- all(
    reg_rr >= ref*0.8, reg_rr <= ref*1.2,
    wapply(pre_rrs, 2, by=1, FUN=function(x) diff(x)<=200), wapply(post_rrs, 2, by=1, FUN=function(x) diff(x)<=200)
  )

  # Checks for criteria and saves PVC as object
  if (is_coupl_interv & is_compen_interv & is_in_range & is_not_deviating) {
    temp_pvc <- pvc(coupl_rr=coupl_rr, compen_rr=compen_rr, pre_rrs=pre_rrs[-(numpre_rrs-2):0], post_rrs=post_rrs[1:15])
    return(temp_pvc)
  }
}

##
calc_averaged_pvc <- function() {
  coupl_rr <- mean(sapply(PVCs, slot, "coupl_rr"))
  compen_rr <- mean(sapply(PVCs, slot, "compen_rr"))
  pre_rrs <- rowMeans(sapply(PVCs, slot, "pre_rrs"))
  post_rrs <- rowMeans(sapply(PVCs, slot, "post_rrs"))

  temp_pvc <- PVC(coupl_rr=coupl_rr, compen_rr=compen_rr, pre_rrs=pre_rrs, post_rrs=post_rrs)
  return(temp_pvc)
}
