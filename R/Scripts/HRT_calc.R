path = "/home/etz/Documents/Promotion/Data/CHF2DB/"

chf201 = read.table(file.path(path , "chf201.ecg.txt"))
chf201 = unlist(chf201*1000)

require(zoo)
rollapply(chf201, 24, checkForPVC) # 6 RR needed before PVC, 1 compensatory interval, 16 after PVC = 26, see http://www.h-r-t.com/hrt/en/calc.html
# Stattdessen
## wapply? http://www.r-bloggers.com/wapply-a-faster-but-less-functional-rollapply-for-vector-setups/ 
## RcppRoll package?

PVCs = NULL

checkForPVC = function(x) { 
  
  i_coupl= x[7] # coupling interval
  i_comp = x[8] # compensatory interval
  i_pre = x[2:6] # preceding intervals
  i_post = x[9:24] # following intervals (after comp. interval)
  i_RRnorm = c(i_pre, i_post) # all RR-intervals that need to be filtered and aren't coupling or comp. interval
  
  ref = mean(i_pre)

    # checks for PVC
  if (i_coupl <= ref*0.8) { # Prematurity of 20%
    if (i_comp >= ref*1.2) { # Lengthening of post PVC interval
      
      # checks for arrhythmias and artefacts
      if (all(i_RRnorm > 300)) {
        if (all(i_RRnorm < 2000)) {
          if (all(i_RRnorm >= ref*0.8) & all(i_RRnorm <= ref*1.2)) {
            if (all(rollapply(i_pre, 2, diff)) && all(rollapply(i_post, 2, diff))) { # checks for difference to preceding / following interval
                PVC =c(PVC, i_coupl)
            }
          }
        }
      }
    }
  }
}

