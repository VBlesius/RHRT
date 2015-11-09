path = "/home/etz/Documents/Promotion/Data/CHF2DB/"

chf201 = read.table(file.path(path , "chf201.ecg.txt"))
chf201 = unlist(chf201*1000)

require(zoo)
rollapply(chf201, 22, checkForPVC) # 5 RR needed before PVC, 1 compensatory interval, 15 after PVC = 26, see http://www.h-r-t.com/hrt/en/calc.html
# Stattdessen
## wapply? http://www.r-bloggers.com/wapply-a-faster-but-less-functional-rollapply-for-vector-setups/ 
## RcppRoll package?

checkForPVC = function(x) { 
  
  i_coupl= x[6] # coupling interval
  i_comp = x[7] # compensatory interval
  i_pre = x[1:5] # preceding intervals
  i_post = x[8:22] # following intervals (after comp. interval)
  
  ref = mean(i_pre)

    # checks for PVC
  if (i_coupl <= ref*0.8) { # Prematurity of 20%
    if (i_comp >= ref*1.2) { # Lengthening of post PVC interval
      
      # checks for arrhythmias and artefacts
      if (all(i_pre > 300)) {
        if (all(i_pre < 2000)) {
          if (all(rollapply())) {
            
          }
        }
      }
    }
  }
  
  
}

