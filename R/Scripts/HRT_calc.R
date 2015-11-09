path = "/home/etz/Documents/Promotion/Data/CHF2DB/"

chf201 = read.table(file.path(path , "chf201.ecg.txt"))
chf201 = unlist(chf201*1000)

require(zoo)
rollapply(chf201, 26, checkForPVC) # 10 RR needed before PVC, 15 after PVC = 26, see http://www.h-r-t.com/hrt/en/calc.html
# Stattdessen
## wapply? http://www.r-bloggers.com/wapply-a-faster-but-less-functional-rollapply-for-vector-setups/ 
## RcppRoll package?

checkForPVC = function(x) {
  
  PVC = x[11]
  prePVC = x[10]
  postPVC = x[12]
  pre5PVCs = x[6:10]

    # checks for PVC
  if (PVC <= prePVC*0.8) { # Prematurity of 20%
    if (postPVC >= refLength*1.2) { # Lengthening of post PVC interval
      
      # checks for arrhythmias and artefacts
      if (all(pre5PVCs > 300)) {
        if (all(pre5PVCs < 2000)) {
          if (all(rollapply())) {
            
          }
        }
      }
    }
  }
  
  
}

