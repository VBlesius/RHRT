PVC = setClass("PVC", 
               slots=list(
                 couplI="numeric",
                 compI="numeric",
                 preRR="vector",
                 postRR="vector",
                 TO="numeric",
                 TS="numeric")
)


setGeneric("getHRTParameters", def=function(thisObject) {standardGeneric("getHRTParameters")})
setMethod("getHRTParameters", "PVC", function(thisObject) {
  preRR = thisObject@preRR
  postRR = thisObject@postRR
  thisObject@TO = ( (sum(postRR[1:2]) - sum(preRR) ) / sum(preRR) ) *100
  thisObject@TS = 2
  return(thisObject)
})


setGeneric("vectorToPVC", def=function(vector) {standardGeneric("vectorToPVC")})
setMethod("vectorToPVC", NULL, function(vector) {
  len = length(vector)
  PVC_LEN = 19
  PVCnumber = len/PVC_LEN
  
  if(len%%PVC_LEN != 0 ) {
    print("Vector can't be made into PVC-objects, length isn't a multiple of ", PVC_LEN)
  } else {
    PVClist = list()
      for(i in 0:(PVCnumber-1)) {
        offset=PVC_LEN*i
        PVClist = c(PVClist, PVC(preRR=vector[1:2]+offset, couplI=vector[3+offset], compI=vector[4+offset], postRR=vector[5:PVC_LEN]+offset))
      }
    return(PVClist)
  }
})
