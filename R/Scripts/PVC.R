PVC = setClass("PVC", slots=list(couplI="numeric", compI="numeric", postRR="vector"))


setGeneric("getHRTParameters", def=function(thisObject, string) {standardGeneric("getHRTParameters")})
setMethod("getHRTParameters", "PVC", function(thisObject, string) {
  print(thisObject)
  })


setGeneric("vectorToPVC", def=function(vector) {standardGeneric("vectorToPVC")})
setMethod("vectorToPVC", NULL, function(vector) {
  len = length(vector)
  PVC_LEN = 17
  PVCnumber = len/PVC_LEN
  
  if(len%%PVC_LEN != 0 ) {
    print("Vector can't be made into PVC-objects, length isn't a multiple of ", PVC_LEN)
  } else {
    PVClist = list()
      for(i in 0:(PVCnumber-1)) {
        offset=PVC_LEN*i
        PVClist = c(PVClist, PVC(couplI=vector[1+offset], compI=vector[2+offset], postRR=vector[3:17]+offset))
      }
    return(PVClist)
  }
})