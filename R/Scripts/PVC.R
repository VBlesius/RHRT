PVC = setClass("PVC", slots=list(couplI="numeric", compI="numeric", postRR="vector"))

setGeneric("getHRTParameters", def=function(thisObject, string){standardGeneric("getHRTParameters")})
setMethod("getHRTParameters", "PVC", function(thisObject, string){
  print(thisObject)
  })