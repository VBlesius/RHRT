PVC <- setClass("PVC", 
               slots=list(
                 couplRR="numeric",
                 compRR="numeric",
                 preRRs="vector",
                 postRRs="vector",
                 TO="numeric",
                 TS="numeric")
)


setGeneric("getHRTParameters", def=function(thisObject) {standardGeneric("getHRTParameters")})
setMethod("getHRTParameters", "PVC", function(thisObject) {
  preRRs <- thisObject@preRRs
  postRRs <- thisObject@postRRs
  
  # Calculate TO
  thisObject@TO <- ( (sum(postRRs[1:2]) - sum(preRRs) ) / sum(preRRs) ) *100
  
  # Calculate TS
  slopes <- wapply(postRRs, 5, by=1, FUN=function(y) return(lm(y ~ seq(1,5))$coefficients[2]))
  thisObject@TS <- max(slopes, na.rm = TRUE)
  
  return(thisObject)
})
