PVC <- setClass("PVC", 
               slots=list(
                 couplRR="numeric",
                 compenRR="numeric",
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

setGeneric("getRRs", def=function(thisObject) {standardGeneric("getRRs")})
setMethod("getRRs", "PVC", function(thisObject) {
  return(c(thisObject@preRRs, thisObject@couplRR, thisObject@compenRR, thisObject@postRRs))
})

setMethod("plot", "PVC", function(x, type="cropped") {

  RRs <- getRRs(x)

  if(type=="full") {
    plot(seq(1:length(RRs)), RRs, 
         "o", pch=20, 
         xlab="# of RR interval", ylab="length of RR interval (ms)")
  } else {
    plot(seq(1:length(RRs)), RRs, 
         "o", pch=20, 
         ylim = c(mean(RRs)-sd(RRs)/2, mean(RRs)+sd(RRs)/2),
         xlab="# of RR interval", ylab="length of RR interval (ms)")
  }})