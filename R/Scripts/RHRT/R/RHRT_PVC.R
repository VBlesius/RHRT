PVC <- setClass("PVC", 
               slots=list(
                 couplRR="numeric",
                 compenRR="numeric",
                 preRRs="vector",
                 postRRs="vector",
                 TO="numeric",
                 TS="numeric",
                 ablineCoefficients="vector")
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
  
  # Calculate coefficients for regression line in plot
  index <- which.max(slopes)
  model <- lm(postRRs[index:(index+4)]~seq(1,5))
  
  slope <- model$coefficients[2]
  intercept <- model$coefficients[1]-(slope*(3+index)) # 3 = #preRRs + #irregular RRs - 1
  
  thisObject@ablineCoefficients <- c(intercept, slope)

  return(thisObject)
})

setGeneric("getRRs", def=function(thisObject) {standardGeneric("getRRs")})
setMethod("getRRs", "PVC", function(thisObject) {
  return(c(thisObject@preRRs, thisObject@couplRR, thisObject@compenRR, thisObject@postRRs))
})

setMethod("plot", "PVC", function(x, type="cropped") {

  RRs <- getRRs(x)

  plot(seq(1:length(RRs)), RRs, 
       "o", pch=20,
       xlab="# of RR interval", ylab="length of RR interval (ms)", xaxt="n",
       ylim=if(type!="full") c(mean(RRs)-sd(RRs)/2, mean(RRs)+sd(RRs)/2))
 
  axis(1, at=seq(1:length(RRs)), labels=seq(-2,length(RRs)-3,1))
  legend("bottomright", c("Turbulence onset", "Turbulence slope"), 
         lty=c(3), pch=c(19, NA), col=c("red", "blue"))
  
  # Turbulence onset
  points(c(1,6), c(RRs[1], RRs[6]), col="red", pch=19)
  arrows(1, RRs[1], 6, RRs[1], lty=3, col="red", code=0)
  arrows(6, RRs[1], 6, RRs[6], lty=3, col="red", code=2)
  
  # Turbulence slope
  abline(coef=x@ablineCoefficients, lty=3, col="blue")
  
})