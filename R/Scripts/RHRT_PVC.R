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
  
  # Calculate TO
  thisObject@TO = ( (sum(postRR[1:2]) - sum(preRR) ) / sum(preRR) ) *100
  
  #Calculate TS
  #slopes = rollapply(postRR, 5, function(y) return(lm(seq(1,5) ~ y)$coefficients[2]))
  slopes = wapply(postRR, 5, by = 1, FUN = function(y) return(lm(y ~ seq(1,5))$coefficients[2])) # andersrum? y ~ x?
  thisObject@TS = max(slopes, na.rm = TRUE)
  
  return(thisObject)
})


setGeneric("listToPVC", def=function(list) {standardGeneric("listToPVC")})
setMethod("listToPVC", NULL, function(list) {
  if(is.null(list)) {
    return(NULL)
  } else {# TODO: Check for usable format!
    return(PVC(
      couplI=list$i_coupl,
      compI=list$i_comp,
      preRR=list$i_pre,
      postRR=list$i_post
    ))
  }
})
