#' Apply method on sliding window
#' 
#' Applies a given function on a vector by rolling over it with a sliding window 
#' mechanism. 
#' 
#' @author A. N. Spiess, University Hospital Hamburg-Eppendorf
#' @source https://rmazing.wordpress.com/2013/04/23/wapply-a-faster-but-less-functional-rollapply-for-vector-setups/
#' 
#' @param x Vector
#' @param width windowsize
#' @param by Value how much to slide 
#' @param FUN function to be applied
#' @param ... additional arguments for FUN
#' @return vector with return values of FUN for each window
wapply <- function(x, width, by = NULL, FUN = NULL, ...) {
  FUN <- match.fun(FUN)
  if (is.null(by)) by <- width
  
  lenX <- length(x)
  SEQ1 <- seq(1, lenX - width + 1, by = by)
  SEQ2 <- lapply(SEQ1, function(x) x:(x + width - 1))
  
  OUT <- lapply(SEQ2, function(a) FUN(x[a], ...))
  OUT <- base::simplify2array(OUT, higher = TRUE)
  return(OUT)
}