#' S4 class to represent a HRT object
#'
#' This class specifies a object to save the lengths of intervals surrounding a
#' premature ventricular beat. It saves the HRT parameters turbulence onset
#' and turbulence slope after calculation as well as the coefficients of an
#' ab-line used for the plot.
#'
#' @slot coupl_rr Numeric, Coupling interval
#' @slot compen_rr Numeric, Compensatory interval
#' @slot pre_rrs Numeric vector, Preceding 6 intervals
#' @slot post_rrs Numeric vector, Following 16 intervals
#' @slot to Numeric, Turbulence onset
#' @slot ts Numeric, Turbulence slope
#' @slot abline_coefficients Numeric vector, Intercept and slope of ab-line
#'
hrt <- setClass("hrt",
                slots = list(
                coupl_rr = "numeric",
                compen_rr = "numeric",
                pre_rrs="vector",
                post_rrs = "vector",
                to = "numeric",
                ts = "numeric",
                abline_coefficients = "vector")
)


setGeneric("get_hrt_params", def = function(thisObject) {
  standardGeneric("get_hrt_params")
})
#' @describeIn hrt Calculates HRT parameters turbulence onset (to) and
#' turbulence slope (ts) and the ab-line parameters of ts
setMethod("get_hrt_params", "hrt", function(thisObject) {
  pre_rrs <- thisObject@pre_rrs
  post_rrs <- thisObject@post_rrs

  # Calculate TO
  thisObject@to <- ( (sum(post_rrs[1:2]) - sum(pre_rrs) ) / sum(pre_rrs) ) * 100

  # Calculate TS
  slopes <- wapply(post_rrs, 5, by = 1, FUN = function(y)
    return(lm(y ~ seq(1,5))$coefficients[2])
  )
  thisObject@ts <- max(slopes, na.rm = TRUE)

  # Calculate coefficients for regression line in plot
  index <- which.max(slopes)
  model <- lm(post_rrs[index:(index + 4)]~seq(1, 5))

  slope <- model$coefficients[2]
  intercept <- model$coefficients[1] - (slope * (3 + index))
    # 3 = #pre_rrs + #irregular_rrs - 1

  thisObject@abline_coefficients <- c(intercept, slope)

  return(thisObject)
})

setGeneric("get_rrs", def = function(thisObject) {
  standardGeneric("get_rrs")
})
setMethod("get_rrs", "hrt", function(thisObject) {
  return(c(thisObject@pre_rrs,
           thisObject@coupl_rr,
           thisObject@compen_rr,
           thisObject@post_rrs))
})

#' @describeIn hrt Plots RR-intervals saved in the hrt object and marks
#' turbulence onset and turbulence slope.
setMethod("plot", "hrt", function(x, y = NULL, type = "cropped") {

  rrs <- get_rrs(x)

  plot(seq(1:length(rrs)), rrs,
       "o", pch = 20,
       xlab = "# of RR interval",
       ylab = "length of RR interval (ms)",
       xaxt = "n",
       ylim = if(type != "full")
         c(mean(rrs) - sd(rrs) / 2, mean(rrs) + sd(rrs) / 2)
  )

  axis(1, at = seq(1:length(rrs)), labels = seq(-2, length(rrs) - 3, 1))
  legend("bottomright", c("Turbulence onset", "Turbulence slope"),
         lty = c(3), pch = c(19, NA), col = c("red", "blue"))

  # Turbulence onset
  points(c(1, 6), c(rrs[1], rrs[6]), col = "red", pch = 19)
  arrows(1, rrs[1], 6, rrs[1], lty = 3, col = "red", code = 0)
  arrows(6, rrs[1], 6, rrs[6], lty = 3, col = "red", code = 2)

  # Turbulence slope
  abline(coef = x@abline_coefficients, lty = 3, col = "blue")

})
