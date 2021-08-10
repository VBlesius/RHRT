###-----------------------------------------------------------------------------
# Part of RHRT: R package to assess Heart Rate Turbulence from RR interval data 
# Copyright (C) 2021 Valeria Blesius

# RHRT is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, version 2 only.

# RHRT is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with RHRT.  If not, see <https://www.gnu.org/licenses/>.
###-----------------------------------------------------------------------------

#' Apply method on sliding window
#'
#' Applies a given function on a vector by rolling over it with a sliding window
#' mechanism.
#'
#' This method was inspired by the function "wapply" by A. N. Spiess, University
#' Hospital Hamburg-Eppendorf (https://rmazing.wordpress.com/2013/04/23/wapply-a-faster-but-less-functional-rollapply-for-vector-setups/),
#' but adjusted for this package to speed it up.
#'
#' @param intervals vector
#' @param width window size
#' @param fun function to be applied
#' @param ... additional arguments for FUN
#' @return (list) List with return values of fun for each window
roll <- function(intervals, width, fun, ...) {
  width <- width - 1
  l <- length(intervals)
  indicesStart <- seq(1, l - width)
  indicesEnd <- seq(1 + width, l)
  lapply(indicesStart, function(i) {
    fun(intervals[indicesStart[i]:indicesEnd[i]], ...)
  })
}
