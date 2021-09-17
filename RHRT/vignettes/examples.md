---
title: "RHRT: Example Pipelines"
author: "Valeria Blesius"
date: "2021-09-17"
output: 
  rmarkdown::html_vignette:
    keep_md: true
vignette: >
  %\VignetteIndexEntry{RHRT: Example Pipelines}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---

--------

## Determining the HRT class of a person

The main focus of the package is to determine the HRT parameters or class of a person by a long-term ECG measurement. Load the data as a numeric vector and use `vectorToHRT` to find HRTs, then `plot` and `getResults` to check the HRT:


```r
library("RHRT")
hrtl <- vectorToHRT(testdataLong) # create the HRTList
plot(hrtl, main = "Zoomed in Tachogram") # plot the HRTs and check the variability
```

![](examples_files/figure-html/unnamed-chunk-1-1.png)<!-- -->

```r
getResults(hrtl) # get the averaged HRT parameters
```

```
## [1] "NR"
```

The results do not pass the reliability check so we get "NR" instead of an HRT class. The plot shows that firstly TO is near to zero and secondly there is a high variability in the VPCSs. We can go deeper into the data by checking the exact parameters (including TT as an additional hint to the person's status) and zooming out of the plot:


```r
round(
  getResults(hrtl, "full", TT = TRUE),
digits = 2) # get the parameters and p-values of the variability check
```

```
##    TO    TS    TT   pTO   pTS   pTT 
## -0.48 26.82  3.00  0.38  0.00  0.00
```

```r
plot(hrtl, cropped = FALSE, main = "Full Tachogram") # plot the full VPCSs
```

![](examples_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

As expected TO is not reliable with a p-value over 0.05. The VPCSs still seem to fluctuate a lot. We can can get a picture of the individual TO values by using `getHRTParams`:


```r
tos <- getHRTParams(hrtl, "TO")
tos
```

```
##  [1]  -3.08977601  -2.91673793  -3.86304382 -11.81622053   1.48341915
##  [6]  -2.04437316   2.02537861   6.54482521   0.04640218   1.24190434
## [11]  10.01034320  -3.41808350
```

```r
summary(tos)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
## -11.816  -3.172  -0.999  -0.483   1.619  10.010
```

```r
par(mar=c(0, 3, 0, 0))
boxplot(tos)
```

![](examples_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

These results can help to come to a well-founded decision on whether to classify the patient as HRT0/HRTA and trust the TO value or rather classify them conservatively as HRT1/HRTB. 

## Comparing HRT results with different methodological parameters

This is an example how the package can be used to analyse the HRT methodology. For instance, we can compare the difference in `TO` and `TS` values when the order of the calculation steps are switched.


```r
library("RHRT")
hrtl <- vectorToHRT(testdataLong)
getResults(hrtl, type = "parameter", safe = FALSE)
```

```
##         TO         TS 
## -0.4829969 26.8151839
```

```r
hrtl@avHRT <- calcAvHRT(hrtl, orTO = "avBefore", orTS = "avAfter")
getResults(hrtl, type = "parameter", safe = FALSE)
```

```
##         TO         TS 
## -0.6618164 35.2097614
```

--------

Further information can be found in the other vignettes: [synopsis](synopsis.md), [objects & functions](objects_functions.md) and [scientific background](background.md).

<!---
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
-->
