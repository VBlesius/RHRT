---
title: "RHRT: Synopsis for the hasty (Quick-start guide)"
author: "Valeria Blesius"
date: "2021-08-12"
output: 
  rmarkdown::html_vignette:
    keep_md: true
vignette: >
  %\VignetteIndexEntry{RHRT: Quick-start guide}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---

The RHRT package helps you assess **Heart Rate Turbulence** (HRT) in RR intervals and calculate **turbulence onset** (TO), **slope** (TS) and **timing** (TT). It can plot the tachograms and checks the results for reliability. The **ventricular premature beats** (VPCs) with **coupling** (CPI) and **compensatory interval** (CMI) can either be given with annotations or found on the basis of the filter rules as first described  by [Grimm et al. 2003](https://doi.org/10.1046/j.1542-474X.2003.08206.x). The type of average and order of calculation for all parameters can be set.

This vignette sums up the most common functions and parameters needed when using RHRT.

--------

## Loading package and data


```r
library("RHRT")
# testdataLong is a numeric vector of RR intervals in msec
data("testdataLong", package = "RHRT")
ints <- testdataLong
# testdataLong_Ann is a character vector of annotations corresponding to testdataLong
data("testdataLong_Ann", package = "RHRT")
ann <- testdataLong_Ann
```

## Checking interval data for HRTs

The **core function** of RHRT is `vectorToHRT` that finds valid VPCs in RR intervals and returns an `HRTList` object (see *HRTList object* in [Objects & Functions](objects_functions.md) for more information):


```r
hrtl <- vectorToHRT(ints) 
```

Every RR interval sequence that matches the needed interval lengths is considered to be a coupling and compensatory interval of a VPC, which can lead to wrong matches. If your data is annotated, you can provide the **annotation data** with the parameters `annotations` and `PVCAnn`.


```r
hrtl <- vectorToHRT(ints, annotations = ann, PVCAnn = "V")
```

Other parameters are:

* `numPreRRs` & `numPostRRs` are used to modify the **filter rules** to find HRTs (number of intervals before and after the VPC that have to match the filter criteria).
* `minHRT` is the **minimal number of HRTs** needed to calculate HRT / create a HRTList
* `normHallstrom` defines whether TS should be **normalised** with the method of Hallstrom et al. (see the chapter *Normalisation of Turbulence Slope* in the [scientific background](background.md) for more information). 

## Getting HRT parameters or class

```r
getResults(hrtl) # get the HRT class of the data
```

```
## [1] "HRT0"
```

Per default `getResults` checks whether all needed HRT parameters can be calculated reliably. This is done via a t-test per parameter value (for more information see chapter *Reliability Check* in the [scientific background](background.md) vignette). If any of the parameter values is **not reliable** `getResults` returns NR (not reliable). 


```r
getResults(hrtl, safe = FALSE) # get the HRT class without safety check
```

```
## [1] "HRT0"
```

In addition to the classification system HRT0-2 RHRT implements **HRTA-C** that is based on the three parameters TO, TS and TT. 


```r
getResults(hrtl, safe = FALSE, TT = TRUE) # include TT
```

```
## [1] "HRTA"
```

With the parameter `type` you can choose between getting only the HRT **class**, all **parameter values** or the parameter values with the corresponding **p-values** (types "class", "parameter" or "full", respectively).


```r
getResults(hrtl, type = "parameter", TT = TRUE) # get the averaged HRT parameters
```

```
##        TO        TS        TT 
## -10.57551  37.61417   3.00000
```

Other parameters are:

* `nTS`: the **normalised TS** is returned or used for classification instead of TS.
* `num`: forces the function to return **numerics** when using `type = parameter`. Depending on the results and your setting of `type` the `getResults` returns characters or numerics.
* `pmax`: changes the needed **significance level** for the parameters to be reliable.

## Plotting


```r
plot(hrtl, TT = TRUE) # plots the averaged VPCS and all underlying VPCSs in background
```

![](synopsis_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

Per default the VPCS is **zoomed** in. If you want to also see the CPI and CMI use `cropped = FALSE`.


```r
plot(hrtl, cropped = FALSE) # shows also coupling and compensatory interval
```

![](synopsis_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

--------

Further information can be found in the other vignettes about the [objects and functions](objects_functions.md), the [scientific background](background.md) or with [example pipelines](examples.md).

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
