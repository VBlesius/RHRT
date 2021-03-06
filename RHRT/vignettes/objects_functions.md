---
title: "RHRT: Objects and Functions"
author: "Valeria Blesius"
date: "2021-09-17"
output: 
  rmarkdown::html_vignette:
    keep_md: true
vignette: >
  %\VignetteIndexEntry{RHRT: Objects and Functions}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---

This vignette gives a detailed overview of all objects and functions provided by RHRT.

--------

## HRT Object

### Slots

An HRT object saves the data of one VPCS and its HRT results. It consists of the following slots:

- **Intervals**
  - `preRRs`: preceding regular intervals, 5 per default
  - `couplRR`: CPI
  - `compRR`: CMI
  - `postRRs`: following regular intervals, 15 per default

- **HRT Parameters**
  - `TO`
  - `TS`
  - `TT`
  - `nTS`: normalised TS, for more Information see chapter *Normalisation of Turbulence Slope* in [Scientific Background](background.md)

- **Line coefficients**
  - `intercept`: The intercept of the TS regression line that is needed for plotting
  - `nintercept`: Analogously, the intercept of `nTS`

### Functions

- **`getRRs`** &nbsp;&nbsp; This function returns all intervals saved in the HRT object. This if helpful for i.e. calculating an averaged VPCS out of a set of HRT objects.

- **`plot`** &nbsp;&nbsp; Per default `plot` displays a zoomed in plot of the VPCS with highlighted TO and TS and a legend given the rounded HRT parameter values. In addition to the common parameters of graphics.plot the method accepts the following parameters:
  - `cropped`: switches between showing a zoomed in version and the full VPCS including CPI and CMI, the default is `TRUE`
  - `add`: adds the plot to the current one
  - `TT`: highlights TT and includes it to the legend
  - `paramsLegend`: switches between showing and hiding the parameter values in the legend, the default is `TRUE`
  - `colTO`, `colTS` and `colTT` determine the colour in which the different parameters are highlighted in the plot (red, blue and purple per default).

## HRTList Object

An HRTList object is created when `vectorToHRT` is called. All HRT parameters are calculated automatically in this process and can be called with the `getHRTParams`-methods.

### Slots

The HRTList object sums up all HRTs found in the given dataset. The slots are:

- `name`: The name of the object if given to `vectorToHRT`
- `IL`: the average length of all intervals in the given cleaned vector (see *vectorToHRT: Cleaning Input* for more information) 
- `pos`: the indices of the CPIs as found in the given vector
- `HRTs`: list of all HRT objects found
- `avHRT`: an avHRT object averaged from all HRTs
- `RMSSD`: the HRV parameter RMSSD calculated from all intervals in the given cleaned vector (see *vectorToHRT: Cleaning Input* for more information): RMSSD is the square root of the mean of the squared respective differences of the successive RR intervals, in R calculated as `sqrt(mean(diff(intervals)^2))`

### Functions

- **`calcAvHRT`** &nbsp;&nbsp; The function calculates the parameters of the averaged HRT. This is called automatically when using `vectorToHRT` with default parameters. If the avHRT should be calculated differently the following options are available:
  - `av`: The function with which averaging should be done: `mean` or `median`.
  - `orTO` and `orTS`: sets the order in which TO and TS should be calculated. With `avAfter` the parameter is assessed separately for every HRT and averaged afterwards, with `avBefore` the intervals of all VPCSs are averaged first and the parameter is assessed afterwards. The default is `avAfter` for `orTO` and `avBefore` for `orTS`.
  - `IL`: the average interval length that is needed to calculate nTS. The default value is automatically calculated from the whole cleaned vector (see *VectorToHRT: Cleaning Input* for more information) when calling `vectorToHRT` and saved in the `IL` slot of the HRTList object. 
  - `normIL`: the interval length to which the other parameters should be normalised. The default is 800 ms.
  - `normHallstrom`: Should nTS be normalised with the method by Hallstrom et al. or just based on the interval length? The default is `TRUE`.
  - `coTO`, `coTS` and `coTT`: The cut-off that should be used to calculate the reliability check for the different parameters. The default is `coTO` 0, `coTS` 2.5 and `coTT` 10.
- **`getResults`** &nbsp;&nbsp; This function returns either the HRT class or the parameter values. You can determine the output with
  - `type`: "class" returns the HRT class (system HRT0-2 or HRTA-C depending on `TT`), "parameter" returns the HRT parameters and "full" returns the HRT parameters and the p-values of the reliability check.
  - `TT`: Should TT be included in the return? The default is `TRUE`.
  - `nTS`: Switches between giving TS (default) and nTS.
  - `safe`: Per default `safe` is `TRUE` so only results that are reliable are returned. For not reliable results the function returns "NR" or, if `num` is `TRUE`, `NA`.
  - `pmax`: The cut-off of the p-value to determine reliability. Per default this is 0.05.
  - `num`: Forces the function to return numerics. Keep in mind that this is inapplicable when using `type` "class", in this case the function gives a warning and returns `NA`. With `type` "full" `num` is ignored, because in that case the result is already numeric.
  - `coTO`, `coTS` and `coTT`: Analogously to `calcAvHRT` the cut-off that should be used to determine whether the parameter values are normal. The default is `coTO` 0, `coTS` 2.5 and `coTT` 10. Be sure to give the same cut-offs to `calcAvHRT` and `getResults` if you don't use the result, otherwise the p-values won't match the results.
- **`getHRTParams`** &nbsp;&nbsp; Returns the values of the given slot of all HRT objects in the `HRTList.` This can be used to quickly list all separate HRT parameters of an `HRTList`. Although the function name focuses on the HRT parameters, it can return any other slot of the HRT objects.
- **`getPositions`** &nbsp;&nbsp; Returns the positions of the couplRRs which is identical to `HRTList@pos`.
- **`plot`** &nbsp;&nbsp; Analogously to the plot function of the HRT object HRTList objects can be plotted. This function plots the avHRT and adds the VPCSs of all HRTs as grey lines in the background.

## avHRT Object

An avHRT object is stored in an HRTList and inherits from the HRT object. It is averaged from the other HRTs in the HRTList automatically and can be recalculated with calcAvHRT.

### Slots

In addition to the HRT slots avHRT stores data about its calculation and the parameter validity:

- `av`, `orTO` and `orTS`: for more information see *HRTList: Functions (calcAvHRT)*
- `pTO`, `pTS`, `pTT` and `pnTS`: p-values from the reliability check, for more information see *Reliability Check* in [Scientific Background](background.md)
- `nRMSSD`: the RMSSD normalised to the a given heart rate, per default to 75 bpm

## vectorToHRT

This is the core function of the package. It finds VPCs, checks the respective VPCS for validity and saves the results in an HRTList object. Its parameters are

- `input`: RR interval data that should be searched for HRT. Data formatted as timestamps should be converted before using `vectorToHRT`.
- `annotations`: If no annotations are given `vectorToHRT` searches for matching patterns of interval lengths in the given vector regardless of any other information in respect to the type of beats. Therefore, the function could also save HRTs based on atrial premature complexes or other arrhythmia if the surrounding intervals match the filter rules (for more information see *Methods & Background: Filter Rules*). If annotations are given the function only checks the intervals marked to stem from ventricular beats. This leads to more accurate results and speeds up the runtime considerably. The annotations should match the beats *at the end* of the intervals. 
- `PVCAnn`: A character or string with which the VPCs are marked in the annotation vector. The default is "V".
- `normIL`: The interval length to which the other parameters should be normalised. The default is 800 ms.
- `normHallstrom`: Should nTS be normalised with the method by Hallstrom et al. or just based on the interval length? The default is `TRUE`.
- `numPreRRs` and `numPostRRs`: The number of regular intervals before and after the CPI and CMI, respectively, on which the filter rules are applied and from which TS and nTS are being calculated. The default is 5 and 15, respectively.
- `inputName`: You can give a name to the `HRTList` to easier organise your data. If no name is given, the slot is set with `NA`. 
- `minHRT`: This sets the minimal amount of HRTs that have to be found. Per default an `HRTList` is only created if the vector contains 5 or more HRTs.
- `cleaning`: To calculate `IL` and `RMSSD` the data is cleaned per default. (for more information see *VectorToHRT: Cleaning Input*).

### Cleaning Input
The `IL` and `RMSSD` can be highly biased through outliers. Since ECG data can include artefacts, especially at the end and the beginning, RHRT cleans the data before calculating these parameters. Intervals are removed if they

- are greater than 2000 or less than 300 ms or
- differ more than 20 % of their own value from the next interval.

--------

Further information can be found in the other vignettes: [synopsis](synopsis.md), [scientific background](background.md) and [example pipelines](examples.md).

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
