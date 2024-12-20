---
title: "RHRT: Scientific Background"
author: "Valeria Blesius"
date: "2021-08-12"
output: 
  rmarkdown::html_vignette:
    keep_md: true
vignette: >
  %\VignetteIndexEntry{RHRT: Scientific Background}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---

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

This vignette gives descriptions and references regarding the scientific background of the package. RHRT includes the possibility for non-standard methodology in order to further analyse HRT and its optimal application. The respective sources and explanations of these variations can be found here. 

--------

## Filter Rules

To ensure snippets free of any bias and containing effective VPCs, the VPCSs are filtered based on their interval lengths. The first publication to mention filter rules was [Grimm et al.][FR1]. With little variations these are the criteria that are used in the package as possible VPCSs are only saved as HRT objects if they match the following criteria:

1) Filter rules for CPI and CMI:

    * CPI must have a maximal length of 80 % 
    * CMI must have a minimal length of 120 %

    Both intervals are compared to the **reference interval (RFI)**. This interval is calculated as the mean of the preceding intervals before the coupling interval.
<br><br>

2) Filter rules for regular intervals:

    * The length has to be between 300 ms and 2000 ms
    * They must not differ more than 20 % from RFI
    * or more than 200 ms from the preceding interval

    How many preceding and following intervals of CPI and CMI are checked is based on `numPreRRs` and `numPostRRs` of `vectorToHRT`. The default is 5 and 15, respectively.  If any of the intervals do not fit the rules, the complete set is neglected.

[FR1]: https://doi.org/10.1046/j.1542-474X.2003.08206.x "Grimm et al., Heart rate turbulence following ventricular premature beats in healthy controls, 2003, Ann. Noninvas. Electro. 8 127–31"

## Normalisation of Turbulence Slope

HRT is influenced by the heart rate. While there is no clear conclusion for TO, TS values clearly positively correlate with the RR interval length (reviewed in [Blesius et al. 2020][NTS1]). Therefore, RHRT calculates `nTS` that is normalised to a fixed interval length (800 ms per default) in addition to the common TS.

Beside the heart rate, TS is biased by the number of HRTs used to calculate it (reviewed in [Blesius et al. 2020][NTS1]). While physiological reasons were suggested for this phenomenon ([Cygankiewicz et al. 2004][NTS2] and [Chen 2009][NTS3]), [Hallstrom et al. 2004][NTS4] reasoned it to be a mathematically induced relation based on the number of VPCSs as well as the number of postRRs to determine TS. They proposed a method to normalise TS in which, firstly, TS is normalised to a HR of 75 bpm (which is 800 ms interval length). Here, it makes no mathematical difference whether TS is normalised or the intervals themselves before assessing TS. Secondly, the following formula is used:

&nbsp;&nbsp;&nbsp;&nbsp; nTS = TS - ( 0.02475 * (numPostRRs-2)^0.9449 * (RMSSD / √#VPCSs) )
    
RHRT uses this normalisation per default. This can be changed with the boolean parameter `normHallstrom` in `vectorToHRT` and `calcAvHRT`.

[NTS1]: https://doi.org/10.1088/1361-6579/ab98b3 "Blesius et al., HRT assessment reviewed: a systematic review of heart rate turbulence methodology, 2020, Physiol. Meas. 41 08TR01"
[NTS2]: https://doi.org/10.1046/j.1540-8167.2004.03613.x "Cygankiewicz et al., Relationship between heart rate turbulence and heart rate, heart rate variability, and number of ventricular premature beats in coronary patients, 2004, J. Cardiovasc. Electrophysiol. 15 731–7"
[NTS3]: https://doi.org/10.1111/j.1542-474X.2009.00322.x "Chen, Impact of preceding ventricular premature beats on heart rate turbulence, 2009, Ann. Noninvas. Electro. 14 333–9"
[NTS4]: https://doi.org/10.1109/TBME.2004.828049 "Hallstrom et al., Structural relationships between measures based on heart beat intervals: potential for improved risk assessment, 2004, IEEE. Trans. Biomed. Eng. 51 1414–20"

## Reliability Check
The HRT parameter values pre se do not give any information about 1) how many VPCSs have been used to determine them and 2) how reliable the values are. However, two identical values are inherently different if one is calculated from a VPCS with a highly varying values and the other from a high amount of VPCS with hardly any variation. Still, HRT classification generally does not take this into account.

RHRT implements a reliability check to give the opportunity to only use HRT parameter values that are reliable to a desired extent. This check consists of a one-sided t-test (`t.test` of the stats package) off all separate values against the respective cut-off of the parameter. The resulting p-value implicates the possibility of the classification being true based on being the combination of average and variability of the parameter values and therefore the reliability of the averaged value.

These t-tests are being done automatically during `calcAvHRT` which is called by `vectorToHRT`. The default values of the cut-offs are 0 for `TO`, 2.5 for `TS` as well as `nTS` and 10 for `TT`.
`getResults` returns the results if reliable. However, it returns all results ignoring the reliability check via the boolean parameter `safe` and changes the p-value cut-off with `pmax` (0.05 per default).

Keep in mind that the parameter value cut-offs `coTO`, `coTS` and `coTT` are only used to compare the values and classify them. They are not related to the identically named parameters of `calcAvHRT` that are used for the t-tests.

## Calculation Order
The order in which the HRT parameters are calculated has an impact on the resulting values ([Chen 2011][CO1]). Though [Schmidt et al. 1999][CO2] proposed to first calculate an averaged tachogram and determine TS then and for TO to first assess it from the separate VPCSs and average the results afterwards, the order gets switched in some studies as reviewed in [Blesius et al. 2020][CO3]. Therefore, RHRT gives the opportunity to change the calculation order for TO and TS through the parameters `orTO` and `orTS` of `calcAvHRT`. By default the order is as suggested by Schmidt et al. Additionally with `av` you can switch between `mean` and `median` as averaging function.

[CO1]: https://doi.org/10.4081/hi.2011.e7 "Chen, Implications of turbulence slope variations in different approaches, 2011, Heart Int. 6 21–5"
[CO2]: https://doi.org/10.1016/S0140-6736(98)08428-1 "Schmidt et al., Heart-rate turbulence after ventricular premature beats as a predictor of mortality after acute myocardial infarction, 1999, The Lancet 353 1390–6"
[CO3]: https://doi.org/10.1088/1361-6579/ab98b3 "Blesius et al., HRT assessment reviewed: a systematic review of heart rate turbulence methodology, 2020, Physiol. Meas. 41 08TR01"

--------

Further information can be found in the other vignettes: [synopsis](synopsis.md), [objects & functions](objects_functions.md) and [example pipelines](examples.md).

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
