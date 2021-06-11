---
title: 'RHRT: An R package to assess Heart Rate Turbulence'
tags:
  - Heart Rate Turbulence
  - R package
  - data analysis
  - Cardiology
  - arrhythmia
  - ventricular premature beats
authors:
  - name: Valeria Blesius
    orcid: 0000-0002-2391-242X
    affiliation: "1"
affiliations:
 - name: THM University of Applied Sciences, Giessen, Germany
   index: 1
date: 27 May 2021
bibliography: RHRT.bib

---

[](250 - 1000 words)
[](Mention if applicable a representative set of past or ongoing research projects using the software and recent scholarly publications enabled by it)

# Summary

[](describing the high-level functionality and purpose of the software for a diverse, non-specialist audience)

Heart Rate Turbulence (HRT) is a naturally occuring phenomenon of the heart that was first described by @schmidt_heart-rate_1999.
After a premature ventricular beat – an atypical heart beat that originates in the ventricles and occurs with a briefer latency than a regular beat – the heart rate fluctuates.
This fluctuation consists of a fast increase and later decrease in heart rate.
This variability of interval lengths depends on the condition of the autonomic nervous system:
While no reaction to the premature ventricular beat and therefore no variation suggests an underlying pathology, a distinctive turbulence is considered healthy.
Therefore HRT can be used in medicine to determine the risk status of a person especially with certain diseases or conditions, e.g. after a myocardial infarction.

``RHRT`` finds occurences of HRT in heart beat interval data, calculates the most used parameters and plots the results.
The package works best and fastest when given annotation data, but can also find HRT based on commonly accepted filtering rules [@grimm_heart_2003].
Most filtering parameters and calculation methods can be set freely to enable research on the methodology itself.
In addition to parameter calculation, ``RHRT`` can classify the data into common risk categories and estimate the reliability of the results based on the number and parameter values of the HRTs.

The package can be found on [CRAN] and [GitHub](https://github.com/VBlesius/RHRT).

# Statement of need

[](clearly illustrates the research purpose of the software)

HRT is a feasible method to estimate the health risk of a person since it reflects the status of the autonomic nervous system.
HRT analysis is already used for risk stratification in the clinical practice, although the optimal methodology for HRT assessment has still not been systematically set [@blesius_hrt_2020].
``RHRT`` is the first R package for HRT analysis and aims to enable further research about HRT.

# Minimal example

To install ``RHRT`` use

install.packages("RHRT")

for the version on CRAN.
To install the continuosly developed version on GitHub you can use the devtools package:

devtools::install_github("vblesius/RHRT/RHRT")



The most straightforward use of `RHRT` is to scan your interval data for valid HRTs and analyse the results:

``` r
library(RHRT)
## scan your interval data and save the results as an HRTList
hrtl <- vectorToHRT(intervals)

## get the HRT class of your data
getResults(hrtl, type = "class")
#> [1] "HRT0"

## have a look at the data and the parameters
plot(hrtl)
```
![Caption for example figure.\label{fig:plot}](../RHRT/man/figures/README-example-1.png)

# Acknowledgements


# References