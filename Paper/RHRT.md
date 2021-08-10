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
  - name: Valeria Blesius^[corresponding author]
    orcid: 0000-0002-2391-242X
    affiliation: "1"
  - name: Andreas Dominik^[co-author]
    orcid: 0000-0002-9368-0812
    affiliation: "1"
affiliations:
 - name: THM University of Applied Sciences, Giessen, Germany
   index: 1
date: 17 June 2021
bibliography: RHRT.bib

---

[](250 - 1000 words)
[](Mention if applicable a representative set of past or ongoing research projects using the software and recent scholarly publications enabled by it)

# Summary

[](describing the high-level functionality and purpose of the software for a diverse, non-specialist audience)

Heart Rate Turbulence (HRT) is a naturally occurring phenomenon of the heart that was first described by @schmidt_heart-rate_1999.
After a premature ventricular contraction (VPC) – an atypical heart beat that originates in the ventricles and occurs with a lower latency than a regular beat – the heart rate fluctuates.
This fluctuation consists of a fast increase and later decrease in heart rate.
This variability of interval lengths is an indicator for the condition of the autonomic nervous system:
While no reaction to the premature ventricular beat and therefore no variation suggests an underlying pathology, a distinctive turbulence is considered healthy.
Therefore, HRT can be used in medicine to determine the risk status of a person especially with certain diseases or conditions, e.g. after a myocardial infarction. [reviewed in @bauer_heart_2008]

``RHRT`` finds occurrences of HRT in heart beat interval data, calculates the most used parameters (Turbulence Onset (TO), Turbulence Slope (TS), Turbulence Timing (TT) and normalised Turbulence Slope (nTS)) and plots the results.
The package works best and fastest when given annotation data, but can also find HRT based on commonly accepted filtering rules that were first published in @grimm_heart_2003.
Most filtering parameters and calculation methods can be freely adjusted to enable research on the methodology itself.
In addition to parameter calculation, ``RHRT`` can classify the data into common risk categories (HRT0-2 and HRTA-C) and estimate the reliability of the results based on the number and parameter values of the HRTs.

The package can be found on [CRAN](https://cran.r-project.org/package=RHRT) and [GitHub](https://github.com/VBlesius/RHRT).

# Statement of need

[](clearly illustrates the research purpose of the software)

Since it reflects the status of the autonomic nervous system, HRT is a feasible method to estimate the health risk of a person [@lombardi_origin_2011].
HRT analysis is already used for risk stratification in the clinical practice, although the optimal methodology for HRT assessment has still not been systematically set [@blesius_hrt_2020].
``RHRT`` is the first R package for HRT analysis and aims to enable further research about the usefulness of HRT and its methodology.

# Minimal example

To install ``RHRT`` use

`install.packages("RHRT")`

for the version on CRAN.
To install the continuosly developed version on GitHub you can use the devtools package:

`devtools::install_github("VBlesius/RHRT/RHRT")`



The most straightforward use of `RHRT` is to scan your interval data for valid HRTs and analyse the results:

``` r
library(RHRT)
## scan your interval data and save the results as an HRTList
### the input should be a numeric vector consisting of RR interval data
### testdataLong is dummy data included in the package
hrtl <- vectorToHRT(testdataLong)

## get the HRT class of your data
getResults(hrtl, type = "class")
#> [1] "HRT0"

## have a look at the data and the parameters
plot(hrtl)
```
![Plot of an HRTList Object: the plot resembles the standard visualisation of HRT – a tachogram – in which the indices of the intervals are plotted against their lengths. The tachogram and HRT parameters TO and TS are drawn in black, red and blue, respectively, while the tachograms of all underlying HRTs are drawn in grey in the background. The plot is zoomed in by default to show the HRT parameter values more precisely, so the intervals before and after the VPC are outside the plot range. RR interval: interval between two heartbeats measured between the R-peaks in the ECG, couplRR: coupling interval (interval between last sinus induced contraction and VPC), compRR: compensatory interval (interval between VPC and following sinus induced contraction), TO: Turbulence Onset, TS: Turbulence Slope. \label{fig:plot}](../RHRT/man/figures/README-example-1.png)

More examples and a detailed description of the objects and functions can be found in the [vignette](https://github.com/VBlesius/RHRT/blob/main/RHRT/vignettes/rhrt-vignette.md) of the package.

# Acknowledgements
We are sincerely grateful to [Christopher Schölzel](https://orcid.org/0000-0001-8627-0594) for testing the package and providing excellent input through the course of its development.

# References