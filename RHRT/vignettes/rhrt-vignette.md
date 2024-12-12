# RHRT Vignette

## Introduction
The RHRT package helps you assess **Heart Rate Turbulence** (HRT) in RR intervals and calculate **turbulence onset** (TO), **slope** (TS) and **timing** (TT). It can plot the tachograms and checks the results for reliability. The **ventricular premature beats** (VPCs) with **coupling** (CPI) and **compensatory interval** (CMI) can either be given with annotations or found on the basis of the filter rules as first described  by [Grimm et al. 2003](https://doi.org/10.1046/j.1542-474X.2003.08206.x).

The package can not only assess HRT parameters and patient classification, but lets you adjust the different workflow paramaters. This feature helps to further analyse and standardise HRT methodology, e.g. to determine optimal patient classification for novel patient subgroups. For example, this has been done to determine the optimal number of intervals needed to calculate TS ([Blesius et al. 2022](https://doi.org/10.3389/fcvm.2022.793535)).

RHRT has also been published in the Journal of Open Source Software ([Blesius et al. 2021](https://doi.org/10.21105/joss.03540)).

## Structure

For better readability the vignette has been divided into several parts:

- The **[Synopsis](https://github.com/VBlesius/RHRT/blob/main/RHRT/vignettes/synopsis.md)** is a **quick-start guide** giving a brief overview about the basic functionality of the package.
- **[Objects and Functions](https://github.com/VBlesius/RHRT/blob/main/RHRT/vignettes/objects_functions.md)** describes all implemented classes, functions and their parameters.
- In **[Example Pipelines](https://github.com/VBlesius/RHRT/blob/main/RHRT/vignettes/examples.md)** the application of the package is shown in two workflows.
- The exact methodology that has been implemented and the corresponding scientific reasons can be found in the **[Scientific Background](https://github.com/VBlesius/RHRT/blob/main/RHRT/vignettes/background.md)**.

