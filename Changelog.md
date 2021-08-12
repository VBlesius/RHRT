# Changelog

All notable changes of RHRT will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/), and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).


## [Unreleased] TBA


## [1.0.1] 2021-06-28

### Changed
- the reliability check returns now 0 as p-values (formerly NA) if parameter values are all identical (and there is more than one HRT in the HRTList)
### Fixed
- corrected testdata which was prone for round-off errors