## Updated release

This is a maintenance release which drops support for R < 4.0.

## Test environments

### Previous R versions
* Ubuntu 20.04                 (GitHub), R 4.0.5
* Ubuntu 20.04                 (GitHub), R 4.1.3
* Windows                      (GitHub), R 4.0.5
* Windows                      (GitHub), R 4.1.3
* Windows                 (win-builder), R 4.1.3

### Current R versions
* macOS 11.6 Big Sur           (GitHub), R 4.2.1
* macOS 11.6 Big Sur            (local), R 4.2.1
* macOs (m1) Big Sur             (rhub), R 4.2.1
* Ubuntu 20.04                 (GitHub), R 4.2.1
* Ubuntu 20.04                  (local), R 4.2.1
* Windows                      (GitHub), R 4.2.1
* Windows                       (local), R 4.2.1
* Windows                 (win-builder), R 4.2.1

### Development R version
* Ubuntu 20.04                 (GitHub), R-devel (2022-08-11 r82713)
* Ubuntu 20.04                  (local), R-devel (2022-08-11 r82713)
* Windows                      (GitHub), R-devel (2022-08-14 r82716 ucrt)
* Windows                 (win-builder), R-devel (2022-08-14 r82716 ucrt)
## R CMD check results

There are no errors, or warningsin any of the above.

There are 2 NOTEs, which are not new notes for this package:

    Package suggested but not available for checking: 'fastshp'

We provide instructions for the user to install the suggested `fastshp` package.

The `fastshp` package in Suggests is optionally installed from Rforge and not required to use the package; it simply speeds up some of the plotting. Instructions for installation are provided in the README and via a message to the user. We believe this should satisfy the CRAN policy requirement regarding additional dependencies.

        Suggests or Enhances not in mainstream repositories:
          fastshp
      
        Availability using Additional_repositories specification:
          fastshp   yes   https://rforge.net

## Downstream dependencies

I have run R CMD check on downstream dependencies and all have passed, except as noted below.

Summary at https://github.com/PredictiveEcology/quickPlot/blob/master/revdep/README.md.

Packages `SpaDES.core` and `reproducible` produced ERRORs, which are fixed in forthcoming releases, to be submitted to CRAN shortly.
