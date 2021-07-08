## Updated release

This is a minor update with a key new function. This update, however, is required in revdep packages.

## Test environments

### Previous R versions
* Ubuntu 20.04                 (GitHub), R 3.6.3
* Ubuntu 20.04                 (GitHub), R 4.0.5
* Windows                      (GitHub), R 3.3.3
* Windows                      (GitHub), R 3.6.3
* Windows                      (GitHub), R 4.0.5
* Windows                 (win-builder), R 4.0.5

### Current R versions
* macOS 10.15.7 Catalina       (GitHub), R 4.1.0
* macOS 11.1 Big Sur            (local), R 4.1.0
* Ubuntu 20.04                 (GitHub), R 4.1.0
* Ubuntu 20.04                  (local), R 4.1.0
* Windows                      (GitHub), R 4.1.0
* Windows                       (local), R 4.1.0
* Windows                 (win-builder), R 4.1.0

### Development R version
* Ubuntu 20.04                 (GitHub), R-devel (2021-07-03 r80596)
* Ubuntu 20.04                  (local), R-devel (2021-07-05 r80598)
* Windows                      (GitHub), R-devel (2021-07-03 r80596)
* Windows                 (win-builder), R-devel (2021-07-03 r80596)

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
