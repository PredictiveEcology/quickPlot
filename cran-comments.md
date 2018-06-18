## resubmission

This is a minor update to our package.

## Test environments

### Previous R versions
* Ubuntu 14.04        (travis-ci), R 3.1.0
* Ubuntu 14.04        (travis-ci), R 3.2.0
* Ubuntu 14.04        (travis-ci), R 3.3.0
* Ubuntu 14.04        (travis-ci), R 3.4.0
* Windows              (appveyor), R 3.1.0
* Windows              (appveyor), R 3.2.0
* Windows              (appveyor), R 3.3.0
* Windows              (appveyor), R 3.4.0

### Current R versions
* macOS High Sierra    (local), R 3.5.0
* OS X El Capitan  (travis-ci), R 3.5.0
* Ubuntu 14.04     (travis-ci), R 3.5.0
* Ubuntu 18.04         (local), R 3.5.0
* Windows           (appveyor), R 3.5.0
* Windows        (win-builder), R 3.5.0
* Windows 7            (local), R 3.5.0

### Development R version
* Ubuntu 14.04     (travis-ci), R 3.6.0 (2018-06-15 r74903)
* Ubuntu 18.04         (local), R 3.6.0 (2018-06-15 r74903)
* Windows           (appveyor), R 3.6.0 (2018-06-11 r74889)
* Windows        (win-builder), R 3.6.0 (2018-06-15 r74904)

## R CMD check results

There are no errors, warnings, or notes in any of the above.

There were 1 NOTE, which is not a new note for this package:

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
