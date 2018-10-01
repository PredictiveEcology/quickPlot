## Updated release

This is a minor update with a key new function. This update, however, is required in revdep packages.

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
* Ubuntu 14.04     (travis-ci), R 3.6.0 (2018-06-20 r74923)
* Ubuntu 18.04         (local), R 3.6.0 (2018-06-15 r74903)
* Windows           (appveyor), R 3.6.0 (2018-06-21 r74927)
* Windows        (win-builder), R 3.6.0 (2018-06-20 r74924)

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

* `SpaDES.core` produce a warning about a vignette that can't be built. That is to due a different dependency (`reproducible`), which is being updated by us in a CRAN submission shortly.
 
* `SpaDES` shows a warning about a cross reference to cover::shine that is missing. That is unrelated to `quickPlot`. We are the maintainers of `SpaDES` and have corrected that missing cross reference, to be submitted soon to CRAN.
