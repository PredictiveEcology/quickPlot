## Updated package

This update adds exported methods to allow extensions by other packages, and provides some additional bug fixes.

## Test environments

### Previous R versions
* Ubuntu 14.04        (travis-ci), R 3.3.3
* Windows              (appveyor), R 3.3.3
* Windows 7               (local), R 3.3.3

### Current R versions
* macOS Sierra         (local), R 3.4.1
* OS X El Capitan  (travis-ci), R 3.4.1
* Ubuntu 14.04     (travis-ci), R 3.4.1
* Ubuntu 16.04         (local), R 3.4.1
* Windows           (appveyor), R 3.4.1
* Windows        (win-builder), R 3.4.1
* Windows 7            (local), R 3.4.1

### Development R version
* Debian:testing (rocker/r-devel), R 3.5.0 (2017-09-13 r73254)
* Ubuntu 14.04        (travis-ci), R 3.5.0 (2017-10-04 r73462)
* Ubuntu 16.04            (local), R 3.5.0 (2017-10-04 r73462)
* Windows              (appveyor), R 3.5.0 (2017-10-03 r73455)
* Windows           (win-builder), R 3.5.0 (2017-09-12 r73242)

## R CMD check results

There were no ERRORs or WARNINGs

There were 2 NOTEs:

1. There are multiple parts to this note:

    a. This is a new package submission:
    
            Maintainer: 'Eliot McIntire <eliot.mcintire@canada.ca>'

    b. Some words were flagged as possibly mispelled, but they are false positives.
     
            Possibly mis-spelled words in DESCRIPTION: 
              Modularity (3:53)
              modularity (5:29)

    c. The `fastshp` package in Suggests is optionally installed from Rforge and not required to use the package. Instructions for installation are provided in the README and via a message to the user. We believe this should satisfy the CRAN policy requirement regarding additional dependencies.

            Suggests or Enhances not in mainstream repositories:
              fastshp
          
            Availability using Additional_repositories specification:
              fastshp   yes   http://rforge.net

2. As noted above, we provide instructions for the user to install the suggested `fastshp` package.

        Package suggested but not available for checking: 'fastshp'

## Downstream dependencies

We have run R CMD check on all downstream dependencies, and all installable packages passed.
