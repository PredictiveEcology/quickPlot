## New submission

This is a spin off of an existing CRAN package (`SpaDES`), which we have split due to growing package size.

We have removed `\dontrun` from the examples so that they *do* run, added additional examples to non-internal functions, and have added the examples to our tests.

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
* Debian:testing (rocker/r-devel), R 3.5.0 (2017-07-26 r72972)
* Ubuntu 14.04        (travis-ci), R 3.5.0 (2017-08-02 r73018)
* Ubuntu 16.04            (local), R 3.5.0 (2017-08-02 r73018)
* Windows              (appveyor), R 3.5.0 (2017-08-01 r73012)
* Windows           (win-builder), R 3.5.0 (2017-08-01 r73011)

## R CMD check results

There were no ERRORs or WARNINGs

There were 2 NOTEs:

1. There are multiple parts to this note:

    a. This is a new package submission:
    
            Maintainer: 'Eliot McIntire <eliot.mcintire@canada.ca>'
            
            New submission

    b. Some words were flagged as possibly mispelled, but they are not. 
     
            Possibly mis-spelled words in DESCRIPTION: 
              Modularity (3:53)
              modularity (5:29)

    c. The `fastshp` package in Suggests is optionally installed from Rforge and not required to use the package. Instructions for installation are provided in the Description, README, and via a message to the user. We believe this should satisfy the CRAN policy requirement regarding additional dependencies.

            Suggests or Enhances not in mainstream repositories:
              fastshp
          
            Availability using Additional_repositories specification:
              fastshp   yes   http://rforge.net

    d. The URL used to install the suggested package `fastshp` should not be in angle brackets because it's part of an R command:

            The Description field contains
              repos = "http://rforge.net", type = "source")'.
              Please enclose URLs in angle brackets (<...>).

2. As noted above, we provide instructions for the user to install the suggested `fastshp` package.

        Package suggested but not available for checking: 'fastshp'

## Downstream dependencies

There are currently no downstream dependencies of this package.
However, as we submit further `SpaDES` spinoff packages, this package will become a dependency for the following packages:

- `SpaDES` (Imports)
- `SpaDES.addins` (Imports)
- `SpaDES.core` (Depends)
- `SpaDES.tools` (Imports)
