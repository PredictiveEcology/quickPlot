## resubmission

This is a major update to our package.

## Test environments

### Previous R versions
* Ubuntu 14.04.5      (travis-ci), R 3.3.3 YES
* Windows              (appveyor), R 3.3.2 YES
* Windows              (appveyor), R 3.3.3 YES
* Windows 7               (local), R 3.3.3 YES

### Current R versions
* macOS High Sierra      (local), R 3.4.3
* OSX Sierra 10.12.6 (travis-ci), R 3.4.3 YES
* Ubuntu 16.0            (local), R 3.4.3 
* Ubuntu 14.04.5     (travis-ci), R 3.4.2 YES
* Debian 4.9.51          (local), R 3.4.3 Running
* Windows             (appveyor), R 3.4.3 YES
* Windows          (win-builder), R 3.4.3 Running -- waiting for email
* Windows 7              (local), R 3.4.3 YES

### Development R version
* Ubuntu 14.04        (travis-ci), R 3.5.0 (2018-01-24 r74157) YES
* Windows                 (local), R 3.5.0 (2018-01-24 r74157) Running
* Windows           (win-builder), R 3.5.0 (2018-01-24 r74157) Running -- waiting for email

## R CMD check results

There are no errors, warnings, or notes in any of the above.

There were 1 NOTE, which is not a new note for this package:

    Package suggested but not available for checking: 'fastshp'

We provide instructions for the user to install the suggested `fastshp` package.

The `fastshp` package in Suggests is optionally installed from Rforge and not required to use the package. Instructions for installation are provided in the README and via a message to the user. We believe this should satisfy the CRAN policy requirement regarding additional dependencies.

        Suggests or Enhances not in mainstream repositories:
          fastshp
      
        Availability using Additional_repositories specification:
          fastshp   yes   http://rforge.net



## Downstream dependencies

We have run R CMD check on all downstream dependencies, and all installable packages passed.
