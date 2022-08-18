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
* Windows                 (win-builder), R-devel (2022-08-17 r82724 ucrt)

## R CMD check results

There are no errors, or warnings in any of the above.

There are some NOTEs:

The `fastshp` and `visualTest` packages in Suggests are optionally installed from our R-universe repository.
Instructions for installation are provided in the README, DESCRIPTION, and via a message to the user.
We believe this should satisfy the CRAN policy requirement regarding additional dependencies.

        Suggests or Enhances not in mainstream repositories:
          fastshp, visualTest
        Availability using Additional_repositories specification:
          fastshp      yes   https://predictiveecology.r-universe.dev/
          visualTest   yes   https://predictiveecology.r-universe.dev/

Related to the above note:

        The Description field contains
          "https://PredictiveEcology.r-universe.dev")'.
        Please enclose URLs in angle brackets (<...>).

This URL is included as part of the command used to install these additional packages.

## Downstream dependencies

We checked 4 reverse dependencies, comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 0 packages
