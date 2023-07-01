## Updated release

This is a medium update that further deals with migration to `terra` and `sf`, and removes `rgdal`, `rgeos`, and `maptools`. 

## Test environments

### GitHub Actions
- os: macOS-latest,   r: 'release'
- os: windows-latest, r: 'devel'
- os: windows-latest, r: 'latest'
.- os: windows-latest, r: 'oldrel'
- os: ubuntu-20.04,   r: 'devel', 
- os: ubuntu-20.04,   r: 'release'
- os: ubuntu-20.04,   r: 'oldrel'

### Winbuilder
* Windows                 (win-builder), R Under development (unstable) (2023-06-29 r84618 ucrt)
* Windows                 (win-builder), 4.3.1 (2023-04-21 ucrt)
* Windows                 (win-builder), 4.2.3 (2023-03-15 ucrt)


### R-hub
* Linux (Debian, Fedora), MacOS, Windows

## R CMD check results

There were no ERRORs nor WARNINGs nor NOTEs.

## revdepcheck results

> revdepcheck::revdep_report_cran() ## update cran-comments with this output
## revdepcheck results

We checked 2 reverse dependencies (0 from CRAN + 2 from Bioconductor), comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 0 packages
