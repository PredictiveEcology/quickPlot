# Setup

## Platform

|setting  |value                        |
|:--------|:----------------------------|
|version  |R version 3.5.1 (2018-07-02) |
|system   |x86_64, mingw32              |
|ui       |RStudio (1.1.383)            |
|language |(EN)                         |
|collate  |English_Canada.1252          |
|tz       |America/Los_Angeles          |
|date     |2018-10-01                   |

## Packages

|package      |*  |version |date       |source                                      |
|:------------|:--|:-------|:----------|:-------------------------------------------|
|covr         |   |3.2.0   |2018-09-21 |CRAN (R 3.5.1)                              |
|data.table   |   |1.11.6  |2018-09-19 |CRAN (R 3.5.1)                              |
|fpCompare    |   |0.2.2   |2018-06-12 |CRAN (R 3.5.1)                              |
|ggplot2      |   |3.0.0   |2018-07-03 |CRAN (R 3.5.1)                              |
|gridBase     |   |0.4-7   |2014-02-24 |CRAN (R 3.5.1)                              |
|hunspell     |   |2.9     |2017-12-16 |CRAN (R 3.5.1)                              |
|igraph       |   |1.2.2   |2018-07-27 |CRAN (R 3.5.1)                              |
|knitr        |   |1.20    |2018-02-20 |CRAN (R 3.5.1)                              |
|quickPlot    |   |0.1.5   |2018-10-01 |local (PredictiveEcology/quickPlot@62ed514) |
|raster       |   |2.6-7   |2017-11-13 |CRAN (R 3.5.1)                              |
|RColorBrewer |   |1.1-2   |2014-12-07 |CRAN (R 3.5.0)                              |
|rgdal        |   |1.3-4   |2018-08-03 |CRAN (R 3.5.1)                              |
|rgeos        |   |0.3-28  |2018-06-08 |CRAN (R 3.5.1)                              |
|rmarkdown    |   |1.10    |2018-06-11 |CRAN (R 3.5.1)                              |
|sp           |   |1.3-1   |2018-06-05 |CRAN (R 3.5.1)                              |
|testthat     |   |2.0.0   |2017-12-13 |CRAN (R 3.5.1)                              |

# Check results

2 packages with problems

|package     |version | errors| warnings| notes|
|:-----------|:-------|------:|--------:|-----:|
|SpaDES.core |0.2.2   |      0|        1|     0|
|SpaDES      |2.0.2   |      0|        1|     0|

## SpaDES.core (0.2.2)
Maintainer: Alex M Chubaty <alex.chubaty@gmail.com>  
Bug reports: https://github.com/PredictiveEcology/SpaDES.core/issues

0 errors | 1 warning  | 0 notes

```
checking Rd cross-references ... WARNING
Missing link or links in documentation object 'moduleCoverage.Rd':
  '[covr]{shine}'

See section 'Cross-references' in the 'Writing R Extensions' manual.

```

## SpaDES (2.0.2)
Maintainer: Alex M Chubaty <alex.chubaty@gmail.com>  
Bug reports: https://github.com/PredictiveEcology/SpaDES/issues

0 errors | 1 warning  | 0 notes

```
checking re-building of vignette outputs ... WARNING
Error in re-building vignettes:
  ...
loading quickPlot        0.1.5
loading SpaDES.core      0.2.2
loading SpaDES.tools     0.3.0
loading SpaDES.addins    0.1.1

Default paths for SpaDES directories set to:
  cachePath:  D:\temp\RtmpoRv0Fm/SpaDES/cache
... 8 lines ...
randomLandscapes: defineParameter: '.useCache' is not of specified type 'logical'.
randomLandscapes: module code appears clean
C:/Temp/RtmpIHDcor/RLIBS_20847b6b3a6e/SpaDES.core/sampleModules/fireSpread/fireSpread.R
fireSpread: module code: landscape, testStats are declared in inputObjects, but no default(s) are provided in .inputObjects
fireSpread: inputObjects: DEM, Fires are used from sim inside doEvent.fireSpread, but are not declared in inputObjects
###### Module Code Checking ########
Quitting from lines 66-68 (iii-cache.Rmd) 
Error: processing vignette 'iii-cache.Rmd' failed with diagnostics:
Could not connect to database:
unable to open database file
Execution halted
```

