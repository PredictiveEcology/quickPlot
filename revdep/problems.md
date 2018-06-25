# Setup

## Platform

|setting  |value                        |
|:--------|:----------------------------|
|version  |R version 3.4.4 (2018-03-15) |
|system   |x86_64, linux-gnu            |
|ui       |RStudio (1.1.453)            |
|language |en_CA:en                     |
|collate  |en_CA.UTF-8                  |
|tz       |America/Edmonton             |
|date     |2018-06-22                   |

## Packages

|package   |*  |version |date       |source                                       |
|:---------|:--|:-------|:----------|:--------------------------------------------|
|fastshp   |   |0.1-2   |2018-06-22 |Github (s-u/fastshp@3e0eb83)                 |
|quickPlot |*  |0.1.4   |2018-06-22 |Github (PredictiveEcology/quickPlot@3107660) |

# Check results

2 packages with problems

|package     |version | errors| warnings| notes|
|:-----------|:-------|------:|--------:|-----:|
|SpaDES.core |0.1.1   |      1|        0|     0|
|SpaDES      |2.0.2   |      0|        1|     0|

## SpaDES.core (0.1.1)
Maintainer: Alex M Chubaty <alexander.chubaty@canada.ca>  
Bug reports: https://github.com/PredictiveEcology/SpaDES.core/issues

1 error  | 0 warnings | 0 notes

```
checking whether package ‘SpaDES.core’ can be installed ... ERROR
Installation failed.
See ‘/home/achubaty/Documents/GitHub/SpaDES/quickPlot/revdep/checks/SpaDES.core.Rcheck/00install.out’ for details.
```

## SpaDES (2.0.2)
Maintainer: Alex M Chubaty <alex.chubaty@gmail.com>  
Bug reports: https://github.com/PredictiveEcology/SpaDES/issues

0 errors | 1 warning  | 0 notes

```
checking whether package ‘SpaDES’ can be installed ... WARNING
Found the following significant warnings:
  Warning: no DISPLAY variable so Tk is not available
  Warning: replacing previous import ‘SpaDES.tools::checkGDALVersion’ by ‘reproducible::checkGDALVersion’ when loading ‘SpaDES’
  Warning: replacing previous import ‘SpaDES.tools::getGDALVersion’ by ‘reproducible::getGDALVersion’ when loading ‘SpaDES’
  Warning: replacing previous import ‘SpaDES.tools::fastMask’ by ‘reproducible::fastMask’ when loading ‘SpaDES’
See ‘/home/achubaty/Documents/GitHub/SpaDES/quickPlot/revdep/checks/SpaDES.Rcheck/00install.out’ for details.
```

