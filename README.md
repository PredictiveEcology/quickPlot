# quickPlot

[![Build Status](https://travis-ci.org/PredictiveEcology/quickPlot.svg?branch=master)](https://travis-ci.org/PredictiveEcology/quickPlot)
[![Appveyor Build status](https://ci.appveyor.com/api/projects/status/2fxqhgk6miv2fytd/branch/master?svg=true)](https://ci.appveyor.com/project/achubaty/quickPlot/branch/master)
[![Coverage Status](https://coveralls.io/repos/github/PredictiveEcology/quickPlot/badge.svg?branch=master)](https://coveralls.io/github/PredictiveEcology/quickPlot?branch=master)
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/quickPlot)](https://cran.r-project.org/package=quickPlot)
[![Downloads](https://cranlogs.r-pkg.org/badges/grand-total/quickPlot)](https://cran.r-project.org/package=quickPlot)

Built on top of `grid`, this package aims at making a high-level plotting system that is optimized for speed and modularity.
This has great utility for quick visualizations when testing code, with the key benefit that visualizations are updated independently of one another.

## Installation

The suggested package `fastshp` can be installed with:

```{r}
install.packages("fastshp", repos = "https://rforge.net", type = "source")
```

Building packages from source requires the appropriate development libraries for your operating system (*e.g.*, Windows users should install [Rtools](https://cran.r-project.org/bin/windows/Rtools/)).

### Current stable release

**Install from CRAN:**

```r
install.packages("quickPlot")
```

**Install from GitHub:**
    
```r
#install.packages("devtools")
library("devtools")
install_github("PredictiveEcology/quickPlot", dependencies = TRUE) # stable
```

### Development version (unstable)

**Install from GitHub:**

```r
#install.packages("devtools")
library("devtools")
install_github("PredictiveEcology/quickPlot", ref = "development", dependencies = TRUE) # unstable
```
