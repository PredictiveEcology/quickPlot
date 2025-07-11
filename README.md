# quickPlot

<!-- badges: start -->
[![R build status](https://github.com/PredictiveEcology/quickPlot/workflows/R-CMD-check/badge.svg)](https://github.com/PredictiveEcology/quickPlot/actions)
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/quickPlot)](https://cran.r-project.org/package=quickPlot)
[![Downloads](https://cranlogs.r-pkg.org/badges/grand-total/quickPlot)](https://cran.r-project.org/package=quickPlot)
[![codecov](https://codecov.io/gh/PredictiveEcology/quickPlot/branch/master/graph/badge.svg)](https://app.codecov.io/gh/PredictiveEcology/quickPlot)
<!-- badges: end -->

Built on top of `grid`, this package aims at making a high-level plotting system that is optimized for speed and modularity.
This has great utility for quick visualizations when testing code, with the key benefit that visualizations are updated independently of one another.

## Installation

The suggested package `fastshp` can be installed with:

```{r}
install.packages("fastshp", repos = "https://rforge.net", type = "source") or for binary:
install.packages('fastshp', repos = 'https://PredictiveEcology.r-universe.dev')
```

Building packages from source requires the appropriate development libraries for your operating system (*e.g.*, Windows users should install [Rtools](https://cran.r-project.org/bin/windows/Rtools/)).

### Current stable release

**Install from CRAN:**

```r
install.packages("quickPlot")
```

**Install from GitHub:**
    
```r
# install.packages("remotes")
remotes::install_github("PredictiveEcology/quickPlot", dependencies = TRUE)
```

### Development version (unstable)

**Install from GitHub:**

```r
# install.packages("remotes")
remotes::install_github("PredictiveEcology/quickPlot@development", dependencies = TRUE)
```
