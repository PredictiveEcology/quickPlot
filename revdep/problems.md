# Setup

## Platform

|setting  |value                        |
|:--------|:----------------------------|
|version  |R version 3.5.1 (2018-07-02) |
|system   |x86_64, darwin15.6.0         |
|ui       |RStudio (1.1.419)            |
|language |(EN)                         |
|collate  |en_CA.UTF-8                  |
|tz       |America/Edmonton             |
|date     |2018-11-06                   |

## Packages

|package   |*  |version |date       |source                                       |
|:---------|:--|:-------|:----------|:--------------------------------------------|
|quickPlot |*  |0.1.6   |2018-11-07 |Github (PredictiveEcology/quickPlot@5c08464) |

# Check results

2 packages with problems

|package      |version | errors| warnings| notes|
|:------------|:-------|------:|--------:|-----:|
|reproducible |0.2.3   |      1|        0|     0|
|SpaDES.core  |0.2.2   |      1|        1|     0|

## reproducible (0.2.3)
Maintainer: Eliot J B McIntire <eliot.mcintire@canada.ca>  
Bug reports: https://github.com/PredictiveEcology/reproducible/issues

1 error  | 0 warnings | 0 notes

```
checking tests ... ERROR
  Running ‘test-all.R’ [24s/21s]
Running the tests in ‘tests/test-all.R’ failed.
Last 13 lines of output:
  1      readRDS          Hashing 0.0007071495  secs
  2      readRDS  Running readRDS 0.0004718304  secs
  3      readRDS   Saving to repo 0.0972189903  secs
  4      readRDS Whole Cache call 0.1204190254  secs
  [31m──[39m [31m1. Failure: fastMask produces correct results (@test-gis[39m
  `newStack1` not equal to `newStack2`.
  Attributes: < Component "layers": Component 1: Attributes: < Component 3: Attributes: < Component 13: 'is.NA' value mismatch: 25 in current 27 in target > > >
  Attributes: < Component "layers": Component 2: Attributes: < Component 3: Attributes: < Component 13: 'is.NA' value mismatch: 25 in current 27 in target > > >
  
  ══ testthat results  ══════════════════════════════════════
  OK: 169 SKIPPED: 12 FAILED: 1
  1. Failure: fastMask produces correct results (@test-gis.R#22) 
  
  Error: testthat unit tests failed
  Execution halted
```

## SpaDES.core (0.2.2)
Maintainer: Alex M Chubaty <alex.chubaty@gmail.com>  
Bug reports: https://github.com/PredictiveEcology/SpaDES.core/issues

1 error  | 1 warning  | 0 notes

```
checking tests ... ERROR
  Running ‘test-all.R’ [101s/104s]
Running the tests in ‘tests/test-all.R’ failed.
Last 13 lines of output:
  [39m[34m  Using cached copy of .inputObjects event in test module. Adding to memoised copy.
  [39m[34m  Using memoised copy of .inputObjects event in test module
  [39m[34m  Using cached copy of .inputObjects event in test module. Adding to memoised copy.
  [39m[31m──[39m [31m1. Failure: test objSize (@test-cache.R#333) [39m [31m──────────[39m
  length(os) == 4 isn't true.
  
  [34m  Using cached copy of .inputObjects event in child6 module. Adding to memoised copy.
  [39m══ testthat results  ══════════════════════════════════════
  OK: 310 SKIPPED: 35 FAILED: 1
  1. Failure: test objSize (@test-cache.R#333) 
  
  Error: testthat unit tests failed
  In addition: Warning message:
  In fun(libname, pkgname) : couldn't connect to display ""
  Execution halted

checking Rd cross-references ... WARNING
Missing link or links in documentation object 'moduleCoverage.Rd':
  ‘[covr]{shine}’

See section 'Cross-references' in the 'Writing R Extensions' manual.

```

