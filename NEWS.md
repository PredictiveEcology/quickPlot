Known issues: https://github.com/PredictiveEcology/quickPlot/issues

version 0.1.2
==================
* Small tweaks that allow methods to be built by other packages (*e.g.*, [PredictiveEcology/NetLogoR](https://github.com/PredictiveEcology/NetLogoR))
* new functions
    - `sp2sl` to convert pairs of `SpatialPoints` objects to single `SpatialLines` objects, principally for plotting arrows as connected points.
* bugfix `Spatial*` objects that had a color column, which was not working correctly.

version 0.1.1
=============

* add more examples to non-internal functions and *do* run them (per CRAN)
* added checks in the tests to ensure all exported functions have examples

version 0.1.0
=============

* A new package, which takes all primary plotting out of the `SpaDES` package.
