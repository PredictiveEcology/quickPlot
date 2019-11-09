Known issues: https://github.com/PredictiveEcology/quickPlot/issues

version 0.1.6
=============
* bugfixes including `dev` on linux alikes

version 0.1.5
=============
* new function `isRstudioServer`

version 0.1.4
=============
* much faster for `SpatialPolygons`
* `clickExtent` now more interactive with new examples
* remove dependency on `reproducible`
* `SpatialPolygons` get a fill colour by default if not passed in by user
* fix bug related to plotting with NAs (#14, @CeresBarros)
* bug fixes
* new options("quickPlot.maxNumPolygons"), used for plotting of large `SpatialPolygons` objects

version 0.1.3
=============
* new function, `thin`, which is similar to rgeos::gSimplify, but uses fastshp::thin, and can get much faster speeds, 
  used internally for plotting, but also available as a function to user
* minor bug fix for when quickPlot::Plot is called inside a custom function called "Plot"
* reworked internal approach to determine object name and object environments

version 0.1.2
=============
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
