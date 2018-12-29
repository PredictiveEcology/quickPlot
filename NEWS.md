Known issues: https://github.com/PredictiveEcology/quickPlot/issues

version 0.1.6.9000
=============

* A memory leak was found; environments where the original data were stored were attached to the `.quickPlotEnv` when `Plot` was called; this was done to minimize copying. However, this has the effect of bringing with it the whole environment, not just the object, so it was larger than necessary. This environment was then not properly removed/deleted when new plots were overlaid, so the memory use would keep escalating. This has been solved by explicitly making a new environment for each plot, setting this environment as the data location, assigning the data to this location. While superficially this would appear to be like making a copy of the data, it is just making a pointer, until the original object changes. At that point, the use likely wants the `Plot` to update anyway. RAM use is now under control, and does not escalate. 


version 0.1.6
=============

* bugfixes

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
