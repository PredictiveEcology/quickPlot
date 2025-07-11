# quickPlot (development version)

* drop support for R <= 4.2;
* remove defunct dependency `fastshp` (#35);
* updates and fixes  for forthcoming `ggplot2` release (#37; #38);
* move `ggplot2` from Suggests to Imports;

# quickPlot 1.0.3

* bugfixes for `SpatRaster` that use lowercase `id` for their levels
* bugfix `nrowLegText` missing

# quickPlot 1.0.2

* Fixed all known errors due to changes in package dependencies (e.g., `raster`, `terra`, `sf`, `sp`)
* completed migration away from `rgdal`, `maptools`, and `rgeos`
* Allow extending `.plotGrob` without `.quickPlottables` class, using method dispatch
* improved wiping of a plotting space when `new = TRUE`
* A function `extents` which tries to provide a consistent output from `st::st_bbox`, `terra::ext` and `raster::extent`
* Bugfixes for revdeps, especially `SpaDES.core`

# quickPlot 1.0.1

* Add support for `terra` and `sf` classes. 
* removed several S4 methods; converted several S4 methods to S3 methods
* multiple bugfixes 

# quickPlot 0.1.8

* drop support for R version 3.6 (versions >= 4.0 are currently supported)

# quickPlot 0.1.7

* bugfixes including `dev` on windows
* bugfix for colour of filling polygons. Now can use `cols` or `gp = gpar(fill = xx)`.  Distinguishes between `Polygons` and `Polygon`

# quickPlot 0.1.6

* bugfixes including `dev` on Linux-alikes

# quickPlot 0.1.5

* new function `isRstudioServer`

# quickPlot 0.1.4

* much faster for `SpatialPolygons`
* `clickExtent` now more interactive with new examples
* remove dependency on `reproducible`
* `SpatialPolygons` get a fill colour by default if not passed in by user
* fix bug related to plotting with NAs (#14, @CeresBarros)
* bug fixes
* new `options("quickPlot.maxNumPolygons")`, used for plotting of large `SpatialPolygons` objects

# quickPlot 0.1.3

* new function, `thin`, which is similar to `rgeos::gSimplify`, but uses `fastshp::thin`, and can get much faster speeds, 
  used internally for plotting, but also available as a function to user
* minor bug fix for when `quickPlot::Plot` is called inside a custom function called "Plot"
* reworked internal approach to determine object name and object environments

# quickPlot 0.1.2

* Small tweaks that allow methods to be built by other packages (*e.g.*, [PredictiveEcology/NetLogoR](https://github.com/PredictiveEcology/NetLogoR))
* new functions
    - `sp2sl` to convert pairs of `SpatialPoints` objects to single `SpatialLines` objects, principally for plotting arrows as connected points.
* bugfix `Spatial*` objects that had a colour column, which was not working correctly.

# quickPlot 0.1.1


* add more examples to non-internal functions and *do* run them (per CRAN)
* added checks in the tests to ensure all exported functions have examples

# quickPlot 0.1.0


* A new package, which takes all primary plotting out of the `SpaDES` package.
