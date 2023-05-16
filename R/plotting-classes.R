### Allow histogram S3 class to be used with Plot, an S4 function
setOldClass("histogram")
selectMethod("show", "histogram")

### Allow gpar S3 class to be used with Plot, an S4 function
setOldClass("gpar")

setAs(from = "list", to = "gpar", function(from) {
  if (length(from[[1]]) > 0) {
    gp1 <- gpar(from[[1]][[1]])
    if (length(from[[1]]) > 1) {
      for (i in 2:length(from[[1]])) {
        gp1 <- gpar(sapply(gp1, function(x) x), from[[1]][[i]])
      }
    }
    names(gp1) <- names(from[[1]])
    gp1
  } else {
    gpar()
  }
})

################################################################################
# The `griddedClasses` class
#
# This class is the union of several spatial objects from \pkg{raster} and
# \pkg{sp} packages.
#
# Members:
#
# \itemize{
#   \item `RasterLayer`, `RasterLayerSparse`, `RasterStack`;
# }
#
# Notably missing is `RasterBrick`, for now.
#
# @seealso [quickPlotClasses()]
#
# @aliases griddedClasses
# @importClassesFrom raster RasterLayer
# @importClassesFrom raster RasterLayerSparse
# @importClassesFrom raster RasterStack
# @name griddedClasses-class
# @rdname griddedClasses-class
# @author Eliot McIntire
# @exportClass griddedClasses
# setClassUnion(name = "griddedClasses",
#               members = c("RasterLayer", "RasterLayerSparse", "RasterStack", "SpatRaster")
# )

################################################################################
# The `spatialObjects` class
#
# This class is the union of several spatial objects from \pkg{raster} and \pkg{sp} packages.
#
# Members:
#
# \itemize{
#   \item `RasterLayer`, `RasterLayerSparse`, `RasterStack`;
#   \item `SpatialLines`, `SpatialLinesDataFrame`;
#   \item `SpatialPixels`, `SpatialPixelsDataFrame`;
#   \item `SpatialPoints`, `SpatialPointsDataFrame`;
#   \item `SpatialPolygons`, `SpatialPolygonsDataFrame`.
# }
#
# Notably missing is `RasterBrick`, for now.
#
# @seealso [quickPlotClasses()]
#
# @aliases spatialObjects
# @importClassesFrom sp SpatialLines
# @importClassesFrom sp SpatialLinesDataFrame
# @importClassesFrom sp SpatialPixels
# @importClassesFrom sp SpatialPixelsDataFrame
# @importClassesFrom sp SpatialPoints
# @importClassesFrom sp SpatialPointsDataFrame
# @importClassesFrom sp SpatialPolygons
# @importClassesFrom sp SpatialPolygonsDataFrame
# @importClassesFrom terra SpatRaster
# @importClassesFrom terra SpatVector
# @name spatialObjects-class
# @rdname spatialObjects-class
# @author Eliot McIntire
# @exportClass spatialObjects
# setClassUnion(name = "spatialObjects",
#               members = c("griddedClasses",
#                           "SpatialLines", "SpatialLinesDataFrame",
#                           "SpatialPixels", "SpatialPixelsDataFrame",
#                           "SpatialPoints", "SpatialPointsDataFrame",
#                           "SpatialPolygons", "SpatialPolygonsDataFrame",
#                           "SpatRaster", "SpatVector"
#                           )
# )

################################################################################
# The `.quickPlotObjects` class
#
# This class contains the union of `spatialObjects` and several other plot-type objects.
# Currently, this includes `SpatialPoints*`, `SpatialPolygons*`,
# `SpatialLines*`, `RasterLayer`, `RasterStack`, and `ggplot` objects.
# These are the object classes that the [Plot()] function can handle.
#
# @aliases .quickPlotObjects
# @author Eliot McIntire
# @name .quickPlotObjects-class
# @rdname quickPlotObjects-class
# @seealso [quickPlotClasses()]
#
# setClassUnion(name = ".quickPlotObjects",
#               members = c("spatialObjects", "gg"))

################################################################################
#' The `.quickPlotGrob` class
#'
#' This class contains the plotting `.quickPlotGrob` information.
#'
#' These `gp*` parameters will specify plot parameters that are available
#' with `gpar()`. `gp` will adjust plot parameters, `gpText` will
#' adjust title and legend text, `gpAxis` will adjust the axes.
#' `size` adjusts point size in a `SpatialPoints` object.
#' These will persist with the original `Plot` call for each individual object.
#' Multiple entries can be used, but they must be named list elements
#' and they must match the `...` items to plot.
#' This is true for a `RasterStack` also, i.e., the list of named elements
#' must be the same length as the number of layers being plotted.
#' The naming convention used is: `RasterStackName$layerName`, i.e, `landscape$DEM`.
#'
#' @seealso [quickPlotClasses()]
#'
#' @slot plotName  character. Name of the plot frame, which is by default a concatenation
#' of the `objName` and `layerName`
#'
#' @slot objName  character. Name of object represented by this `.quickPlotGrob`
#'
#' @slot envir environment. The environment in which to find the `objName`
#'
#' @slot layerName character. Name of the layer represented by this `.quickPlotGrob`.
#' Primarily used for `RasterStack`s.
#'
#' @slot objClass character. Class of the object represented by this `.quickPlotGrob`.
#'
#' @slot isSpatialObjects logical. TRUE if the object is one of the SpaDES recognized
#' `spatialObject` classes.
#'
#' @slot plotArgs list. Any parameters needed for plotting, set by `Plot` call.
#'
#' @aliases .quickPlotGrob
#' @keywords internal
#' @name .quickPlotGrob-class
#' @rdname quickPlotGrob-class
#' @author Eliot McIntire
#'
setClass(".quickPlotGrob",
         slots = list(plotName = "character", objName = "character",
                      envir = "environment", layerName = "character",
                      objClass = "character", isSpatialObjects = "logical",
                      plotArgs = "list"),
         prototype = list(plotName = NA_character_, objName = NA_character_,
                          layerName = NA_character_, objClass = NA_character_,
                          isSpatialObjects = NA, plotArgs = as.list(NULL)),
         validity = function(object) {
           # check for valid extents
           if (any(is.character(object@objName))) {
             stop("must supply an object name")
           }
})

###########################################################################
#' The `.arrangement` class
#'
#' This class contains the plotting arrangement information.
#'
#' These `gp*` parameters will specify plot parameters that are
#' available with `gpar()`. `gp` will adjust plot parameters,
#' `gpText` will adjust title and legend text, `gpAxis` will
#' adjust the axes. `size` adjusts point size in a `SpatialPoints` object.
#' These will persist with the original `Plot` call for each individual object.
#' Multiple entries can be used, but they must be named list elements
#' and they must match the `...` items to plot.
#' This is true for a `RasterStack` also, i.e., the list of named elements
#' must be the same length as the number of layers being plotted.
#' The naming convention used is: `RasterStackName$layerName`, i.e, `landscape$DEM`.
#'
#' @seealso [quickPlotClasses()]
#'
#' @slot rows    numeric. Number of rows in the arrangement.
#'
#' @slot columns numeric. Number of columns in the arrangement.
#'
#' @slot actual.ratio numeric. Ratio of columns to rows
#'
#' @slot ds.dimensionRatio numeric. Ratio of the device size to the ratio of the extents.
#'
#' @slot ds  numeric of length 2. The dimensions of the plotting window in inches.
#'
#' @slot objects  list of length number of spatial objects. Each list has a character vector
#' of the layer names in each of those.
#'
#' @slot isRaster  logical vector, indicating whether each object is a `Raster*` object.
#'
#' @slot names  character vector, indicating the names of the layers in the plot.
#'
#' @slot extents list of class `Extent` objects. These are needed to calculate the
#' `ds.dimensionRatio`, which is used to scale the `Spatial*` objects correctly.
#'
#' @slot isSpatialObjects logical indicating whether the object(s) are `spatialObjects` or not.
#'
#' @slot layout list of length 2, with width and height measurements for layout.
#'
#' @slot gp a `gpar` object or list of named `gpar` objects. These names must
#' match the names of the `...` objects. Default is `NULL.` See details.
#'
#' @slot gpText a `gpar` object or a list of named `gpar` objects. These names must
#' match the names of the `...` objects. Default is `NULL.` See details.
#'
#' @slot gpAxis a `gpar` object or a list of named `gpar` objects. These names must
#' match the names of the `...` objects. Default is `NULL.` See details.
#'
#' @slot size a numeric or a named list of numerics, used for `SpatialPoints` plots.
#' Default is 5. See details.
#'
#' @aliases .arrangement
#' @keywords internal
#' @name .arrangement-class
#' @rdname arrangement-class
#' @author Eliot McIntire
#'
setClass(".arrangement",
         slots = list(rows = "numeric", columns = "numeric",
                    actual.ratio = "numeric", ds.dimensionRatio = "numeric",
                    ds = "numeric", objects = "list", isRaster = "logical", names = "character",
                    extents = "list", isSpatialObjects = "logical", layout = "list",
                    gp = "list", gpText = "list", gpAxis = "list", size = "list"),
         prototype = list(rows = 1, columns = 1,
                        actual.ratio = 1, ds.dimensionRatio = 1,
                        ds = c(7, 7), objects = as.list(NULL), isRaster = NA,
                        names = as.character(NULL),
                        extents = as.list(NULL), isSpatialObjects = NA, layout = as.list(NULL),
                        gp = as.list(NULL), gpText = as.list(NULL),
                        gpAxis = as.list(NULL), size = as.list(NULL)),
         validity = function(object) {
           # check for valid extents
           if (any(is.na(object@extents))) {
             stop("must supply a list of extents")
           }
})

###########################################################################
#' The `.quickPlot` class
#'
#' This class contains all necessary information to build a Plot on a device,
#' except the actual data. Thus, this class differs notably from the grid package,
#' which keeps a copy of all data *and* information in a hidden location for further
#' access for rebuilding, erasing etc. This difference allows the Plot function to
#' be much faster than using the grid methodology directly. The cost to this speed
#' gain is that the objects *must* be available, by name, in the `.GlobalEnv`.
#'
#' This class contains two slots, one for the overall arrangement of the plots within
#' the device window, and the second for all the [.quickPlotGrob()] objects within
#' that device window. These [.quickPlotGrob()] objects are the individual
#' "smallest" plot unit.
#'
#' These `gp*` parameters will specify plot parameters that are
#' available with `gpar()`. `gp` will adjust plot parameters,
#' `gpText` will adjust title and legend text, `gpAxis` will
#' adjust the axes. `size` adjusts point size in a
#' `SpatialPoints` object. These will persist with the
#' original `Plot` call for each individual object. Multiple
#' entries can be used, but they must be named list elements
#' and they must match the `...` items to plot. This is true
#' for a `RasterStack` also, i.e., the list of named elements
#' must be the same length as the number of layers being
#' plotted. The naming convention used is: `RasterStackName$layerName`,
#' i.e, `landscape$DEM`.
#'
#' @seealso [quickPlotClasses()]
#'
#' @slot arr  An .arrangement object
#'
#' @slot quickPlotGrobList list. A list of lists of `.quickPlotGrob` objects
#'
#' @aliases .quickPlot
#' @keywords internal
#' @name .quickPlot-class
#' @rdname quickPlot-class
#' @author Eliot McIntire
#'
setClass(".quickPlot",
         slots = list(arr = ".arrangement", quickPlotGrobList = "list"),
         prototype = list(arr = new(".arrangement"), quickPlotGrobList = as.list(NULL)),
         validity = function(object) {
           # check for valid extents
           if (any(inherits(object@arr, ".arrangement"))) {
             stop("must supply an arrangement")
           }
})

################################################################################
# The `.quickPlottables` class
#
# This class is the union of all `.quickPlotObjects` (e.g., `RasterLayer*`,
# `SpatialPoints*`, `SpatialPolygons*`, `ggplot`, `hist`, etc.)
# and [.quickPlot()] class objects.
# This allows replotting of a [.quickPlot()] object.
#
# @seealso [quickPlotClasses()]
#
# @aliases .quickPlottables
# @keywords internal
# @name .quickPlottables-class
# @rdname quickPlottables-class
# @author Eliot McIntire
# @exportClass .quickPlottables
# setClassUnion(name = ".quickPlottables",
#               members = c(".quickPlotObjects", ".quickPlot"))
