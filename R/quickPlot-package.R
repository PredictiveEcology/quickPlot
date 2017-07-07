################################################################################
# data documentation
#

#' Dummy maps included with \code{quickPlot}
#'
#' All maps included here are randomly generated maps created using
#' \code{SpaDES.tools::gaussMap()}.
#' These are located within the \code{maps} folder of the package, and are used
#' in the vignettes.
#' Use \code{system.file("maps", package = "quickPlot")} to locate the \file{maps/}
#' directory on your system.
#'
#' @details
#' \itemize{
#'   \item \code{DEM.tif}: converted to a a small number of discrete levels
#'         (in 100m hypothetical units).
#'   \item \code{habitatQuality.tif}: made to look like a continuous habitat surface,
#'          rescaled to 0 to 1.
#'   \item \code{forestAge.tif}: rescaled to possible forest ages in a boreal forest setting.
#'   \item \code{forestCover.tif}: rescaled to possible forest cover in a boreal forest setting.
#'   \item \code{percentPine.tif}: rescaled to percentages.
#' }
#'
#' @docType data
#' @keywords maps
#' @name sample-maps
#' @rdname sample-maps
#' @format raster
NULL

################################################################################
# package imports
#

#' @import graphics
#' @import grid
#' @import igraph
#' @import methods
NULL
