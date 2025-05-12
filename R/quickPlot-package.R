#' The `quickPlot` package
#'
#' A high-level plotting system, built using 'grid' graphics, that is optimized
#' for speed and modularity. This has great utility for quick visualizations
#' when testing code, with the key benefit that visualizations are updated
#' independently of one another.
#'
"_PACKAGE"

## data documentation

#' Dummy maps included with `quickPlot`
#'
#' All maps included here are randomly generated maps created using
#' `SpaDES.tools::gaussMap()`.
#' These are located within the `maps` folder of the package, and are used
#' in the vignettes.
#' Use `system.file("maps", package = "quickPlot")` to locate the \file{maps/}
#' directory on your system.
#'
#' @details
#' \itemize{
#'   \item `DEM.tif`: converted to a a small number of discrete levels
#'         (in 100m hypothetical units).
#'   \item `habitatQuality.tif`: made to look like a continuous habitat surface,
#'          rescaled to 0 to 1.
#'   \item `forestAge.tif`: rescaled to possible forest ages in a boreal forest setting.
#'   \item `forestCover.tif`: rescaled to possible forest cover in a boreal forest setting.
#'   \item `percentPine.tif`: rescaled to percentages.
#' }
#'
#' @docType data
#' @keywords maps
#' @name sample-maps
#' @rdname sample-maps
#' @format raster
NULL

## package imports (see \url{https://r-pkgs.had.co.nz/namespace.html#imports})

#' @import graphics
#' @import grid
#' @import methods
NULL

## usethis namespace: start
#' @importFrom data.table data.table
#' @importFrom data.table :=
#' @importFrom data.table .SD
#' @importFrom data.table .BY
#' @importFrom data.table .N
#' @importFrom data.table .I
#' @importFrom data.table .GRP
#' @importFrom data.table .NGRP
#' @importFrom data.table .EACHI
## usethis namespace: end
NULL
