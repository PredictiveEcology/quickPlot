.onLoad <- function(libname, pkgname) {
  opts <- options()
  opts.quickPlot <- list( # nolint
    quickPlot.tolerance = .Machine$double.eps ^ 0.5,
    quickPlot.maxNumPolygons = 1e4
  )
  toset <- !(names(opts.quickPlot) %in% names(opts))
  if (any(toset)) options(opts.quickPlot[toset])

  backports::import(pkgname, obj = "isFALSE")

  invisible()
}

#' @import rgdal
NULL
