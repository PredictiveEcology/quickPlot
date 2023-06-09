.onLoad <- function(libname, pkgname) {
  opts <- options()
  opts.quickPlot <- list( # nolint
    quickPlot.tolerance = .Machine$double.eps ^ 0.5,
    quickPlot.verbose = getOption("reproducible.verbose", 1L),
    quickPlot.maxNumPolygons = 1e4
  )
  toset <- !(names(opts.quickPlot) %in% names(opts))
  if (any(toset)) options(opts.quickPlot[toset])

  invisible()
}

