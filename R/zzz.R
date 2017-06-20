.onLoad <- function(libname, pkgname) {
  opts <- options()
  opts.quickPlot <- list(
    quickPlot.tolerance = .Machine$double.eps ^ 0.5
  )
  toset <- !(names(opts.quickPlot) %in% names(opts))
  if (any(toset)) options(opts.quickPlot[toset])
  
  invisible()
}
