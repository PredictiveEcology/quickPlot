.onLoad <- function(libname, pkgname) {
  opts <- options()
  opts.quickPlot <- list( # nolint
    quickPlot.tolerance = .Machine$double.eps ^ 0.5,
    quickPlot.maxNumPolygons = 1e4
  )
  mess <- character()
  notInstalled <- character()
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    notInstalled <- c(notInstalled, "ggplot2")
  }
  if (!requireNamespace("raster", quietly = TRUE)) {
    notInstalled <- c(notInstalled, "raster")
  }
  if (!requireNamespace("sp", quietly = TRUE)) {
    notInstalled <- c(notInstalled, "sp")
  }

  if (length(notInstalled)) {
    mess <- paste("To Plot", paste(notInstalled, collapse = ", "), "objects:",
          paste0("install.packages(c('", paste0(notInstalled, collapse = "', '"), "'))"))
    packageStartupMessage(mess)
  }

  toset <- !(names(opts.quickPlot) %in% names(opts))
  if (any(toset)) options(opts.quickPlot[toset])

  backports::import(pkgname, obj = "isFALSE")

  invisible()
}

