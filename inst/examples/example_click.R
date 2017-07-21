if (interactive()) {
  library(igraph)
  library(raster)

  files <- system.file("maps", package = "quickPlot") %>%
    dir(., full.names = TRUE, pattern = "tif")
  maps <- lapply(files, function(x) raster(x))
  names(maps) <- sapply(basename(files), function(x) {
    strsplit(x, split = "\\.")[[1]][1]
  })
  landscape <- stack(maps$DEM, maps$forestCover, maps$habitatQuality)

  clearPlot()
  Plot(landscape)
  clickValues(3) # click at three locations on the Plot device

  clearPlot()
  Plot(landscape)
  e <- clickExtent() # click at two locations on the Plot device
  print(e)
}
