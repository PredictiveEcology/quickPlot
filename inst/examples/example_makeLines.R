library(terra)
# Make 2 objects
caribou1 <- terra::vect(cbind(x = stats::runif(10, -50, 50),
                              y = stats::runif(10, -50, 50)))
caribou2 <- terra::vect(cbind(x = stats::runif(10, -50, 50),
                              y = stats::runif(10, -50, 50)))

caribouTraj <- makeLines(caribou1, caribou2)

if (interactive())
  if (requireNamespace("quickPlot", quietly = TRUE)) {
    quickPlot::Plot(caribouTraj, length = 0.1) # shows arrows
  } else {
    terra::plot(caribouTraj)
  }

# or  to a previous Plot
files <- dir(system.file("maps", package = "quickPlot"), full.names = TRUE, pattern = "tif")
maps <- lapply(files, terra::rast)
names(maps) <- lapply(maps, names)

caribouTraj <- makeLines(caribou1, caribou2)

if (interactive()) {
  clearPlot()
  quickPlot::Plot(maps$DEM)
  quickPlot::Plot(caribouTraj, addTo = "maps$DEM", length = 0.1)
}

