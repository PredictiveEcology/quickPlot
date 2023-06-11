test_that("clickExtent works", {
  skip_if_not(interactive(), message = "clickExtent tests must be run manually.")

  skip_on_os("linux")
  testInit("terra")

  # make a SpatialPolygon
  coords1 <- structure(c(-123.98, -117.1, -80.2, -100, -123.98, 60.9, 67.73, 65.58, 51.79, 60.9),
                       .Dim = c(5L, 2L))
  shpEcozone <- terra::vect(coords1, type = "polygon")
  terra::crs(shpEcozone) <- "+init=epsg:4326 +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

  files <- dir(system.file("maps", package = "quickPlot"), full.names = TRUE, pattern = "tif")

  a <- terra::rast(files)
  dev()
  clearPlot()
  Plot(a)

  for (i in 1:5) {
    print("click on plots --> this is a clickExtent test ... 10 clicks")
    warns <- capture_warnings(clickExtent())
  }
  expect_true(1 == 1) # if get to here, then all good.
})
