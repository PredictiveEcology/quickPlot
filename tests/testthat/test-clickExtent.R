test_that("clickExtent works", {
  skip("This must be run manually")
  library(sp)
  library(raster)
  ow <- setwd(tempdir())

  # make a SpatialPolygon
  coords1 <- structure(c(-123.98, -117.1, -80.2, -100, -123.98, 60.9, 67.73, 65.58, 51.79, 60.9),
                       .Dim = c(5L, 2L))
  Sr1 <- Polygon(coords1)
  Srs1 <- Polygons(list(Sr1), "s1")
  shpEcozone <- SpatialPolygons(list(Srs1), 1L)
  crs(shpEcozone) <- "+init=epsg:4326 +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

  Plot(shpEcozone)

  cacheRepo <- file.path(dirname(tempdir()), "testClickExtent")
  dir.create(cacheRepo)
  theFile <- "Boreal-ecosystem-anthropogenic-disturbance-vector-data-2008-2010.zip"
  b <- download.file(destfile =  theFile,
                       url = "http://www.ec.gc.ca/data_donnees/STB-DGST/003/Boreal-ecosystem-anthropogenic-disturbance-vector-data-2008-2010.zip")
  unzip(zipfile = theFile, exdir = dirname(tempdir()))
  bb <- raster::shapefile(file.path(dirname(tempdir()), "EC_borealdisturbance_polygonal_2008_2010_FINAL_ALBERS.shp"))

  dev(); clearPlot();
  Plot(bb);
  for (i in 1:5) clickExtent()


})
