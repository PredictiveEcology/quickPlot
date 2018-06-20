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


  # crop_raster_gdal=function(srcfile, dstfile,shapefile) {
  #   require(gdalUtils)
  #   gdalwarp(srcfile=srcfile, dstfile=dstfile,cutline=shapefile, dstalpha=T,of = "GTiff", crop_to_cutline=T, overwrite=T)
  #
  # }
  # 1st error -- click just outside of polygon
  #clickExtent()

  library(reproducible)
  cacheRepo <- file.path(dirname(tempdir()), "reproducibleCache")

  cacheRepo <- file.path("c:/Eliot/data",
                         "Boreal-ecosystem-anthropogenic-disturbance-vector-data-2008-2010")
  checkPath(cacheRepo, create = TRUE)
  b <- Cache(reproducible::prepInputs, cacheRepo = cacheRepo,
             destinationPath = dirname(cacheRepo),
             alsoExtract = c("EC_borealdisturbance_polygonal_2008_2010_FINAL_ALBERS.dbf",
                           "EC_borealdisturbance_polygonal_2008_2010_FINAL_ALBERS.prj",
                           "EC_borealdisturbance_polygonal_2008_2010_FINAL_ALBERS.sbn",
                           "EC_borealdisturbance_polygonal_2008_2010_FINAL_ALBERS.sbx",
                           "EC_borealdisturbance_polygonal_2008_2010_FINAL_ALBERS.shp",
                           "EC_borealdisturbance_polygonal_2008_2010_FINAL_ALBERS.shp.xml",
                           "EC_borealdisturbance_polygonal_2008_2010_FINAL_ALBERS.shx"),
             archive = "Boreal-ecosystem-anthropogenic-disturbance-vector-data-2008-2010.zip",
             showSimilar = TRUE, #cacheId = "74b33a4fb9ba7b537c017e8d0ec3e6e6",
             targetFile = "EC_borealdisturbance_polygonal_2008_2010_FINAL_ALBERS.shp",
             url = "http://www.ec.gc.ca/data_donnees/STB-DGST/003/Boreal-ecosystem-anthropogenic-disturbance-vector-data-2008-2010.zip")


  dev(); clearPlot(); Plot(b); clickExtent()


})
