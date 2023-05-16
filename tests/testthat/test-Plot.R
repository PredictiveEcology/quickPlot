##  to regenerate tests/fingerprints/fingerprints.csv, use:
##    Sys.setenv(R_QUICKPLOT_NEW_FINGERPRINTS = TRUE)

skip_on_ci() ## August 2022 -- GitHub actions fingerprints differ by system + R version

## block A
test_that("Plot 1 is not error-free", {
  tmpdir <- withr::local_tempdir()
  withr::local_dir(new = tmpdir)
  withr::local_package("terra")
  on.exit({
    if (length(dev.list()) > 0) dev.off()
    if (file.exists("Rplots.pdf")) file.remove("Rplots.pdf")
  #   unlink(tmpdir, recursive = TRUE)
  }, add = TRUE) # nolint

  ras <- rast(xmin = 0, xmax = 10, ymin = 0, ymax = 10,
                vals = sample(1:4, replace = TRUE, size = 100), res = 1)
  DEM8765 <- ras
  names(DEM8765) <- "DEM87654"
  DEM87654 <- raster::raster(DEM8765)

  habitatQuality8765 <- rast(ras)
  habitatQuality8765[] <- sample(1:10, replace = TRUE, size = 100)
  names(habitatQuality8765) <- "habitatQuality87654"
  habitatQuality87654 <- raster::raster(habitatQuality8765)

  landscape8765 <- c(DEM8765, habitatQuality8765)
  landscape87654 <- raster::stack(landscape8765)
  caribou8765 <- terra::vect(type = "points",
    x = cbind(x = stats::runif(1e1, 0, 10), y = stats::runif(1e1, 0, 10))
  )
  caribou87654 <- as(caribou8765, "Spatial")


  Sr1 <- sp::Polygon(cbind(c(2, 4, 4, 1, 2), c(2, 3, 5, 4, 2)))
  Sr2 <- sp::Polygon(cbind(c(5, 4, 2, 5), c(2, 3, 2, 2)))
  Srs1 <- sp::Polygons(list(Sr1), "s1")
  Srs2 <- sp::Polygons(list(Sr2), "s2")
  SpP87654 <- sp::SpatialPolygons(list(Srs1, Srs2), 1:2)
  SpP8765 <- terra::vect(SpP87654)



  # Test polygon with > 1e3 points to test the speedup parameter
  Sr1 <- sp::Polygon(cbind(c(2, 4, 4, 1, 2), c(2, 3, 5, 4, 2)))
  Sr2 <- sp::Polygon(cbind(c(5, 4, 2, 5), c(2, 3, 2, 2)))
  Srs1 <- sp::Polygons(list(Sr1), "s1")
  Srs2 <- sp::Polygons(list(Sr2), "s2")
  r <- 1
  N <- 1000
  cx <- 0
  cy <- 0
  a <- seq(0, 2 * pi, length.out = N)
  x <- cx + r * cos(a)
  y <- cy + r * sin(a)
  Sr1 <- sp::Polygon(cbind(x, y))
  Sr2 <- sp::Polygon(cbind(c(5, 4, 2, 5), c(2, 3, 2, 2)))
  Srs1 <- sp::Polygons(list(Sr1), "s1")
  Srs2 <- sp::Polygons(list(Sr2), "s2")
  SpP874 <- sp::SpatialPolygons(list(Srs1, Srs2), 1:2)
  SpP87 <- terra::vect(SpP874)


  # test SpatialLines
  l1 <- cbind(c(10, 2, 30), c(30, 2, 2))
  l1a <- cbind(l1[, 1] + .05, l1[, 2] + .05)
  l2 <- cbind(c(1, 20, 3), c(10, 1.5, 1))
  Sl1 <- sp::Line(l1)
  Sl1a <- sp::Line(l1a)
  Sl2 <- sp::Line(l2)
  S1 <- sp::Lines(list(Sl1, Sl1a), ID = "a")
  S2 <- sp::Lines(list(Sl2), ID = "b")
  Sl87654 <- sp::SpatialLines(list(S1, S2))
  Sl8765 <- terra::vect(Sl87654)

  # If any rearrangements are required, Plot searches for objects in Global Env
  # So all tests must run a clearPlot or a new = TRUE to be cleared to
  # prevent rearrangements
  clearPlot()
  expect_error(Plot(asdfd))

  cars <- list(caribou8765, caribou87654)
  lands <- list(landscape8765, landscape87654)
  habs <- list(habitatQuality8765, habitatQuality87654)
  DEMs <- list(DEM8765, DEM87654)
  SpPs <- list(SpP8765, SpP87654)
  SpP8s <- list(SpP87, SpP874)
  Sls <- list(Sl8765, Sl87654)

  for (i in seq_along(lands)) {
    car <- cars[[sample(2, 1)]]
    land <- lands[[sample(2, 1)]]
    DEM <- DEMs[[sample(2, 1)]]
    hab <- habs[[sample(2, 1)]]
    SpP <- SpPs[[sample(2, 1)]]
    SpP8 <- SpP8s[[sample(2, 1)]]
    Sl <- Sls[[sample(2, 1)]]

    clearPlot()
    expect_silent(Plot(land))
    clearPlot()
    expect_silent(Plot(car))
    # Test speedup > 0.1 for SpatialPoints
    clearPlot()
    expect_silent(Plot(car, speedup = 2))
    clearPlot()
    expect_silent(Plot(land))
    # can add a plot to the plotting window
    expect_silent(Plot(car, new = FALSE))
    clearPlot()
    # Can add two maps with same name, if one is in a stack; they are given
    #  unique names based on object name
    expect_silent(Plot(land, car, DEM))
    # can mix stacks, rasters, SpatialPoint*
    clearPlot()
    expect_silent(Plot(land, hab, car))
    # can mix stacks, rasters, SpatialPoint*, and SpatialPolygons*
    clearPlot()
    expect_silent(Plot(land, car))


    clearPlot()
    expect_silent(Plot(SpP))
    clearPlot()
    expect_silent(Plot(land, car, SpP, new = TRUE))
    clearPlot()
    expect_silent(Plot(SpP))
    clearPlot()
    expect_silent(Plot(land, car, SpP, new = TRUE))

    expect_silent(Plot(SpP8, new = TRUE))
    expect_silent(Plot(Sl))

  }



  M <- 2
  polys1 <- lapply(seq(M), function(m) {
    N <- 20
    adds <- rep(1:N, each = 4)
    x <- rep((c(0,0,1,1) + m ) * N , N)
    y <- rep(c(0,1,1,0), N) + adds
    polyNum <- adds
    coords1 <- cbind(x, y, polyNum)
    polys1 <- sp::Polygons(by(coords1, polyNum, function(coo) {
      list(sp::Polygon(coo[, 1:2]))}), paste0("ss", m))#[], paste0("s", unique(coo[, 3])))

    polys1
  })
  polys2 <- sp::SpatialPolygons(polys1, seq(M))
  Plot(polys2, new = TRUE, col = c("red", "blue"))
  Plot(polys2, new = TRUE, col = c("Set3"))

  polys <- terra::vect(polys2)
  Plot(polys, new = TRUE, col = c("red", "blue"))
  Plot(polys, new = TRUE, col = c("Set3"))

  for (pol in list(polys2, polys)) {
    mess <- capture_messages(Plot(pol, new = TRUE, col = c("red", "blue", "green")))
    expect_true(sum(grepl("Incorrect", mess)) == 1)
    mess <- capture_messages(Plot(pol, new = TRUE, col = RColorBrewer::brewer.pal(8, "Set3")))
    expect_true(sum(grepl("Incorrect", mess)) == 1)
    mess <- capture_messages(Plot(pol, new = TRUE, gp = gpar(fill = "Set3")))
  }

  # Test polygon with > 1e3 points to test the speedup parameter
  r <- 1
  N <- 1000
  cx <- 0
  cy <- 0
  a <- seq(0, 2 * pi, length.out = N)
  x <- cx + r * cos(a)
  y <- cy + r * sin(a)
  l1 <- cbind(x, y)
  l1a <- cbind(l1[, 1] + .05, l1[, 2] + .05)
  l2 <- cbind(c(1, 20, 3), c(10, 1.5, 1))
  Sl1 <- sp::Line(l1)
  Sl1a <- sp::Line(l1a)
  Sl2 <- sp::Line(l2)
  S1 <- sp::Lines(list(Sl1, Sl1a), ID = "a")
  S2 <- sp::Lines(list(Sl2), ID = "b")
  Sl87654 <- sp::SpatialLines(list(S1, S2))
  Sl8765 <- terra::vect(Sl87654)
  Sls <- list(Sl87654, Sl8765)


  # test speedup
  caribou874 <- sp::SpatialPoints(
    coords = cbind(x = stats::runif(1.1e3, 0, 10), y = stats::runif(1.1e3, 0, 10))
  )
  caribou87 <- terra::vect(caribou874)

  cars2 <- list(caribou87, caribou874)
  for (i in seq_along(Sls)) {
    land <- lands[[sample(2, 1)]]
    Sl <- Sls[[sample(2, 1)]]
    Sp <- SpPs[[sample(2, 1)]]
    car <- cars[[sample(2, 1)]]
    car2 <- cars2[[sample(2, 1)]]
    DEM <- DEMs[[sample(2, 1)]]
    suppressWarnings(terra::crs(Sp) <- terra::crs(land))
    suppressWarnings(terra::crs(Sl) <- terra::crs(land))
    expect_silent(Plot(land, new = TRUE))
    expect_silent(Plot(Sl, new = TRUE))
    expect_silent(Plot(land$DEM87654, addTo = "land$habitatQuality87654"))
    expect_silent(Plot(Sp, addTo = "land$habitatQuality87654"))
    # test various arguments
    clearPlot()
    expect_silent(Plot(car, new = TRUE, gpAxis = gpar(cex = 0.4), size = 1))
    clearPlot()
    expect_silent(Plot(DEM, gpText = gpar(cex = 0.4)))
    # test colors
    clearPlot()
    expect_silent(Plot(DEM, cols = c("blue", "red")))
    # Should work with col as well as cols
    clearPlot()
    expect_silent(Plot(DEM, col = c("blue", "red")))
    # test visualSqueeze
    clearPlot()
    expect_silent(Plot(DEM, visualSqueeze = 0.2, new = TRUE))
    clearPlot()
    expect_silent(Plot(car2, speedup = 10, new = TRUE))
  }



  # test ggplot2 and hist -- don't work unless invoke global environment
  clearPlot()
  dev()
  hist87654 <- hist(stats::rnorm(1e3), plot = FALSE)
  clearPlot()
  expect_silent(Plot(hist87654))

  # test ggplot2 and hist -- don't work unless invoke global environment
  clearPlot()


  if (requireNamespace("ggplot2")) {
    suppressWarnings(ggplot87654 <- ggplot2::qplot(stats::rnorm(1e3), binwidth = 0.3,
                                                   geom = "histogram")) # warning is about deprecation
    expect_silent(Plot(ggplot87654))
  }

  for (i in seq_along(Sls)) {
    land <- lands[[sample(2, 1)]]
    Sl <- Sls[[sample(2, 1)]]
    Sp <- SpPs[[sample(2, 1)]]
    car <- cars[[sample(2, 1)]]
    car2 <- cars2[[sample(2, 1)]]
    DEM <- DEMs[[sample(2, 1)]]

    # test rearrangements
    expect_silent(Plot(car, new = TRUE))
    expect_silent(Plot(DEM))
    expect_silent(Plot(land))

    testPlot <- Plot(land)
    expect_silent(Plot(testPlot))
    expect_silent(Plot(car, addTo = "DEM"))
    expect_silent(rePlot())
  }
})

## block B
test_that("Unit tests for image content is not error-free", {
  skip_if_not_installed("visualTest")

  # library(terra)
  # library(visualTest)
  fingerprints <- setupTestFingerprints()

  tmpdir <- file.path(tempdir(), "test_Plot_imageContent")
  dir.create(tmpdir)

  cwd <- getwd()

  on.exit({
    if (length(dev.list()) > 0) dev.off()
    unlink(tmpdir, recursive = TRUE)
  }, add = TRUE) # nolint

  ncol <- 3
  nrow <- 4
  N <- ncol * nrow
  nLevels <- 4

  # Test legend with a factor raster
  set.seed(24334)
  ras <- rast(matrix(sample(1:nLevels, size = N, replace = TRUE),
                       ncol = ncol, nrow = nrow))
  levels(ras) <- data.frame(ID = 1:nLevels, Class = paste0("Level", 1:nLevels))

  # New Section
  png(file = file.path(tmpdir, "test.png"), width = 400, height = 300)
  clearPlot()
  Plot(ras, new = TRUE)
  dev.off()

  test_id <- "B1"
  if (Sys.getenv("R_QUICKPLOT_NEW_FINGERPRINTS") == "TRUE") {
    newValue <- data.table(test_id = test_id, r_version = r_version(), sys_name = sysname(),
                           value =  getFingerprint(file = file.path(tmpdir, "test.png")))
    fingerprints <- updateFingerprint(newValue = newValue, fingerprints = fingerprints)
  }
  orig <- fingerprint(fingerprints, test_id, r_version(), sysname())
  expect_true(isSimilar(file = file.path(tmpdir, "test.png"), fingerprint = orig, threshold = 0.3))
  # New Section

  # Test legend with a factor raster
  set.seed(24334)
  ras <- rast(matrix(sample(1:nLevels, size = N, replace = TRUE),
                       ncol = ncol, nrow = nrow))
  png(file = file.path(tmpdir, "test.png"), width = 400, height = 300)
  clearPlot()
  Plot(ras)
  dev.off()

  test_id <- "B2"
  if (Sys.getenv("R_QUICKPLOT_NEW_FINGERPRINTS") == "TRUE") {
    newValue <- data.table(test_id = test_id, r_version = r_version(), sys_name = sysname(),
                           value =  getFingerprint(file = file.path(tmpdir, "test.png")))
    fingerprints <- updateFingerprint(newValue = newValue, fingerprints = fingerprints)
  }
  orig <- fingerprint(fingerprints, test_id, r_version(), sysname())
  expect_true(isSimilar(file = file.path(tmpdir, "test.png"), fingerprint = orig, threshold = 0.3))
  # New Section

  # test non contiguous factor raster
  nLevels <- 6
  N <- ncol * nrow
  set.seed(24334)
  levs <- (1:nLevels)[-((nLevels - 2):(nLevels - 1))] # nolint
  ras <- rast(matrix(sample(levs, size = N, replace = TRUE),
                       ncol = ncol, nrow = nrow))
  levels(ras) <- data.frame(ID = levs, Class = paste0("Level", levs))
  ras <- setColors(ras, n = 4, c("red", "orange", "blue", "yellow"))

  png(file = file.path(tmpdir, "test.png"), width = 400, height = 300)
  clearPlot()
  Plot(ras, new = TRUE)
  dev.off()

  test_id <- "B3"
  if (Sys.getenv("R_QUICKPLOT_NEW_FINGERPRINTS") == "TRUE") {
    newValue <- data.table(test_id = test_id, r_version = r_version(), sys_name = sysname(),
                           value =  getFingerprint(file = file.path(tmpdir, "test.png")))
    fingerprints <- updateFingerprint(newValue = newValue, fingerprints = fingerprints)
  }
  orig <- fingerprint(fingerprints, test_id, r_version(), sysname())
  expect_true(isSimilar(file = file.path(tmpdir, "test.png"), fingerprint = orig, threshold = 0.3))

  teardownTestFingerprints(fingerprints, cwd)
})

## block C
test_that("Unit tests for plotting colors", {
  skip_if_not_installed("visualTest")

  # library(terra)
  # library(visualTest)
  fingerprints <- setupTestFingerprints()

  tmpdir <- file.path(tempdir(), "test_Plot_colors")
  dir.create(tmpdir)

  cwd <- getwd()

  on.exit({
    if (length(dev.list()) > 0) dev.off()
    unlink(tmpdir, recursive = TRUE)
  }, add = TRUE) # nolint

  ras <- rast(matrix(c(1, 0, 1, 2), ncol = 2))
  setColors(ras, n = 3) <- c("red", "blue", "green")

  ##
  png(file = file.path(tmpdir, "test.png"), width = 400, height = 300)
  clearPlot()

  # should be a 2 x 2 raster, bottom left red, top row blue, bottom right green
  Plot(ras, new = TRUE)
  dev.off()

  test_id <- "C1"
  if (Sys.getenv("R_QUICKPLOT_NEW_FINGERPRINTS") == "TRUE") {
    newValue <- data.table(test_id = test_id, r_version = r_version(), sys_name = sysname(),
                           value =  getFingerprint(file = file.path(tmpdir, "test.png")))
    fingerprints <- updateFingerprint(newValue = newValue, fingerprints = fingerprints)
  }
  orig <- fingerprint(fingerprints, test_id, r_version(), sysname())
  expect_true(isSimilar(file = file.path(tmpdir, "test.png"), fingerprint = orig, threshold = 0.002))
  ##

  ras2 <- rast(matrix(c(3, 1, 1, 2), ncol = 2))
  rasStack <- terra::rast(ras, ras2)
  names(rasStack) <- c("ras", "ras2")
  setColors(rasStack, n = 3) <- list(ras = c("black", "blue", "green"))
  png(file = file.path(tmpdir, "test.png"), width = 400, height = 300)
  clearPlot()

  # should be left 2 x 2 raster, blue top, black bot lef, green bot right;
  # 2nd raster, 2 x 2, topleft green, topRight & botLef grey, botright = beige
  Plot(rasStack, new = TRUE)
  dev.off()

  test_id <- "C2"
  if (Sys.getenv("R_QUICKPLOT_NEW_FINGERPRINTS") == "TRUE") {
    newValue <- data.table(test_id = test_id, r_version = r_version(), sys_name = sysname(),
                           value =  getFingerprint(file = file.path(tmpdir, "test.png")))
    fingerprints <- updateFingerprint(newValue = newValue, fingerprints = fingerprints)
  }
  orig <- fingerprint(fingerprints, test_id, r_version(), sysname())
  expect_true(isSimilar(file = file.path(tmpdir, "test.png"), fingerprint = orig, threshold = 0.3))

  #####

  # Test setColors
  ras <- setColors(ras, c("red", "purple", "orange"), n = 3)
  png(file = file.path(tmpdir, "test.png"), width = 400, height = 300)
  clearPlot()
  Plot(ras, new = TRUE)
  dev.off()

  test_id <- "C3"
  if (Sys.getenv("R_QUICKPLOT_NEW_FINGERPRINTS") == "TRUE") {
    newValue <- data.table(test_id = test_id, r_version = r_version(), sys_name = sysname(),
                           value =  getFingerprint(file = file.path(tmpdir, "test.png")))
    fingerprints <- updateFingerprint(newValue = newValue, fingerprints = fingerprints)
  }
  orig <- fingerprint(fingerprints, test_id, r_version(), sysname())
  expect_true(isSimilar(file = file.path(tmpdir, "test.png"), fingerprint = orig, threshold = 0.3))

  # New Section

  ras <- setColors(ras, c("yellow", "orange"))
  png(file = file.path(tmpdir, "test.png"), width = 400, height = 300)
  clearPlot()
  Plot(ras, new = TRUE)
  dev.off()

  test_id <- "C4"
  if (Sys.getenv("R_QUICKPLOT_NEW_FINGERPRINTS") == "TRUE") {
    newValue <- data.table(test_id = test_id, r_version = r_version(), sys_name = sysname(),
                           value =  getFingerprint(file = file.path(tmpdir, "test.png")))
    fingerprints <- updateFingerprint(newValue = newValue, fingerprints = fingerprints)
  }
  orig <- fingerprint(fingerprints, test_id, r_version(), sysname())
  expect_true(isSimilar(file = file.path(tmpdir, "test.png"), fingerprint = orig, threshold = 8))
  unlink("test.png")

  teardownTestFingerprints(fingerprints, cwd)
})

## block D
test_that("Unit tests for internal functions in Plot", {
  skip_if_not_installed("visualTest")

  # library(terra)
  # library(visualTest)
  fingerprints <- setupTestFingerprints()

  tmpdir <- file.path(tempdir(), "test_Plot_internal")
  dir.create(tmpdir)
  cwd <- getwd()

  on.exit({
    if (length(dev.list()) > 0) dev.off()
    unlink(tmpdir, recursive = TRUE)
  }, add = TRUE) # nolint


  # Test .makeColorMatrix for subsampled rasters
  # (i.e., where speedup is high compared to ncells)
  set.seed(1234)
  ras <- rast(matrix(sample(1:3, size = 100, replace = TRUE), ncol = 10))
  setColors(ras, n = 3) <- c("red", "blue", "green")

  png(file = file.path(tmpdir, "test.png"), width = 400, height = 300)
  clearPlot()
  Plot(ras, new = TRUE, speedup = 2e5)
  dev.off()

  test_id <- "D1"
  if (Sys.getenv("R_QUICKPLOT_NEW_FINGERPRINTS") == "TRUE") {
    newValue <- data.table(test_id = test_id, r_version = r_version(), sys_name = sysname(),
                           value =  getFingerprint(file = file.path(tmpdir, "test.png")))
    fingerprints <- updateFingerprint(newValue = newValue, fingerprints = fingerprints)
  }
  orig <- fingerprint(fingerprints, test_id, r_version(), sysname())
  expect_true(isSimilar(file = file.path(tmpdir, "test.png"), fingerprint = orig, threshold = 0.3))

  # New Section
  # Test that NA rasters plot correctly, i.e., with na.color only
  ras <- matrix(NA_real_, ncol = 3, nrow = 3)
  ras <- suppressWarnings(rast(ras)) # There is a min and max warning on NA rasters
  setColors(ras, n = 3) <- c("red", "blue", "green")

  png(file = file.path(tmpdir, "test.png"), width = 400, height = 300)
  clearPlot()
  suppressWarnings(Plot(ras, new = TRUE, speedup = 2e5))
  dev.off()

  test_id <- "D2"
  if (Sys.getenv("R_QUICKPLOT_NEW_FINGERPRINTS") == "TRUE") {
    newValue <- data.table(test_id = test_id, r_version = r_version(), sys_name = sysname(),
                           value =  getFingerprint(file = file.path(tmpdir, "test.png")))
    fingerprints <- updateFingerprint(newValue = newValue, fingerprints = fingerprints)
  }
  orig <- fingerprint(fingerprints, test_id, r_version(), sysname())
  expect_true(isSimilar(file = file.path(tmpdir, "test.png"), fingerprint = orig, threshold = 0.3))

  # New Section
  # Test legendRange in Plot
  set.seed(1234)
  ras <- rast(matrix(sample(1:3, size = 100, replace = TRUE), ncol = 10))
  setColors(ras, n = 3) <- c("red", "blue", "green")

  png(file = file.path(tmpdir, "test.png"), width = 400, height = 300)
  clearPlot()
  Plot(ras, legendRange = 0:5, new = TRUE)
  dev.off()

  test_id <- "D3"
  if (Sys.getenv("R_QUICKPLOT_NEW_FINGERPRINTS") == "TRUE") {
    newValue <- data.table(test_id = test_id, r_version = r_version(), sys_name = sysname(),
                           value =  getFingerprint(file = file.path(tmpdir, "test.png")))
    fingerprints <- updateFingerprint(newValue = newValue, fingerprints = fingerprints)
  }
  orig <- fingerprint(fingerprints, test_id, r_version(), sysname())
  expect_true(isSimilar(file = file.path(tmpdir, "test.png"), fingerprint = orig, threshold = 0.3))

  teardownTestFingerprints(fingerprints, cwd)
})

## block E
test_that("Plot 2 is not error-free", {
  skip_if_not_installed("visualTest")

  # library(terra)
  # library(visualTest)
  fingerprints <- setupTestFingerprints()

  tmpdir <- file.path(tempdir(), "test_Plot2")
  dir.create(tmpdir)

  cwd <- getwd()

  on.exit({
    if (length(dev.list()) > 0) dev.off()
    unlink(tmpdir, recursive = TRUE)
  }, add = TRUE) # nolint

  set.seed(123)
  r <- rast(matrix(sample(1:3, size = 100, replace = TRUE), ncol = 10))

  png(file = file.path(tmpdir, "test.png"), width = 400, height = 300)
  clearPlot()
  spplot(r, colorkey = FALSE, interpolate = FALSE,
         col.regions = colorRampPalette(c("black", "red"))(30))
  dev.off()

  png(file = file.path(tmpdir, "test.png"), width = 400, height = 300)
  clearPlot()
  Plot(r, visualSqueeze = 0.88, title = FALSE,
       legend = FALSE, cols = colorRampPalette(c("black", "red"))(3)
  )
  dev.off()

  test_id <- "E1"
  if (Sys.getenv("R_QUICKPLOT_NEW_FINGERPRINTS") == "TRUE") {
    newValue <- data.table(test_id = test_id, r_version = r_version(), sys_name = sysname(),
                           value =  getFingerprint(file = file.path(tmpdir, "test.png")))
    fingerprints <- updateFingerprint(newValue = newValue, fingerprints = fingerprints)
  }
  orig <- fingerprint(fingerprints, test_id, r_version(), sysname())
  expect_true(isSimilar(file = file.path(tmpdir, "test.png"), fingerprint = orig, threshold = 4))

  teardownTestFingerprints(fingerprints, cwd)

  # New Section#
  skip("Remainder are visual tests ... difficult to assess - see verbal expectations")

  clearPlot()

  ## 128 < vals < 1806
  Plot(r) # Expect rainbow colors, lots of peach, little green

  ## -71 < vals < 1606
  r1 <- r - 200
  clearPlot()
  Plot(r1) # Expect legend from below 0 to just above 1500

  ## 0 < vals <= 1
  r1 <- r / max(getValues(r), na.rm = TRUE)
  clearPlot()
  Plot(r1, new = TRUE) # Expect legend from below 0.2 to exactly 1

  ## 0 <= vals < 1
  r1 <- (r - min(getValues(r), na.rm = TRUE)) / max(getValues(r), na.rm = TRUE)
  clearPlot()
  Plot(r1, new = TRUE)# Expect legend from exactly 0 to above 0.8

  ## 0 <= vals <= 1
  r1 <- r - min(getValues(r), na.rm = TRUE)
  r1 <- r1 / max(getValues(r1), na.rm = TRUE)
  clearPlot()
  Plot(r1, new = TRUE)# Expect legend from exactly 0 to exactly 1

  ## 0, 1, 2, 3
  r1 <- rast(ncol = 3, nrow = 3)
  set.seed(234)
  r1[] <- sample(0:3, replace = TRUE, size = 9)
  clearPlot()
  Plot(r1, new = TRUE) # integers - 0, 1, 2 and 3 should line up with centre of
                       # each color, even though there is no peach in plot

  ## 0, 1 #
  r1 <- rast(ncol = 3, nrow = 3)
  r1[] <- sample(0:1, replace = TRUE, size = 9)
  clearPlot()
  Plot(r1, new = TRUE) # Expect 0 and 1 lined up to middle of green and light grey
                       #  only Green and light grey
  Plot(r1, new = TRUE, zero.color = "black") # black zeros

  ## 0, 1, 2, 3, ... 30
  r1 <- rast(ncol = 30, nrow = 30)
  r1[] <- sample(0:30, replace = TRUE, size = 900)
  Plot(r1, new = TRUE)
  Plot(r1, new = TRUE, zero.color = "black") # black zeros, some scattered

  ## black zeros, plus legend -10 to 40
  Plot(r1, new = TRUE, zero.color = "black", legendRange = c(-10, 40))

  ## 0, 1, 2, 3, 4, 5, 6
  r1 <- rast(ncol = 30, nrow = 30)
  r1[] <- sample(0:6, replace = TRUE, size = 900)
  Plot(r1, new = TRUE)

  ## 1, 2, 3, 4, 5, 6, ... 200
  r1 <- rast(ncol = 30, nrow = 30)
  r1[] <- sample(1:200, replace = TRUE, size = 900)
  #Plot(r1, new = TRUE)

  # should be no black because no zeros
  Plot(r1, new = TRUE, zero.color = "black")

  ## should be slim black in legend, none in fig
  Plot(r1, new = TRUE, zero.color = "black", legendRange = c(-10, 200))

  ## 31, 32, ... 40
  r1 <- rast(ncol = 30, nrow = 30)
  r1[] <- sample(31:40, replace = TRUE, size = 900)
  Plot(r1, new = TRUE)
  Plot(r1, new = TRUE, legendRange = c(0, 40)) # legend from 0 to 40, mostly green
  Plot(r1, new = TRUE, zero.color = "black") # no black
  Plot(r1, new = TRUE, zero.color = "black", legendRange = c(35, 40)) # lots of white

  pixelGroupMap <- rast(xmin = 50, xmax = 50 + 3 * 100,
                          ymin = 50, ymax = 50 + 3 * 100,
                          res = c(100, 100), val = 1)
  pixelGroupMap[1] <- -1
  pixelGroupMap[2:6] <- 2
  clearPlot()
  Plot(pixelGroupMap, new = TRUE)

  ## legend Should have all colors
  Plot(pixelGroupMap, new = TRUE, cols = c("red", "yellow", "green", "blue"))

  ## Test legend that is pre-set, even with various types of rasters
  ## should be dark red raster, legend from 0 to 200
  clearPlot()
  Plot(r1, legendRange = c(0, 200), new = TRUE, cols = c("red", "green"))

  # should be mostly red raster, a bit of green, legend below 0 to 2000
  Plot(r1, legendRange = c(-200, 2000), new = TRUE, cols = c("red", "green"))

  # zero.color on Real numbers doesn't do anything - expect NO BLACK
  r1 <- r - 200
  clearPlot()
  Plot(r1, new = TRUE, zero.color = "black") # NO BLACK

  # zero.color on Integer numbers should work - expect BLACK both in legend and in a few cells
  r1 <- r - 1000
  r1 <- round(r1 / 300, 0)
  clearPlot()
  Plot(r1, new = TRUE, zero.color = "black")

  Plot(pixelGroupMap, zero.color = "red")
  Plot(r)

  clearPlot()
  Plot(pixelGroupMap, cols = "Blues", new = TRUE, legendRange = c(-3, 4))
  Plot(r)
  pixelGroupMap[] <- pixelGroupMap[] + 5
  Plot(pixelGroupMap, na.color = "white") # Should keep one dark Blue, rest white

  # raster with bottom not zero
  r1 <- raster(ncol = 30, nrow = 30)
  r1[] <- sample(17:83, replace = TRUE, size = 900)
  setColors(r1) <- c("green", "red")
  Plot(r1, new = TRUE)
})

## block F
test_that("setColors is not error-free", {
  skip("Apparently color palettes are not universal")

  # library(raster)

  tmpdir <- file.path(tempdir(), "test_setColors")
  dir.create(tmpdir)
  cwd <- getwd()

  on.exit({
    unlink(tmpdir, recursive = TRUE)
  }, add = TRUE) # nolint

  set.seed(1234)
  ras1 <- raster(matrix(sample(1:3, size = 100, replace = TRUE), ncol = 10))
  ras2 <- raster(matrix(sample(1:3, size = 100, replace = TRUE), ncol = 10))
  rasStack <- stack(ras1, ras2)
  expect_error({
    setColors(rasStack, n = c(ras1 = 3, ras2 = 5)) <-
      list(ras1 = c("red", "blue", "green"), ras2 = c("purple", "yellow"))
  })
  names(rasStack) <- c("ras1", "ras2")
  expect_silent({
    setColors(rasStack, n = c(ras1 = 3, ras2 = 5)) <-
      list(ras1 = c("red", "blue", "green"), ras2 = c("purple", "yellow"))
  })

  expect_true(identical(
    getColors(rasStack),
    structure(list(ras1 = c("#FF0000FF", "#0000FFFF", "#00FF00FF"),
                   ras2 = c("#A020F0FF", "#B757B3FF", "#CF8F78FF", "#E7C73CFF",
                            "#FFFF00FF")),
              .Names = c("ras1", "ras2"))
  ))

  ras3 <- raster(matrix(sample(1:3, size = 100, replace = TRUE), ncol = 10))
  rasStack <- stack(rasStack, ras3)
  names(rasStack)[3] <- "ras3"

  expect_silent({
    setColors(rasStack, n = c(ras1 = 3, 5)) <- list(
      ras1 = c("red", "blue", "green"),
      ras2 = c("purple", "yellow"),
      ras3 = c("orange", "yellow")
    )
  })
  expect_true(identical(
    getColors(rasStack),
    structure(list(
      ras1 = c("#FF0000FF", "#0000FFFF", "#00FF00FF"),
      ras2 = c("#A020F0FF", "#B757B3FF", "#CF8F78FF", "#E7C73CFF", "#FFFF00FF"),
      ras3 = c("#FFA500FF", "#FFBB00FF", "#FFD200FF", "#FFE800FF", "#FFFF00FF")),
      .Names = c("ras1", "ras2", "ras3"))
  ))
})

## block G
test_that("Plot with base is not error-free", {
  skip_if_not_installed("visualTest")

  # library(raster)
  # library(visualTest)
  # library(ggplot2)
  # library(igraph)
  fingerprints <- setupTestFingerprints()

  tmpdir <- file.path(tempdir(), "test_Plot1")
  dir.create(tmpdir)
  cwd <- getwd()

  on.exit({
    unlink(tmpdir, recursive = TRUE)
  }, add = TRUE) # nolint

  set.seed(123)
  rasOrig <- raster(extent(0, 40, 0, 20), vals = sample(1:8, replace = TRUE, size = 800), res = 1)
  ras <- rasOrig
  aTime <- Sys.time()

  # New Section
  clearPlot()
  png(file = file.path(tmpdir, "test.png"), width = 400, height = 300)
  clearPlot()
  Plot(ras)
  dev.off()

  test_id <- "G1"
  if (Sys.getenv("R_QUICKPLOT_NEW_FINGERPRINTS") == "TRUE") {
    newValue <- data.table(test_id = test_id, r_version = r_version(), sys_name = sysname(),
                           value =  getFingerprint(file = file.path(tmpdir, "test.png")))
    fingerprints <- updateFingerprint(newValue = newValue, fingerprints = fingerprints)
  }
  orig <- fingerprint(fingerprints, test_id, r_version(), sysname())
  expect_true(isSimilar(file = file.path(tmpdir, "test.png"), fingerprint = orig, threshold = 0.3))

  # New Section
  set.seed(123)
  png(file = file.path(tmpdir, "test.png"), width = 400, height = 300)
  clearPlot()
  ras[] <- sort(ras[])
  Plot(ras)
  ras[] <- sample(ras[])
  Plot(ras)
  Plot(rasOrig)
  dev.off()

  test_id <- "G2"
  if (Sys.getenv("R_QUICKPLOT_NEW_FINGERPRINTS") == "TRUE") {
    newValue <- data.table(test_id = test_id, r_version = r_version(), sys_name = sysname(),
                           value =  getFingerprint(file = file.path(tmpdir, "test.png")))
    fingerprints <- updateFingerprint(newValue = newValue, fingerprints = fingerprints)
  }
  orig <- fingerprint(fingerprints, test_id, r_version(), sysname())
  expect_true(isSimilar(file = file.path(tmpdir, "test.png"), fingerprint = orig, threshold = 0.3))

  # New Section

  # Test overplotting, replotting
  set.seed(123)
  png(file = file.path(tmpdir, "test.png"), width = 400, height = 300)
  clearPlot()
  ras[] <- sort(ras[])
  Plot(ras, cols = "Reds")
  ras[] <- sample(ras[])
  Plot(ras)
  Plot(rasOrig)
  dev.off()

  test_id <- "G3"
  if (Sys.getenv("R_QUICKPLOT_NEW_FINGERPRINTS") == "TRUE") {
    newValue <- data.table(test_id = test_id, r_version = r_version(), sys_name = sysname(),
                           value =  getFingerprint(file = file.path(tmpdir, "test.png")))
    fingerprints <- updateFingerprint(newValue = newValue, fingerprints = fingerprints)
  }
  orig <- fingerprint(fingerprints, test_id, r_version(), sysname())
  expect_true(isSimilar(file = file.path(tmpdir, "test.png"), fingerprint = orig, threshold = 0.3))

  # New Section

  png(file = file.path(tmpdir, "test.png"))
  clearPlot()
  Plot(1:10, ylab = "hist")
  dev.off()

  test_id <- "G4"
  if (Sys.getenv("R_QUICKPLOT_NEW_FINGERPRINTS") == "TRUE") {
    newValue <- data.table(test_id = test_id, r_version = r_version(), sys_name = sysname(),
                           value =  getFingerprint(file = file.path(tmpdir, "test.png")))
    fingerprints <- updateFingerprint(newValue = newValue, fingerprints = fingerprints)
  }
  orig <- fingerprint(fingerprints, test_id, r_version(), sysname())
  expect_true(isSimilar(file = file.path(tmpdir, "test.png"), fingerprint = orig, threshold = 0.3))

  # Mixing base and grid
  png(file = file.path(tmpdir, "test.png"))
  clearPlot()
  Plot(ras)
  Plot(1:10, ylab = "hist")
  dev.off()

  test_id <- "G5"
  if (Sys.getenv("R_QUICKPLOT_NEW_FINGERPRINTS") == "TRUE") {
    newValue <- data.table(test_id = test_id, r_version = r_version(), sys_name = sysname(),
                           value =  getFingerprint(file = file.path(tmpdir, "test.png")))
    fingerprints <- updateFingerprint(newValue = newValue, fingerprints = fingerprints)
  }
  orig <- fingerprint(fingerprints, test_id, r_version(), sysname())
  expect_true(isSimilar(file = file.path(tmpdir, "test.png"), fingerprint = orig, threshold = 0.3))

  # New Section

  png(file = file.path(tmpdir, "test.png"), width = 500, height = 400)
  ras <- rasOrig
  set.seed(123)
  clearPlot()
  Plot(rnorm(10), addTo = "hist", ylab = "test")
  a <- hist(rnorm(10), plot = FALSE)
  Plot(a, addTo = "histogram", axes = "L", col = "#33EEAA33", xlim = c(-3, 3))
  a <- hist(rnorm(100), plot = FALSE)
  Plot(a, addTo = "histogram", axes = FALSE, col = paste0("#1133FF", "33"),
       xlim = c(-3, 3), xlab = "", ylab = "")
  ras2 <- raster(ras)
  ras2[] <- sample(1:8)
  Plot(ras2)

  if (requireNamespace("ggplot2")) {
    gg1 <- ggplot2::qplot(1:10)
    suppressMessages(Plot(gg1))
  }
  suppressMessages(Plot(rnorm(10), ylab = "hist", new = TRUE))
  Plot(ras2)
  Plot(rnorm(10), ylab = "hist")
  ras <- ras ^ 2
  Plot(ras, new = TRUE, cols = "Reds")
  Plot(rnorm(10), ylab = "hist", new = TRUE, addTo = "hist")
  Plot(ras, new = TRUE, cols = "Reds", addTo = "ras2")
  Plot(ras, cols = "Reds", addTo = "ras2")
  dev.off()

  test_id <- "G6"
  if (Sys.getenv("R_QUICKPLOT_NEW_FINGERPRINTS") == "TRUE") {
    newValue <- data.table(test_id = test_id, r_version = r_version(), sys_name = sysname(),
                           value =  getFingerprint(file = file.path(tmpdir, "test.png")))
    fingerprints <- updateFingerprint(newValue = newValue, fingerprints = fingerprints)
  }
  orig <- fingerprint(fingerprints, test_id, r_version(), sysname())
  expect_true(isSimilar(file = file.path(tmpdir, "test.png"), fingerprint = orig, threshold = 0.3))

  # New Section

  png(file = file.path(tmpdir, "test.png"), width = 500, height = 400)
  ras <- rasOrig
  clearPlot()
  set.seed(3123)
  a <- rnorm(1e2)
  b <- rnorm(1e2)
  Plot(a, axes = TRUE, addTo = "first", visualSqueeze = 0.6)
  Plot(a, b, axes = TRUE, addTo = "second", visualSqueeze = 0.6)
  Plot(1:10, axes = TRUE, addTo = "third", visualSqueeze = 0.6)
  Plot(1:10, 1:10, axes = TRUE, addTo = "fourth", visualSqueeze = 0.6,
       main = "test4", title = FALSE)
  Plot(1:10, 1:10, axes = TRUE, addTo = "fourth", visualSqueeze = 0.6,
       main = "test4", title = "test5")
  Plot(1:10, 1:10, axes = TRUE, addTo = "fifth", visualSqueeze = 0.6,
       main = "test4", title = "test5")
  Plot(ras)
  dev.off()

  test_id <- "G7"
  if (Sys.getenv("R_QUICKPLOT_NEW_FINGERPRINTS") == "TRUE") {
    newValue <- data.table(test_id = test_id, r_version = r_version(), sys_name = sysname(),
                           value =  getFingerprint(file = file.path(tmpdir, "test.png")))
    fingerprints <- updateFingerprint(newValue = newValue, fingerprints = fingerprints)
  }
  orig <- fingerprint(fingerprints, test_id, r_version(), sysname())
  expect_true(isSimilar(file = file.path(tmpdir, "test.png"), fingerprint = orig, threshold = 0.3))

  # New Section

  png(file = file.path(tmpdir, "test.png"), width = 400, height = 300)
  set.seed(123)
  ras <- rasOrig
  ras2 <- ras
  ras2[] <- sample(ras[])
  clearPlot()
  Plot(ras,  title = "test", new = TRUE)
  Plot(ras2,  addTo = "ras", cols = "Reds")
  Plot(ras,  addTo = "ras", cols = "Blues")
  dev.off()

  test_id <- "G8"
  if (Sys.getenv("R_QUICKPLOT_NEW_FINGERPRINTS") == "TRUE") {
    newValue <- data.table(test_id = test_id, r_version = r_version(), sys_name = sysname(),
                           value =  getFingerprint(file = file.path(tmpdir, "test.png")))
    fingerprints <- updateFingerprint(newValue = newValue, fingerprints = fingerprints)
  }
  orig <- fingerprint(fingerprints, test_id, r_version(), sysname())
  expect_true(isSimilar(file = file.path(tmpdir, "test.png"), fingerprint = orig, threshold = 0.3))

  teardownTestFingerprints(fingerprints, cwd)
})

## block H
test_that("Plot messages and warnings and errors", {
  rasOrig <- rast(ext(0, 40, 0, 20), vals = sample(1:8, replace = TRUE, size = 800), res = 1)
  ras <- rasOrig
  expect_error(Plot(ras, rnorm(10)), "Can't mix base plots with .quickPlottables")
})

## block I
test_that("rePlot doesn't work", {
  skip_if_not_installed("visualTest")

  # library(raster);
  # library(visualTest)
  fingerprints <- setupTestFingerprints()

  tmpdir <- file.path(tempdir(), "test_Plot1")
  dir.create(tmpdir)
  cwd <- getwd()

  on.exit({
    unlink(tmpdir, recursive = TRUE)
  }, add = TRUE) # nolint

  f1 <- file.path(tmpdir, "test1.png")
  f2 <- file.path(tmpdir, "test2.png")
  png(file = f1, width = 400, height = 300)
    a <- dev.cur()
    set.seed(123)
    rasOrig <- raster(extent(0, 40, 0, 20), vals = sample(1:8, replace = TRUE, size = 800), res = 1)
    ras <- rasOrig
    clearPlot()
    ras <- ras + 1
    Plot(ras)
    Plot(rnorm(10), ylab = "hist")
  dev.off(a)

  png(file = f2, width = 400, height = 300)
    b <- dev.cur()
    rePlot(a, b)
  dev.off(b)

  orig <- getFingerprint(file = f1)
  expect_true(isSimilar(file = f2, fingerprint = orig, threshold = 0.3))
})

## block J
test_that("Plot - going through package coverage", {
  tmpdir <- withr::local_tempdir()
  withr::local_dir(new = tmpdir)
  withr::local_package("terra")
  #
  #
  # tmpdir <- file.path(tempdir(), "test_Plot2")
  # dir.create(tmpdir)
  # cwd <- getwd()
  #
  # on.exit({
  #   unlink(tmpdir, recursive = TRUE)
  # }, add = TRUE) # nolint

  set.seed(123)
  rasOrig <- rast(ext(0, 40, 0, 20), vals = sample(1:8, replace = TRUE, size = 800), res = 1)
  ras <- rasOrig

  if (requireNamespace("fastshp", quietly = TRUE)) {
    expect_silent(Plot(ras, new = TRUE))
  } else {
    expect_message(Plot(ras, new = TRUE)) ## message about fastshp not being installed
  }

  clearPlot(force = TRUE)

  ## do.call version:
  #expect_error(do.call(Plot, list(ras = ras)), "Currently,") # nolint

  try(dev.off())
})

## block K
test_that("Plot lists", {
  skip_if_not_installed("visualTest")

  # library(ggplot2)
  # library(raster)
  # library(visualTest)
  fingerprints <- setupTestFingerprints()

  tmpdir <- file.path(tempdir(), "test_Plot3")
  dir.create(tmpdir)

  cwd <- getwd()

  on.exit({
    unlink(tmpdir, recursive = TRUE)
  }, add = TRUE) # nolint

  clearPlot()
  set.seed(123)
  rasOrig <- raster(
    extent(0, 40, 0, 20), vals = sample(1:8, replace = TRUE, size = 800), res = 1
  )
  ras1 <- ras2 <- ras3 <- ras4 <- rasOrig
  a <- list(); for (i in 1:4) a[[paste0("ras", i)]] <- get(paste0("ras", i))
  Sr1 <- Polygon(cbind(c(2, 4, 4, 1, 2), c(2, 3, 5, 4, 2)) * 20 - 50)
  Sr2 <- Polygon(cbind(c(5, 4, 2, 5), c(2, 3, 2, 2)) * 20 - 50)
  Srs1 <- Polygons(list(Sr1), "s1")
  Srs2 <- Polygons(list(Sr2), "s2")
  SpP <- SpatialPolygons(list(Srs1, Srs2), 1:2)

  png(file = file.path(tmpdir, "test.png"), width = 400, height = 300)
  clearPlot()
  Plot(a)
  dev.off()

  test_id <- "K1"
  if (Sys.getenv("R_QUICKPLOT_NEW_FINGERPRINTS") == "TRUE") {
    newValue <- data.table(test_id = test_id, r_version = r_version(), sys_name = sysname(),
                           value =  getFingerprint(file = file.path(tmpdir, "test.png")))
    fingerprints <- updateFingerprint(newValue = newValue, fingerprints = fingerprints)
  }
  orig <- fingerprint(fingerprints, test_id, r_version(), sysname())
  expect_true(isSimilar(file = file.path(tmpdir, "test.png"), fingerprint = orig, threshold = 0.02))

  if (requireNamespace("fastshp", quietly = TRUE)) {
    set.seed(123)
    a$SpP <- SpP
    png(file = file.path(tmpdir, "test.png"), width = 400, height = 300)
    clearPlot()
    Plot(a)
    dev.off()

    test_id <- "K2"
    if (Sys.getenv("R_QUICKPLOT_NEW_FINGERPRINTS") == "TRUE") {
      newValue <- data.table(test_id = test_id, r_version = r_version(), sys_name = sysname(),
                             value =  getFingerprint(file = file.path(tmpdir, "test.png")))
      fingerprints <- updateFingerprint(newValue = newValue, fingerprints = fingerprints)
    }
    orig <- fingerprint(fingerprints, test_id, r_version(), sysname())
    expect_true(isSimilar(file = file.path(tmpdir, "test.png"), fingerprint = orig, threshold = 0.3))

    set.seed(123)
    if (requireNamespace("ggplot2")) {
      gg <- qplot(1:10, sample(1:10))
      gg1 <- qplot(1:10, sample(1:10))
      b <- list(gg = gg, gg1 = gg1)
      png(file = file.path(tmpdir, "test.png"), width = 400, height = 300)
      clearPlot()
      Plot(a, b)
      dev.off()
    }

    test_id <- "K3"
    if (Sys.getenv("R_QUICKPLOT_NEW_FINGERPRINTS") == "TRUE") {
      newValue <- data.table(test_id = test_id, r_version = r_version(), sys_name = sysname(),
                             value =  getFingerprint(file = file.path(tmpdir, "test.png")))
      fingerprints <- updateFingerprint(newValue = newValue, fingerprints = fingerprints)
    }
    orig <- fingerprint(fingerprints, test_id, r_version(), sysname())
    expect_true(isSimilar(file = file.path(tmpdir, "test.png"), fingerprint = orig, threshold = 0.02))
  }

  teardownTestFingerprints(fingerprints, cwd)
})

## block L
test_that("Plot non-complicated object names", {
  # library(raster)
  withr::local_package("terra")

  a <- list()
  a$e <- new.env()
  rasOrig <- rast(ext(0, 40, 0, 20), vals = sample(1:8, replace = TRUE, size = 800), res = 1)
  rasOrig2 <- rasOrig
  a$e$p <- rasOrig
  a$e$s <- c(rasOrig2, lyr.2 = rasOrig)
  expect_silent(Plot(a$e$p))
  expect_silent(Plot(a$e[["p"]]))
  expect_silent(Plot(a$e[["s"]]$lyr.1))
  expect_silent(Plot(a$e[["s"]]$lyr.1[1:10], addTo = "secondPlot"))

  # add the same data as a different plot -- use a named list
  expect_silent(Plot(list("thirdPlot" = a$e[["s"]]$lyr.1), new = TRUE))
  a$e[["s"]]$lyr.1[2] <- terra::minmax(a$e[["s"]]$lyr.1)[2]
  expect_silent(Plot(list("thirdPlot" = a$e[["s"]]$lyr.1), new = TRUE))
  dev.off()
})

## block M
test_that("Plot functions NOT in quickPlot, i.e. redefining Plot", {
  # library(raster)
  withr::local_package("terra")

  Plot <- function(x) {
    quickPlot::Plot(x)
  }

  expect_silent(Plot(terra::rast(matrix(1:100, 10, 10))))

  try(dev.off())
})
