## block B
test_that("image content is not error-free", {

  testInit("terra", opts = list(quickPlot.verbose = TRUE), dev = FALSE)

  on.exit(
    {
      if (length(dev.list()) > 0) dev.off()
      # unlink(tmpdir, recursive = TRUE)
    },
    add = TRUE) # nolint

  ncol <- 3
  nrow <- 4
  N <- ncol * nrow
  nLevels <- 4

  # Test legend with a factor raster
  set.seed(24334)
  ras <- rast(matrix(sample(1:nLevels, size = N, replace = TRUE), ncol = ncol, nrow = nrow))
  levels(ras) <- data.frame(ID = 1:nLevels, Class = paste0("Level", 1:nLevels))

  # New Section
  for (os in oses) {
    fil <- fn(tmpdir, desc, counter, os)
    announce_snapshot_file(name = basename(fil))
    if (correctOS(os))
      testthat::expect_snapshot_file(
        {
          png(filename = fil, width = 400, height = 300)
          clearPlot()
          Plot(ras, new = TRUE)
          dev.off()
          fil
        },
        basename(fil))
  }

  # Test legend with a factor raster
  set.seed(24334)
  ras <- rast(matrix(sample(1:nLevels, size = N, replace = TRUE), ncol = ncol, nrow = nrow))
  for (os in oses) {
    fil <- fn(tmpdir, desc, counter, os)
    announce_snapshot_file(name = basename(fil))
    if (correctOS(os))
      expect_snapshot_file({
        png(filename = fil, width = 400, height = 300)
        clearPlot()
        Plot(ras)
        dev.off()
        fil
      })
  }

  # test non contiguous factor raster
  fil <- file.path(tmpdir, "test3.png")
  nLevels <- 6
  N <- ncol * nrow
  set.seed(24334)
  levs <- (1:nLevels)[-((nLevels - 2):(nLevels - 1))] # nolint
  ras <- rast(matrix(sample(levs, size = N, replace = TRUE),
    ncol = ncol, nrow = nrow))
  levels(ras) <- data.frame(ID = levs, Class = paste0("Level", levs))
  ras <- setColors(ras, n = 4, c("red", "orange", "blue", "yellow"))

  for (os in oses) {
    fil <- fn(tmpdir, desc, counter, os)
    announce_snapshot_file(name = basename(fil))
    if (correctOS(os))
      expect_snapshot_file({
        png(filename = fil, width = 400, height = 300)
        clearPlot()
        Plot(ras, new = TRUE)
        dev.off()
        fil
      })
  }
})

# # ## block C
test_that("plotting colors", {

  testInit("terra", opts = list(quickPlot.verbose = TRUE), dev = FALSE)
  on.exit(
    {
      if (length(dev.list()) > 0) dev.off()
    },
    add = TRUE) # nolint

  rasts <- list()

  # should be a 2 x 2 raster, bottom left red, top row blue, bottom right green
  rasts[[1]] <- rast(matrix(c(1, 0, 1, 2), ncol = 2))
  setColors(rasts[[1]], n = 3) <- c("red", "blue", "green")

  ras2 <- rast(matrix(c(3, 1, 1, 2), ncol = 2))
  rasts[[2]] <- c(rasts[[1]], ras2)
  names(rasts[[2]]) <- c("ras", "ras2")
  setColors(rasts[[2]], n = 3) <- list(ras = c("black", "blue", "green"))

  rasts[[3]] <- setColors(rasts[[1]], c("red", "purple", "orange"), n = 3)

  ##
  if (requireNamespace("raster", quietly = TRUE)) {
    rasterVec <- seq_along(rasts) + length(rasts)
    rasts[rasterVec] <- lapply(rasts, function(r) {
      if (length(names(r)) == 1) {
        raster::raster(r)
      } else {
        raster::stack(r)
      }
    })
  }
  # # envirHere <- environment()
  Map(testNum = seq_along(rasts), ras = rasts, function(testNum, ras) {
    for (os in oses) {
      fil <- fn(tmpdir, desc, counter, os, envir = envirHere)
      announce_snapshot_file(name = basename(fil))
      if (correctOS(os))
        expect_snapshot_file({
          png(filename = fil, width = 400, height = 300)
          clearPlot()
          Plot(ras, new = TRUE)
          dev.off()
          fil
        })
    }
  })
})

# ## test.png 10 to 11
test_that("internal functions in Plot", {
  testInit("terra", opts = list(quickPlot.verbose = TRUE), dev = FALSE)
  on.exit(
    {
      if (length(dev.list()) > 0) dev.off()
    },
    add = TRUE) # nolint

  on.exit(
    {
      if (length(dev.list()) > 0) dev.off()
    },
    add = TRUE) # nolint


  # Test .makeColorMatrix for subsampled rasters
  # (i.e., where speedup is high compared to ncells)
  rasts <- list()

  # 1 Test .makeColorMatrix for subsampled rasters
  # (i.e., where speedup is high compared to ncells)
  set.seed(1234)
  rasts[[1]] <- rast(matrix(sample(1:3, size = 100, replace = TRUE), ncol = 10))
  setColors(rasts[[1]], n = 3) <- c("red", "blue", "green")

  # 2 Test that NA rasters plot correctly, i.e., with na.color only
  rasts[[2]] <- matrix(NA_real_, ncol = 3, nrow = 3)
  rasts[[2]] <- rast(rasts[[2]]) # There is a min and max warning on NA rasters
  setColors(rasts[[2]], n = 3) <- c("red", "blue", "green")

  # 3 Test legendRange in Plot
  set.seed(1234)
  rasts[[3]] <- rast(matrix(sample(1:3, size = 100, replace = TRUE), ncol = 10))
  setColors(rasts[[3]], n = 3) <- c("red", "blue", "green")

  set.seed(123)
  rasts[[4]] <- rast(matrix(sample(1:3, size = 100, replace = TRUE), ncol = 10))

  if (requireNamespace("raster", quietly = TRUE)) {
    rasterVec <- seq_along(rasts) + length(rasts)
    rasts[rasterVec] <- Map(r = rasts, i = seq_along(rasts), function(r, i) {
      r <- if (length(names(r)) == 1) raster::raster(r) else raster::stack(r)
      cols <- getColors(rasts[[i]])[[1]]
      if (length(cols))
        setColors(r, n = length(cols)) <- cols
      r
    })
  }

  # envirHere <- environment()
  Map(testNum = seq_along(rasts), ras = rasts, function(testNum, ras) {
    val <-  (testNum - 1) %% (length(rasts) / 2) + 1
    for (os in oses) {
      fil <- fn(tmpdir, desc, counter, os, envir = envirHere)
      announce_snapshot_file(name = basename(fil))
      aaaa <<- 1
      on.exit(rm(aaaa, envir = .GlobalEnv))
      if (correctOS(os))
        expect_snapshot_file({
          png(filename = fil, width = 400, height = 300)
          clearPlot()
          set.seed(123)
          switch(val,
            `1` = Plot(rasts[[1]], new = TRUE, speedup = 3.21e4),
            `2` = suppressWarnings(Plot(rasts[[2]], new = TRUE, speedup = 2e5)),
            `3` = Plot(rasts[[3]], legendRange = 0:5, new = TRUE),
            `4` = Plot(rasts[[4]], visualSqueeze = 0.88, title = FALSE,
              legend = FALSE, cols = colorRampPalette(c("black", "red"))(3))

          )
          dev.off()
          fil
        })
    }
  })
})

## block E 15 to
test_that("Plot 2 is not error-free", {
  testInit("terra", opts = list(quickPlot.verbose = TRUE), dev = FALSE)
  on.exit(
    {
      if (length(dev.list()) > 0) dev.off()
    },
    add = TRUE) # nolint

  on.exit(
    {
      if (length(dev.list()) > 0) dev.off()
    },
    add = TRUE) # nolint

  rasts <- list()

  set.seed(123)
  v <- c(128, 400, 1806)
  r <- rast(matrix(sample(v, size = 100, replace = TRUE), ncol = 10))

  rasts <- list()

  ## 128 < vals < 1806
  rasts[[1]] <- r # Expect rainbow colors, lots of peach, little green

  ## -71 < vals < 1606
  rasts[[2]] <- r - 200 # Expect legend from below 0 to just above 1500

  # Expect legend from below 0.2 to exactly 1
  rasts[[3]] <- r / max(as.numeric(values(r)), na.rm = TRUE)

  # Expect legend from exactly 0 to above 0.8
  rasts[[4]] <- (r - min(as.numeric(values(r)), na.rm = TRUE)) / max(as.numeric(values(r)), na.rm = TRUE)

  # Expect legend from exactly 0 to exactly 1
  rasts[[5]] <- r - min(as.numeric(values(r)), na.rm = TRUE)
  rasts[[5]] <- rasts[[5]] / max(as.numeric(values(rasts[[5]])), na.rm = TRUE)

  # integers - 0, 1, 2 and 3 should line up with centre of
  # each color, even though there is no peach in plot
  rasts[[6]] <- rast(ncol = 3, nrow = 3)
  set.seed(391) # no yellow in plot, yes in legend
  rasts[[6]][] <- sample(0:3, replace = TRUE, size = 9)

  #  only Green and light grey with 0 and 1
  rasts[[7]] <- rast(ncol = 3, nrow = 3)
  rasts[[7]][] <- sample(0:1, replace = TRUE, size = 9)

  # many colours 0 to 30
  rasts[[8]] <- rast(ncol = 30, nrow = 30)
  rasts[[8]][] <- sample(0:30, replace = TRUE, size = 900)

  ## 0, 1, 2, 3, 4, 5, 6
  rasts[[9]] <- rast(ncol = 30, nrow = 30)
  rasts[[9]][] <- sample(0:6, replace = TRUE, size = 900)

  ## 1, 2, 3, 4, 5, 6, ... 200
  rasts[[10]] <- rast(ncol = 30, nrow = 30)
  rasts[[10]][] <- sample(1:200, replace = TRUE, size = 900)

  rasts[[11]] <- rast(ncol = 30, nrow = 30)
  rasts[[11]][] <- sample(31:40, replace = TRUE, size = 900)

  rasts[[12]] <- rast(xmin = 50, xmax = 50 + 3 * 100,
    ymin = 50, ymax = 50 + 3 * 100,
    res = c(100, 100), val = 1)
  rasts[[12]][1] <- -1
  rasts[[12]][2:6] <- 2

  rasts[[13]] <- r - 200
  # Plot(rasts[[13]], new = TRUE, zero.color = "black") # NO BLACK

  rasts[[14]] <- r - 1000
  rasts[[14]] <- round(rasts[[14]] / 300, 0)
  rasts[[14]][4] <- 0


  if (requireNamespace("raster", quietly = TRUE)) {
    rasterVec <- seq_along(rasts) + length(rasts)
    rasts[rasterVec] <- Map(r = rasts, i = seq_along(rasts), function(r, i) {
      r <- if (length(names(r)) == 1) raster::raster(r) else raster::stack(r)
      cols <- getColors(rasts[[i]])[[1]]
      if (length(cols))
        setColors(r, n = length(cols)) <- cols
      r
    })
  }

  hasRasterLayer <- sum(vapply(rasts, is, "Raster", FUN.VALUE = logical(1))) > 0
  # if RasterLayer are present, then it will be 14 * 2 long, otherwise, just 14

  # envirHere <- environment()
  Map(testNum = seq_along(rasts), function(testNum) {
    val <-  (testNum - 1) %% (length(rasts) / (1 + hasRasterLayer)) + 1 # this tests whether SpatRaster is same as Raster
    for (os in oses) {
      fil <- fn(tmpdir, desc, counter, os, envir = envirHere)
      announce_snapshot_file(name = basename(fil))
      if (correctOS(os))
        expect_snapshot_file({
          png(filename = fil, width = 400, height = 300)
          clearPlot()
          Plot(rasts[[testNum]], new = TRUE)
          dev.off()
          fil
        })

    }

  })

  # envirHere <- environment()
  Map(testNum = seq_along(rasts), function(testNum) {
    val <-  (testNum - 1) %% (length(rasts) / (1 + hasRasterLayer)) + 1
    if (val %in% c(7, 8, 10, 11, 12, 14)) {
      for (os in oses) {
        fil <- fn(tmpdir, desc, counter, os, envir = envirHere)
        announce_snapshot_file(name = basename(fil))
        if (correctOS(os))
          expect_snapshot_file({
            png(filename = fil, width = 400, height = 300)
            clearPlot()
            set.seed(123)
            switch(as.character(val),

              "7" = Plot(rasts[[testNum]], new = TRUE, zero.color = "black"), # black zeros
              "8" = {
                a <- testNum
                Plot(rasts[[testNum]], new = TRUE, zero.color = "black") # black zeros, some scattered
                ## black zeros, plus legend -10 to 40
                Plot(rasts[[a]], new = TRUE, zero.color = "black", legendRange = c(-10, 40)) # legend changed
              },
              "10" = {
                a <- testNum
                Plot(rasts[[testNum]], new = TRUE, zero.color = "black") # should be no black because no zeros
                Plot(rasts[[a]], new = TRUE, zero.color = "black", legendRange = c(-10, 200))
              },

              ## should be slim black in legend, none in fig
              #

              ## Test legend that is pre-set, even with various types of rasters
              ## should be dark red raster, legend from 0 to 200
              "11" = {
                clearPlot()
                # should be mostly red raster, a bit of green, legend 0 to 200
                Plot(rasts[[11]], legendRange = c(0, 200), new = TRUE, cols = c("red", "green"))
                # should be mostly almost entirely red raster, legend below 0 to 2000
                f <- e <- d <- b <- a <- 11
                Plot(rasts[[a]], legendRange = c(-200, 2000), new = TRUE, cols = c("red", "green"))
                Plot(rasts[[b]], new = TRUE)
                Plot(rasts[[d]], new = TRUE, legendRange = c(0, 40)) # legend from 0 to 40, mostly green
                Plot(rasts[[e]], new = TRUE, zero.color = "black") # no black
                Plot(rasts[[f]], new = TRUE, zero.color = "black", legendRange = c(35, 40)) # lots of white
              },

              ## legend Should have all colors in legend
              "12" = {
                a <- 12
                clearPlot()
                Plot(rasts[[12]], new = TRUE)
                Plot(rasts[[a]], new = TRUE, cols = c("red", "yellow", "green", "blue"))
              },

              "14" = {
                # zero.color on Integer numbers should work - expect BLACK both in legend and in a few cells
                a <- 14
                clearPlot()
                Plot(rasts[[14]], new = TRUE, zero.color = "black")
                # zero.color on Integer numbers should work - expect red both in legend and in a few cells
                Plot(rasts[[a]], zero.color = "red")

              }
            )
            dev.off()
            fil
          })

      }

    }
  })

  # After thought -- move raster values outside of legend
  for (os in oses) {
    fil <- fn(tmpdir, desc, counter, os, envir = envirHere)
    announce_snapshot_file(name = basename(fil))
    if (correctOS(os))
      expect_snapshot_file({
        png(filename = fil, width = 800, height = 600)
        clearPlot()
        set.seed(123)
        Plot(rasts[[12]], cols = "Blues", new = TRUE, legendRange = c(-3, 4))
        rasts[[12]][] <- rasts[[12]][] + 5
        Plot(rasts[[12]], na.color = "white") # Should keep one dark Blue, rest white
        dev.off()
        fil
      })
  }
})


## block F
test_that("setColors is not error-free", {
  # skip("Apparently color palettes are not universal")

  testInit("terra", opts = list(quickPlot.verbose = TRUE), dev = FALSE)
  on.exit(
    {
      if (length(dev.list()) > 0) dev.off()
    },
    add = TRUE) # nolint

  set.seed(1234)
  ras1 <- rast(matrix(sample(1:3, size = 100, replace = TRUE), ncol = 10))
  ras2 <- rast(matrix(sample(1:3, size = 100, replace = TRUE), ncol = 10))
  rasStack <- c(ras1, ras2)
  expect_no_error(setColors(rasStack, n = c(ras1 = 3, ras2 = 5)) <-
    list(ras1 = c("red", "blue", "green"), ras2 = c("purple", "yellow")))
  names(rasStack) <- c("ras1", "ras2")
  expect_no_error({
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

  ras3 <- rast(matrix(sample(1:3, size = 100, replace = TRUE), ncol = 10))
  rasStack <- c(rasStack, ras3)
  names(rasStack)[3] <- "ras3"

  expect_warning({
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
      ras3 = c("#FFA500FF", "#FFD200FF", "#FFFF00FF")),
    .Names = c("ras1", "ras2", "ras3"))
  ))
})

# block G
test_that("Plot with base is not error-free", {

  testInit("terra", opts = list(quickPlot.verbose = TRUE), dev = FALSE)
  on.exit(
    {
      if (length(dev.list()) > 0) dev.off()
    },
    add = TRUE) # nolint

  set.seed(123)
  rasOrig <- rast(ext(0, 40, 0, 20), vals = sample(1:8, replace = TRUE, size = 800), res = 1)
  ras <- rasOrig
  aTime <- Sys.time()
  #   # New Section
  for (os in oses) {
    fil <- fn(tmpdir, desc, counter, os, envir = envirHere)
    announce_snapshot_file(name = basename(fil))
    if (correctOS(os))
      expect_snapshot_file({
        png(filename = fil, width = 800, height = 600)
        clearPlot()
        set.seed(123)
        ras5 <- ras6 <- ras7 <- ras2 <- ras3 <- ras4 <- ras1 <- ras
        Plot(ras, ras1, ras2, ras3, ras4, ras5, ras6, ras7)
        Plot(1:10, ylab = "hist")
        Plot(2:22, addTo = "newOne")

        # New Section
        ras <- rasOrig
        set.seed(123)
        Plot(rnorm(10), addTo = "hist", ylab = "test")
        a <- hist(rnorm(10), plot = FALSE)
        Plot(a, addTo = "histogram", axes = "L", col = "#33EEAA33", xlim = c(-3, 3))
        a <- hist(rnorm(100), plot = FALSE)
        Plot(a, addTo = "histogram", axes = FALSE, col = paste0("#1133FF", "33"),
          xlim = c(-3, 3), xlab = "", ylab = "")
        ras2 <- rast(ras)
        ras2[] <- sample(1:8)
        Plot(ras2)
        dev.off()
        fil
      })
  }

  for (os in oses) {
    fil <- fn(tmpdir, desc, counter, os, envir = envirHere)
    announce_snapshot_file(name = basename(fil))
    if (requireNamespace("ggplot2", quietly = TRUE)) {
      if (correctOS(os))
        expect_snapshot_file({
          png(filename = fil, width = 800, height = 600)
          clearPlot()
          set.seed(123)
          gg1 <- ggplot2::ggplot(data.frame(x = 1:10, y = 1:10)) +
            ggplot2::geom_point(ggplot2::aes(x, y))
          clearPlot()
          suppressMessages(Plot(gg1, title = "gg plot"))
          Plot(ras1, ras2, ras3)
          Plot(rnorm(1:10), ylab = "hist")
          dev.off()
          fil
        })
    }
  }

  # New Section
  for (os in oses) {
    fil <- fn(tmpdir, desc, counter, os, envir = envirHere)
    announce_snapshot_file(name = basename(fil))
    if (correctOS(os))
      expect_snapshot_file({
        png(filename = fil, width = 800, height = 600)
        clearPlot()
        set.seed(123)
        ras <- rasOrig
        a <- rnorm(1e2)
        b <- rnorm(1e2)
        Plot(a, axes = TRUE, addTo = "first", visualSqueeze = 0.6)
        Plot(a, b, axes = TRUE, addTo = "second", visualSqueeze = 0.6)
        Plot(1:10, axes = FALSE, addTo = "third", visualSqueeze = 0.6)
        Plot(1:10, 1:10, axes = "L", addTo = "fourth", visualSqueeze = 0.6,
          main = "test4", title = FALSE)
        Plot(1:10, 1:10, axes = TRUE, addTo = "fourth", visualSqueeze = 0.6,
          main = "test4", title = "test5")
        Plot(1:10, 1:10, axes = TRUE, addTo = "fifth", visualSqueeze = 0.6,
          main = "test4", title = "test5")
        Plot(ras)
        dev.off()
        fil
      })
  }

  # New Section
  for (os in oses) {
    fil <- fn(tmpdir, desc, counter, os, envir = envirHere)
    announce_snapshot_file(name = basename(fil))
    set.seed(123)
    if (correctOS(os))
      expect_snapshot_file({
        png(filename = fil, width = 800, height = 600)
        ras <- rasOrig
        ras2 <- ras
        ras2[] <- sample(ras[])
        clearPlot()
        Plot(ras,  title = "test", new = TRUE)
        Plot(ras2,  addTo = "ras", cols = "Reds")
        Plot(ras2, title = "test2", new = TRUE)
        Plot(ras,  addTo = "ras2", cols = "Blues")
        dev.off()
        fil
      })
  }
})


## block H
test_that("Plot messages and warnings and errors", {
  testInit("terra", opts = list(quickPlot.verbose = TRUE), dev = FALSE)
  rasOrig <- rast(ext(0, 40, 0, 20), vals = sample(1:8, replace = TRUE, size = 800), res = 1)
  ras <- rasOrig
  expect_error(Plot(ras, rnorm(10)), "Can't mix base plots with .quickPlottables")
})

## block I
test_that("rePlot doesn't work", {
  testInit("terra", opts = list(quickPlot.verbose = TRUE), dev = FALSE)
  for (os in oses) {
    fil <- fn(tmpdir, desc, counter, os, envir = envirHere)
    announce_snapshot_file(name = basename(fil))
    if (correctOS(os))
      expect_snapshot_file({
        png(filename = fil, width = 400, height = 300)
        a <- dev.cur()
        set.seed(123)
        rasOrig <- rast(ext(0, 40, 0, 20), vals = sample(1:8, replace = TRUE, size = 800), res = 1)
        ras <- rasOrig
        clearPlot()
        ras <- ras + 1
        Plot(ras)
        Plot(rnorm(10), ylab = "hist")
        dev.off(a)
        fil
      })

    unlink(fil)

    # same file for snapshot b/c basename is same as previous
    announce_snapshot_file(name = basename(fil))
    if (correctOS(os))
      expect_snapshot_file({
        png(filename = fil, width = 400, height = 300)
        b <- dev.cur()
        rePlot(a, b)
        dev.off(b)
        fil
      })
  }

})

## block J
test_that("Plot - going through package coverage", {
  testInit("terra", opts = list(quickPlot.verbose = TRUE), dev = FALSE)

  set.seed(123)
  rasOrig <- rast(ext(0, 40, 0, 20), vals = sample(1:8, replace = TRUE, size = 800), res = 1)
  ras <- rasOrig

  expect_no_error(Plot(ras, new = TRUE))

  clearPlot(force = TRUE)

  ## do.call version:
  # expect_error(do.call(Plot, list(ras = ras)), "Currently,") # nolint

  try(dev.off())
})

# block K
test_that("Plot lists", {
  prevLastPlotNumber <- 48
  testInit("terra", opts = list(quickPlot.verbose = TRUE), dev = FALSE)

  clearPlot()
  set.seed(123)
  rasOrig <- rast(
    ext(0, 40, 0, 20), vals = sample(1:8, replace = TRUE, size = 800), res = 1
  )
  ras1 <- ras2 <- ras3 <- ras4 <- rasOrig
  a <- list()
  for (i in 1:4) a[[paste0("ras", i)]] <- get(paste0("ras", i))

  Sr1 <- cbind(object = 1, cbind(c(2, 4, 4, 1, 2), c(2, 3, 5, 4, 2)) * 20 - 50)
  Sr2 <- cbind(object = 2, cbind(c(5, 4, 2, 5), c(2, 3, 2, 2)) * 20 - 50)
  SpP <- rbind(Sr1, Sr2)
  # Srs1 <- Polygons(list(Sr1), "s1")
  # Srs2 <- Polygons(list(Sr2), "s2")
  # SpP <- SpatialPolygons(list(Srs1, Srs2), 1:2)
  SpP <- terra::vect(SpP, "polygons")

  set.seed(123)
  rasOrig <- rast(ext(0, 40, 0, 20), vals = sample(1:8, replace = TRUE, size = 800), res = 1)
  ras <- rasOrig
  aTime <- Sys.time()
  #   # New Section

  for (os in oses) {
    # Mixing base and grid
    a$SpP <- SpP
    fil <- fn(tmpdir, desc, counter, os, envir = envirHere)
    announce_snapshot_file(name = basename(fil))
    if (correctOS(os))

    # fil <- paste0("test", prevLastPlotNumber + 1 ,".png")
    # fil <- file.path(tmpdir, fil)
    # announce_snapshot_file(name = basename(fil))
    # if (isLinux()) {
      expect_snapshot_file({
        png(filename = fil, width = 800, height = 600)
        clearPlot()
        set.seed(123)
        Plot(a)
        dev.off()
        fil
      })
  }

  for (os in oses) {
    if (correctOS(os)) {
      ggplotVersion <- if (utils::packageVersion("ggplot2") > "3.5.2") "newer" else "older"
      fil <- fn(tmpdir, paste0(desc, "ggplotV", ggplotVersion), counter, os, envir = envirHere)
      announce_snapshot_file(name = basename(fil))
      if (requireNamespace("ggplot2", quietly = TRUE)) {
        ggplotVer <- utils::packageVersion("ggplot2")
        gg <- ggplot2::ggplot(data.frame(x = 1:10, y = sample(1:10))) +
          ggplot2::geom_point(ggplot2::aes(x,  y))
        gg1 <- ggplot2::ggplot(data.frame(x = 1:10, y = sample(1:10))) +
          ggplot2::geom_point(ggplot2::aes(x,  y))
        b <- list(gg = gg, gg1 = gg1)
        expect_snapshot_file({
          png(filename = fil, width = 800, height = 600)
          clearPlot()
          set.seed(123)
          clearPlot()
          Plot(append(a, b))
          dev.off()
          fil
        })

      }
    }
  }
})

## block L
test_that("Plot non-complicated object names", {
  testInit("terra", opts = list(quickPlot.verbose = TRUE), dev = FALSE)

  a <- list()
  a$e <- new.env()
  rasOrig <- rast(ext(0, 40, 0, 20), vals = sample(1:8, replace = TRUE, size = 800), res = 1)
  rasOrig2 <- rasOrig
  a$e$p <- rasOrig
  a$e$s <- c(rasOrig2, lyr.2 = rasOrig)
  clearPlot()
  expect_no_error(Plot(a$e$p))
  expect_no_error(Plot(a$e[["p"]]))
  expect_no_error(Plot(a$e[["s"]]$lyr.1))
  expect_no_error(Plot(a$e[["s"]]$lyr.1[1:10], addTo = "secondPlot"))

  # add the same data as a different plot -- use a named list
  expect_no_error(Plot(list("thirdPlot" = a$e[["s"]]$lyr.1), new = TRUE))
  a$e[["s"]]$lyr.1[2] <- terra::minmax(a$e[["s"]]$lyr.1)[2]
  expect_no_error(Plot(list("thirdPlot" = a$e[["s"]]$lyr.1), new = TRUE))
  dev.off()
})

## block M
test_that("Plot functions NOT in quickPlot, i.e. redefining Plot", {
  testInit("terra", opts = list(quickPlot.verbose = TRUE), dev = FALSE)

  Plot <- function(x) {
    quickPlot::Plot(x)
  }

  clearPlot()
  expect_no_error(Plot(terra::rast(matrix(1:100, 10, 10))))

  try(dev.off())
})

test_that("Issue 20; arr working", {
  testInit("terra", opts = list(quickPlot.verbose = TRUE), dev = FALSE)

  files <- dir(system.file("maps", package = "quickPlot"), full.names = TRUE, pattern = "tif")
  maps <- lapply(files, rast)
  names(maps) <- lapply(maps, names)

  for (os in oses) {
    fil <- fn(tmpdir, desc, counter, os, envir = envirHere)
    announce_snapshot_file(name = basename(fil))
    if (correctOS(os))
      expect_snapshot_file({
        png(filename = fil, width = 800, height = 600)
        clearPlot()
        Plot(maps$DEM, maps$forestCover, maps$forestAge)
        Plot(maps$habitatQuality)  ## I get a 2 row 2 column layout
        Plot(maps$habitatQuality, arr = c(1, 4), new = TRUE) ## doesn't change the layout.
        dev.off()
        fil
      })
  }
})

test_that("Issue 32 Plot factors lower case id", {
  prevLastPlotNumber <- 51
  testInit("terra", opts = list(quickPlot.verbose = TRUE), dev = FALSE)

  for (colPalette in 1:2) {
    r <- rast(ncols = 3, nrows = 2, vals = 1:6)
    col <- if (colPalette == 1) rainbow(6, end = .9) else colorRampPalette(c("light green", "dark green"))(6)
    coltb <- data.frame(value = 1:6, col = col)
    coltab(r) <- coltb
    cls <- data.frame(id = 1:6, class = LETTERS[1:6])
    levels(r) <- cls

    for (os in oses) {
      fil <- fn(tmpdir, desc, counter, os, envir = envirHere)
      announce_snapshot_file(name = basename(fil))
      if (correctOS(os))
        expect_snapshot_file({
          png(filename = fil, width = 800, height = 600)
          clearPlot()
          Plot(r, new = TRUE)
          dev.off()
          fil
        })
    }
  }

  for (fn in 3) {
    r <- rast(ncols = 3, nrows = 2, vals = 1:6)
    cls <- data.frame(id = 1:6, class = LETTERS[1:6])
    levels(r) <- cls

    for (os in oses) {
      fil <- fn(tmpdir, desc, counter, os, envir = envirHere)
      announce_snapshot_file(name = basename(fil))
      if (correctOS(os))
        expect_snapshot_file({
          png(filename = fil, width = 800, height = 600)
          clearPlot()
          Plot(r, col = "Reds")
          dev.off()
          fil
        })
    }
  }
})
