test_that("Plot 1 is not error-free", {
  testInit("terra", opts = list(quickPlot.verbose = TRUE), dev = TRUE)
  on.exit(
    {
      if (length(dev.list()) > 0) dev.off()
    },
    add = TRUE) # nolint

  set.seed(1234)
  ras <- rast(
    xmin = 0, xmax = 10, ymin = 0, ymax = 10,
    vals = sample(1:4, replace = TRUE, size = 100), resolution = 1
  )
  objs <- list()
  objs$rasts$DEMs <- list(ras)

  names(objs$rasts$DEMs[[1]]) <- "DEM"

  objs$rasts$habQuals[[1]] <- rast(ras)
  objs$rasts$habQuals[[1]][] <- sample(1:10, replace = TRUE, size = 100)
  names(objs$rasts$habQuals[[1]]) <- "habQuality"

  objs$rasts$lands[[1]] <- c(objs$rasts$DEMs[[1]], objs$rasts$habQuals[[1]])

  objs$vects$caribous[[1]] <- terra::vect(
    type = "points",
    x = cbind(x = stats::runif(1e1, 0, 10), y = stats::runif(1e1, 0, 10))
  )

  x1 <- rbind(c(-180, -20), c(-140, 55), c(10, 0), c(-140, -60))
  x2 <- rbind(c(-10, 0), c(140, 60), c(160, 0), c(140, -55))
  x3 <- rbind(c(-125, 0), c(0, 60), c(40, 5), c(15, -45))
  hole <- rbind(c(80, 0), c(105, 13), c(120, 2), c(105, -13))
  z <- rbind(
    cbind(object = 1, part = 1, x1, hole = 0), cbind(object = 2, part = 1, x3, hole = 0),
    cbind(object = 3, part = 1, x2, hole = 0), cbind(object = 3, part = 1, hole, hole = 1)
  )
  colnames(z)[3:4] <- c("x", "y")

  objs$vects$polys[[1]] <- vect(z, "polygons")

  # Test polygon with > 1e3 points to test the speedup parameter
  r <- 1
  N <- 1100
  cx <- 0
  cy <- 0
  a <- seq(0, 2 * pi, length.out = N)
  x <- cx + r * cos(a)
  y <- cy + r * sin(a)
  objs$vects$polysLrg[[1]] <- vect(cbind(object = 1, x, y), "polygons")

  # test SpatialLines
  l1 <- cbind(c(10, 2, 30), c(30, 2, 2))
  l1a <- cbind(l1[, 1] + .05, l1[, 2] + .05)
  l2 <- cbind(c(1, 20, 3), c(10, 1.5, 1))

  obj <- cbind(
    object = c(rep(1, NROW(l1)), rep(2, NROW(l1a)), rep(3, NROW(l2))),
    rbind(rbind(l1, l1a), l2)
  )
  objs$vects$lins[[1]] <- terra::vect(obj, "lines")


  # Test points with > 1e3 points to test the speedup parameter
  # test speedup
  objs$vects$caribousLrg[[1]] <- terra::vect(cbind(
    x = stats::runif(N, 0, 10), y = stats::runif(N, 0, 10)
  ))

  # If any rearrangements are required, Plot searches for objects in Global Env
  # So all tests must run a clearPlot or a new = TRUE to be cleared to
  # prevent rearrangements
  clearPlot()
  expect_error(Plot(asdfd))

  if (requireNamespace("raster", quietly = TRUE)) {
    objs$rasts$DEMs[[2]] <- raster::raster(objs$rasts$DEMs[[1]])
    objs$rasts$habQuals[[2]] <- raster::raster(objs$rasts$habQuals[[1]])
    objs$rasts$lands[[2]] <- raster::stack(objs$rasts$lands[[1]])
  }
  if (requireNamespace("sp", quietly = TRUE)) {
    objs$vects$lins[[2]] <- as(objs$vects$lins[[1]], "Spatial")
    objs$vects$polys[[2]] <- as(objs$vects$polys[[1]], "Spatial")
    objs$vects$caribous[[2]] <- as(objs$vects$caribous[[1]], "Spatial")
    objs$vects$polysLrg[[2]] <- as(objs$vects$polysLrg[[1]], "Spatial")
    objs$vects$caribousLrg[[2]] <- as(objs$vects$caribousLrg[[1]], "Spatial")
  }

  for (i in seq_along(objs$rasts)) {
    car <- sample(objs$vects$caribous, 1)[[1]]
    land <- sample(objs$rasts$lands, 1)[[1]]
    DEM <- sample(objs$rasts$DEMs, 1)[[1]]
    hab <- sample(objs$rasts$habQuals, 1)[[1]]
    SpP <- sample(objs$vects$polys, 1)[[1]]
    polyLrg <- sample(objs$vects$polysLrg, 1)[[1]]
    Lin <- sample(objs$vects$lins, 1)[[1]]

    clearPlot()
    expect_no_error(Plot(land))
    clearPlot()
    expect_no_error(Plot(car))
    # Test speedup > 0.1 for SpatialPoints
    clearPlot()
    expect_no_error(Plot(car, speedup = 2))
    clearPlot()
    expect_no_error(Plot(land))
    # can add a plot to the plotting window
    expect_no_error(Plot(car, new = FALSE))
    clearPlot()
    # Can add two maps with same name, if one is in a stack; they are given
    #  unique names based on object name
    expect_no_error(Plot(land, car, DEM))
    # can mix stacks, rasters, SpatialPoint*
    clearPlot()
    expect_no_error(Plot(land, hab, car))
    # can mix stacks, rasters, SpatialPoint*, and SpatialPolygons*
    clearPlot()
    expect_no_error(Plot(land, car))


    clearPlot()
    expect_no_error(Plot(SpP))
    clearPlot()

    expect_no_error(Plot(land, car, SpP, new = TRUE)) |>
      capture_messages()

    clearPlot()
    expect_no_error(Plot(SpP))
    clearPlot()
    expect_no_error(Plot(land, car, SpP, new = TRUE))

    expect_no_error(Plot(polyLrg, new = TRUE))
    expect_no_error(Plot(Lin))
  }

  M <- 2
  if (requireNamespace("sp", quietly = TRUE)) {
    polys1 <- lapply(seq(M), function(m) {
      N <- 20
      adds <- rep(1:N, each = 4)
      x <- rep((c(0, 0, 1, 1) + m) * N, N)
      y <- rep(c(0, 1, 1, 0), N) + adds
      polyNum <- adds
      coords1 <- cbind(x, y, polyNum)
      polys1 <- sp::Polygons(by(coords1, polyNum, function(coo) {
        list(sp::Polygon(coo[, 1:2]))
      }), paste0("ss", m)) # [], paste0("s", unique(coo[, 3])))

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
      expect_true(sum(grepl("Incorrect", mess)) == 1)
    }
  }

  for (i in seq_along(objs$vects$lins)) {
    car <- sample(objs$vects$caribous, 1)[[1]]
    car2 <- sample(objs$vects$caribousLrg, 1)[[1]]
    land <- sample(objs$rasts$lands, 1)[[1]]
    DEM <- sample(objs$rasts$DEMs, 1)[[1]]
    hab <- sample(objs$rasts$habQuals, 1)[[1]]
    Sp <- sample(objs$vects$polys, 1)[[1]]
    polyLrg <- sample(objs$vects$polysLrg, 1)[[1]]
    Lin <- sample(objs$vects$lins, 1)[[1]]

    expect_no_error(Plot(land, new = TRUE))
    expect_no_error(Plot(Lin, new = TRUE))
    expect_no_error(Plot(DEM, addTo = "land$habQuality"))
    expect_no_error(Plot(Sp, addTo = "land$habQuality"))
    # test various arguments
    clearPlot()
    expect_no_error(Plot(car, new = TRUE, gpAxis = gpar(cex = 0.4), size = 1))
    clearPlot()
    expect_no_error(Plot(DEM, gpText = gpar(cex = 0.4)))
    # test colours
    clearPlot()
    expect_no_error(Plot(DEM, cols = c("blue", "red")))
    # Should work with col as well as cols
    clearPlot()
    expect_no_error(Plot(DEM, col = c("blue", "red")))
    # test visualSqueeze
    clearPlot()
    expect_no_error(Plot(DEM, visualSqueeze = 0.2, new = TRUE))
    clearPlot()
    expect_no_error(Plot(car2, speedup = 10, new = TRUE))
  }

  # test ggplot2 and hist -- don't work unless invoke global environment
  clearPlot()
  if (!isRstudioServer())
    dev()
  hist87654 <- hist(stats::rnorm(1e3), plot = FALSE)
  clearPlot()
  expect_no_error(Plot(hist87654))

  # test ggplot2 and hist -- don't work unless invoke global environment
  clearPlot()

  if (requireNamespace("ggplot2", quietly = TRUE)) {
    suppressWarnings(
      ggplot87654 <- ggplot2::qplot(stats::rnorm(1e3), binwidth = 0.3, geom = "histogram")
    ) # warning is about deprecation
    expect_no_error(Plot(ggplot87654))
  }

  for (i in seq_along(objs$vects$lins)) {
    car <- sample(objs$vects$caribous, 1)[[1]]
    car2 <- sample(objs$vects$caribousLrg, 1)[[1]]
    land <- sample(objs$rasts$lands, 1)[[1]]
    DEM <- sample(objs$rasts$DEMs, 1)[[1]]
    hab <- sample(objs$rasts$habQuals, 1)[[1]]
    Sp <- sample(objs$vects$polys, 1)[[1]]
    polyLrg <- sample(objs$vects$polysLrg, 1)[[1]]
    Lin <- sample(objs$vects$lins, 1)[[1]]

    # test rearrangements
    expect_no_error(Plot(car, new = TRUE))
    expect_no_error(Plot(DEM))
    expect_no_error(Plot(land))

    testPlot <- Plot(land)
    expect_no_error(Plot(testPlot))
    expect_no_error(Plot(car, addTo = "DEM"))
    expect_no_error(rePlot())
  }
})
