test_that("Plotting types without visual checking works", {
  testInit("terra")
  r1 <- terra::rast(xmin = 0, xmax = 10, ymin = 0, ymax = 10,
                       vals = sample(1:4, replace = TRUE, size = 100), res = 1)
  r2 <- terra::rast(xmin = 0, xmax = 10, ymin = 0, ymax = 10,
                       vals = sample(1:4, replace = TRUE, size = 100), res = 1)
  s1 <- terra::rast(c(layer.1 = r1, layer.2 = r2))
  sp1 <- terra::vect(cbind(x = stats::runif(10, 0, 10),
                                 y = stats::runif(10, 0, 10)))
  if (interactive()) {
    clearPlot()

    expect_silent(Plot(sp1))
    a <- .getQuickPlot(paste0("quickPlot", dev.cur()))
    expect_true(length(a$isBaseLayer) == 1)
    expect_true(length(a$curr@quickPlotGrobList) == 1)

    expect_silent(Plot(r1, axes = TRUE, new = TRUE))
    a <- .getQuickPlot(paste0("quickPlot", dev.cur()))
    expect_true(length(a$isBaseLayer) == 2)
    expect_true(length(a$curr@quickPlotGrobList) == 2)
    expect_true(length(a$curr@quickPlotGrobList$r1) == 1)

    expect_silent(Plot(sp1, addTo = "r1"))
    a <- .getQuickPlot(paste0("quickPlot", dev.cur()))
    expect_true(length(a$isBaseLayer) == 2)
    expect_true(length(a$curr@quickPlotGrobList) == 2)
    expect_true(length(a$curr@quickPlotGrobList$r1) == 2)

    expect_silent(Plot(r2))
    a <- .getQuickPlot(paste0("quickPlot", dev.cur()))
    expect_true(length(a$isBaseLayer) == 3)
    expect_true(length(a$curr@quickPlotGrobList) == 3)
    expect_true(length(a$curr@quickPlotGrobList$r2) == 1)

    expect_silent(Plot(s1))
    a <- .getQuickPlot(paste0("quickPlot", dev.cur()))
    expect_true(length(a$isBaseLayer) == 5)
    expect_true(length(a$curr@quickPlotGrobList) == 5)
    expect_true(length(a$curr@quickPlotGrobList$`s1$layer.1`) == 1)

    expect_silent(Plot(sp1, addTo = "s1$layer.1"))
    a <- .getQuickPlot(paste0("quickPlot", dev.cur()))
    expect_true(length(a$isBaseLayer) == 5)
    expect_true(length(a$curr@quickPlotGrobList) == 5)
    expect_true(length(a$curr@quickPlotGrobList$`s1$layer.1`) == 2)

    s1$layer.1[3] <- 15
    s1$layer.2[3] <- 25

    expect_silent(Plot(s1$layer.1)) # doesn't change s1$layer.2
    expect_silent(Plot(s1$layer.2)) # doesn't change s1$layer.1

    expect_silent(rePlot())
    a <- .getQuickPlot(paste0("quickPlot", dev.cur()))
    expect_true(length(a$isBaseLayer) == 5)
    expect_true(length(a$curr@quickPlotGrobList) == 5)
    expect_true(length(a$curr@quickPlotGrobList$`s1$layer.1`) == 2)
    expect_true(length(a$curr@quickPlotGrobList$r1) == 2)
  }
})

test_that("setColors with an NA", {
  withr::local_package("terra")

  nLevels <- 6
  ncol <- 3
  nrow <- 4
  N <- ncol * nrow
  set.seed(24334)
  levs <- (1:nLevels)[-((nLevels - 2):(nLevels - 1))] # nolint
  ras <- terra::rast(matrix(sample(levs, size = N, replace = TRUE),
                       ncol = ncol, nrow = nrow))
  ras[1] <- NA
  levels(ras) <- data.frame(ID = levs, Class = paste0("Level", levs))
  expect_silent({
    ras <- setColors(ras, n = 4, value = c("red", "orange", "blue", "yellow"))
  })
})


