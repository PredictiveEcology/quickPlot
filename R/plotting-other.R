### deal with spurious data.table warnings
if (getRversion() >= "3.1.0") {
  utils::globalVariables(c(".", "..xyCols"))
}

################################################################################
#' Clear plotting device
#'
#' Under some conditions, a device and its metadata need to be cleared manually.
#' This can be done with either the `new = TRUE` argument within the call to `Plot`.
#' Sometimes, the metadata of a previous plot will prevent correct plotting of
#' a new `Plot` call.
#' Use `clearPlot` to clear the device and all the associated metadata manually.
#'
#' @param dev Numeric. Device number to clear.
#'
#' @param removeData Logical indicating whether any data that was stored in the
#' `.quickPlotEnv` should also be removed; i.e., not just the plot window wiped.
#'
#' @param force Logical or "all". Sometimes the graphics state cannot be fixed by a simple
#'              `clearPlot()`. If TRUE, this will close the device and reopen the same
#'              device number. If "all", then all `quickPlot` related data from all devices
#'              will be cleared, in addition to device closing and reopening.
#'
#' @author Eliot McIntire
#' @export
#' @inheritParams Plot
#' @importFrom grDevices dev.cur dev.off
#' @importFrom grid grid.newpage
#' @include plotting-classes.R
#' @rdname clearPlot
#' @seealso [Plot()].
#'
#' @examples
#' if (interactive()) {
#'   Plot(1:10)
#'   clearPlot() # clears
#' }
#'
setGeneric("clearPlot", function(dev = dev.cur(), removeData = TRUE, force = FALSE,
                                 verbose = getOption("quickPlot.verbose")) {
  standardGeneric("clearPlot")
})

#' @export
#' @rdname clearPlot
setMethod(
  "clearPlot",
  signature = c("numeric", "logical", "ANY"),
  definition = function(dev, removeData, force,
                        verbose = getOption("quickPlot.verbose")) {

    suppressWarnings(
      try(rm(list = paste0("quickPlot", dev), envir = .quickPlotEnv))
    )

    if (removeData) {
      suppressWarnings(
        try(rm(list = paste0("basePlots_", dev), envir = .quickPlotEnv))
      )
      suppressWarnings(
        try(rm(list = paste0("Dev", dev), envir = .quickPlotEnv))
      )

      suppressWarnings(
        try(rm(list = ls(.quickPlotEnv[[paste0("dev", dev)]]),
               envir = .quickPlotEnv[[paste0("dev", dev)]]), silent = TRUE)
      )
    }
    if (!identical(FALSE, force)) {
      if (force == "all") {
        rm(list = ls(.quickPlotEnv), envir = .quickPlotEnv)
      }
      dc <- dev.cur()
      dev.off()
      dev(dc, verbose = verbose)
      return(invisible())
    }
    devActive <- dev.cur()
    if (devActive == 1) return(invisible())
    dev(dev)
    grid.newpage()
    plot.new()
    par(.quickPlotEnv$.parOrig)
    dev(devActive)
  }
)

#' @export
#' @rdname clearPlot
setMethod("clearPlot",
          signature = c("numeric", "missing", "ANY"),
          definition = function(dev, force) {
            clearPlot(dev, removeData = TRUE, force = force)
          })

#' @export
#' @rdname clearPlot
setMethod("clearPlot",
          signature = c("missing", "logical", "ANY"),
          definition =  function(removeData, force) {
            clearPlot(dev = dev.cur(), removeData = removeData, force = force)
          })

#' @export
#' @rdname clearPlot
setMethod("clearPlot",
          signature = c("missing", "missing"),
          definition =  function(dev, removeData, force) {
            clearPlot(dev.cur(), removeData = TRUE, force = force)
          })

################################################################################
#' Convert `grid.locator` units
#'
#' Internal function from example in `?grid.locator`.
#' Converts `grid.locator` units to meaningful units.
#' Used within `.clickCoord`
#'
#' @param grid.locator an object that was output by a call to `grid.locator`
#'                     and mouse click(s).
#'
#' @export
#' @keywords internal
#' @rdname unittrim
#' @author Paul Murrell
#'
.unittrim <- function(grid.locator) {
  as.numeric(sub("^([0-9]+|[0-9]+[.][0-9])[0-9]*", "\\1",
                 as.character(grid.locator)))
}

################################################################################
#' Mouse interactions with Plots
#'
#' These functions use `grid.locator`. The primary two user-level functions are
#' `clickValues` and `clickExtent`. These functions automatically select
#' the correct viewport (i.e., map) where the mouse clicks occurred so the user
#' does not have to manually specify which map is being clicked on.
#' This works for `Raster*`, `SpatialPoints*`, and `SpatialPolygons*` objects.
#'
#' `clickValues` is equivalent to running `X[SpatialPoints(locator(n))]`, where
#' X is the raster being clicked on, in base graphics. This function determines which place in the
#' grid.layout was clicked and makes all appropriate calculations to determine the value
#' on the raster(s) at that or those location(s). It should be noted that when zooming in
#' to rasters, plotting of rasters will only allow for complete pixels to be plotted, even
#' if the extent is not perfectly in line with pixel edges. As a result, when values
#' returned by this function may be slightly off (<0.5 pixel width).
#'
#' `clickExtent` is for drawing an extent with two mouse clicks on a given Plotted map.
#'
#' `clickCoordinates` is the workhorse function that determines which plot has been
#' clicked on and passes this plot name and the clicked coordinates to `.clickCoord`.
#'
#' `.clickCoord` is intended for internal use and is called by other functions here.
#'
#' @param n The number of mouse clicks to do.
#'
#' @return `clickValues` returns the layer names and values at the clicked points.
#' `clickExtent` invisibly returns the extent object, and optionally plots
#' it in a new device window.
#' `clickCoordinates` returns the xy coordinates in the units of the plot clicked on.
#'
#' @author Eliot McIntire
#' @export
#' @include plotting-classes.R
#' @rdname quickPlotMouseClicks
#'
#' @examples
#' \donttest{
#' # clickValues and family are unreliable on Rstudio Server as the plotting device
#' #   does not report its dimensions correctly; this may change in future
#' #   updates to Rstudio
#' if (interactive() && !isRstudioServer() ) {
#'   files <- system.file("maps", package = "quickPlot")
#'   files <- dir(files, full.names = TRUE, pattern = "tif")
#'   maps <- lapply(files, function(x) terra::rast(x))
#'   names(maps) <- sapply(basename(files), function(x) {
#'     strsplit(x, split = "\\.")[[1]][1]
#'   })
#'   landscape <- c(maps$DEM, maps$forestCover, maps$habitatQuality)
#'
#'   clearPlot()
#'   Plot(landscape)
#'   clickValues(3) # click at three locations on the Plot device
#'
#'   clearPlot()
#'   Plot(landscape)
#'   e <- clickExtent() # click at two locations on the Plot device
#'   print(e)
#'
#'   # repeated zooming to try various places on the original device
#'   for(i in 1:4) clickExtent() # click at two locations on the Plot device
#' }
#' }
#'
clickValues <- function(n = 1) {
  coords <- clickCoordinates(n = n)
  setDT(coords$coords)
  objLay <- strsplit(coords$map, "\\$")
  objNames <- unlist(lapply(objLay, function(x) x[1]))
  layNames <- unlist(lapply(objLay, function(x) x[2]))
  for (i in 1:n) {
    ras1 <- eval(parse(text = objNames[i]), envir = coords$envir[[i]])
    xyCols <- c("x", "y")
    if (isSF(ras1) || isSpat(ras1)) {
      crds <- coords$coords[i, ..xyCols]
      if (isSF(ras1)) {
        a <- sf::st_sfc(sf::st_point(as.matrix(crds)))
        sf::st_crs(a) <- sf::st_crs(ras1)
        b <- sf::st_intersection(ras1, a)
        d <- sf::st_drop_geometry(b)
      } else {
        a <- terra::vect(crds, geom = xyCols)
        b <- terra::extract(ras1, a)
        d <- as.data.frame(b)
      }
      df <- cbind(crds, cbind("plot" = objNames[i], d))
      cols <- intersect(colnames(coords$coords), c("x", "y"))
      otherCols <- setdiff(colnames(df), cols)
      coords$coords[df, (otherCols) := lapply(otherCols, function(x) get(paste0("i.", x))), on = cols]

    } else {
      if (!is.na(layNames[i])) {
        coords$coords$value <- unlist(lapply(seq_len(n), function(i) {
          ras1[[layNames[i]]][terra::cellFromXY(ras1[[layNames[i]]], coords$coords[i, 1:2])]
        }))
      } else {
        coords$coords$value <- unlist(lapply(seq_len(n), function(i) {
          ras1[terra::cellFromXY(ras1, coords$coords[i, 1:2])]
        }))
      }
    }

  }
  if (any(terra::is.factor(ras1))) {
    for (i in which(terra::is.factor(ras1)))
      coords$coords$value <- factorValues(ras1[[i]], coords$coords$value)
  }
  return(coords$coords[])
}

#' @param devNum The device number for the new plot to be plotted on.
#'
#' @param plot.it Logical. If `TRUE` a new plotting window is made for the
#'                new extent. Default `TRUE`.
#'
#' @export
#' @importFrom grDevices dev.cur
#' @importFrom fpCompare %==%
#' @inheritParams Plot
#' @include plotting-classes.R
#' @rdname quickPlotMouseClicks
#'
#' @details
#' `clickExtent` will place the new, zoomed in plot over top of the existing
#' object. To recover original full object, double click anywhere during an
#' active `clickExtent`. Currently, subsequent `clickExtent` is a click on the
#' original map extent, not the "zoomed in" map extent. See example.
#'
clickExtent <- function(devNum = NULL, plot.it = TRUE,
                        verbose = getOption("quickPlot.verbose")) {
  corners <- clickCoordinates(2)
  zoom <- terra::ext(c(sort(corners[[3]]$x), sort(corners[[3]]$y)))

  if (plot.it) {

    objLay <- strsplit(corners$map, "\\$")
    objNames <- unique(unlist(lapply(objLay, function(x) x[1])))
    layNames <- unique(unlist(lapply(objLay, function(x) x[2])))

    devActive <- dev.cur()
    if (!(is.null(devNum))) {
      dev(devNum, verbose = verbose)
    }

    obj <- list()
    if (!is.na(layNames)) {
      theObj <- eval(parse(text = objNames), envir = corners$envir[[1]])[[layNames]]
      theName <- paste0(objNames, "$", layNames)
    } else {
      theObj <- get(objNames, envir = corners$envir[[1]])
      theName <- objNames
    }
    theObj <- list(theObj)
    names(theObj) <- objNames
    if (sum(corners$coords[1,] - corners$coords[2,]) %==% 0) {
      Plot(theObj, addTo = theName, title = theName, new = TRUE)
    } else {
      Plot(theObj, addTo = theName, title = theName, zoomExtent = zoom, new = TRUE)
    }

    if (!(is.null(devNum))) {
      dev(devActive, verbose = verbose)
    }
    return(invisible(zoom))
  } else {
    return(zoom)
  }
}

#' @export
#' @importFrom grDevices dev.cur
#' @importFrom grid grid.layout grid.locator unit
#' @include environment.R
#' @include plotting-classes.R
#' @rdname quickPlotMouseClicks
clickCoordinates <- function(n = 1) {
  dc <- dev.cur()

  arr <- try(.getQuickPlot(paste0("quickPlot", dc)))
  if (inherits(arr, "try-error")) {
    stop(paste("Plot does not already exist on current device.",
               "clearPlot() or change device to",
               "one that has objects from a call to Plot()."))
  }
  gl <- grid.layout(nrow = arr$curr@arr@rows * 3 + 2,
                    ncol = arr$curr@arr@columns * 3 + 2,
                    widths = arr$curr@arr@layout$wdth,
                    heights = arr$curr@arr@layout$ht)

  grepNullsW <- grep("null$", gl$widths)
  grepNpcsW <- grep("npc$", gl$widths)

  # remove pipe --> keep compatible with old R, without requiring magrittr
  nulls <- as.character(gl$widths)[grepNullsW]# |>
  nulls <- strsplit(x = nulls, "null") #|>
  nulls <- unlist(nulls) #|>
  nulls <- as.numeric(nulls)

  npcs <- as.character(gl$widths)[grepNpcsW]
  npcs <- strsplit(x = npcs, "npc")
  npcs <- unlist(npcs)
  npcs <- as.numeric(npcs)

  remaining <- 1 - sum(npcs)
  npcForNulls <- nulls * remaining / sum(nulls)
  widthNpcs <- c(npcs, npcForNulls)[order(c(grepNpcsW, grepNullsW))]

  grepNullsH <- grep("null$", gl$heights)
  grepNpcsH <- grep("npc$", gl$heights)

  nulls <- as.character(gl$heights)[grepNullsH]
  nulls <- strsplit(x = nulls, "null")
  nulls <- unlist(nulls)
  nulls <- as.numeric(nulls)

  npcs <- as.character(gl$heights)[grepNpcsH]
  npcs <- strsplit(x = npcs, "npc")
  npcs <- unlist(npcs)
  npcs <- as.numeric(npcs)
  remaining <- 1 - sum(npcs)
  npcForNulls <- nulls * remaining / sum(nulls)
  heightNpcs <- c(npcs, npcForNulls)[order(c(grepNpcsH, grepNullsH))]

  clickCoords <- data.frame(x = NA_real_, y = NA_real_, stringsAsFactors = FALSE)
  mapNames <- character(n)
  envs <- list()

  grobLoc <- list()

  if (isRstudioDevice() && isWindows())
    warning(RstudioDeviceWarning())

  for (i in 1:n) {
    seekViewport("top", recording = FALSE)
    gloc <- grid.locator(unit = "npc")
    if (isRstudioDevice()) {
      if (isWindows())
        gloc$y <- grid::unit(as.numeric(gloc$y) + 0.2, "npc")
    }
    xInt <- findInterval(as.numeric(strsplit(as.character(gloc$x), "npc")[[1]]),
                         c(0, cumsum(widthNpcs)))
    # for the y, grid package treats bottom left as origin, Plot treats top left
    #  as origin... so, require 1-
    yInt <- findInterval(as.numeric(strsplit(as.character(gloc$y), "npc")[[1]]),
                         c(0, cumsum(heightNpcs)))
    if (!(xInt %in% grepNpcsW) || !(yInt %in% grepNpcsH)) {
      stop("No plot at those coordinates")
    }
    column <-  which(xInt == grepNpcsW)
    row <- which((yInt == grepNpcsH)[length(grepNpcsH):1]) # nolint

    if (length(row) == 0) { # above the plot
      row <- if (yInt > grepNpcsH) length(grepNpcsH) else 1
    }
    if (length(column) == 0) { # above the plot
      column <- if (xInt > grepNpcsW) length(grepNpcsW) else 1
    }

    map <- column + (row - 1) * arr$curr@arr@columns

    maxLayX <- cumsum(widthNpcs)[xInt]
    minLayX <- cumsum(widthNpcs)[xInt - 1]
    grobLoc$x <- unit(
      (as.numeric(strsplit(
        as.character(gloc$x), "npc"
      )[[1]]) - minLayX) / (maxLayX - minLayX), "npc")

    maxLayY <- cumsum(heightNpcs)[yInt]
    minLayY <- cumsum(heightNpcs)[yInt - 1]
    grobLoc$y <- unit(
      (as.numeric(strsplit(
        as.character(gloc$y), "npc"
      )[[1]]) - minLayY) / (maxLayY - minLayY), "npc")

    clickCoords[i, ] <- .clickCoord(arr$curr@quickPlotGrobList[[map]][[1]]@plotName,
                                    n = 1, gl = grobLoc)
    mapNames[i] <- arr$curr@quickPlotGrobList[[map]][[1]]@plotName
    envs[[i]] <- arr$curr@quickPlotGrobList[[map]][[1]]@envir
  }
  return(list(map = mapNames, envir = envs, coords = clickCoords))
}

#' @param X The raster object whose values will be returned where mouse clicks occur.
#'
#' @param gl An object created by a call to `grid.locator`.
#'
#' @export
#' @importFrom grid seekViewport grid.locator convertX convertY
#' @include plotting-classes.R
#' @keywords internal
#' @rdname quickPlotMouseClicks
.clickCoord <- function(X, n = 1, gl = NULL) { # nolint
  pts <- data.frame(x = NA_real_, y = NA_real_, stringsAsFactors = FALSE)
  seekViewport(X, recording = FALSE)
  for (i in 1:n) {
    if (is.null(gl)) {
      gl <- grid.locator()
      pts[i, ] <- .unittrim(gl)
    } else {
      pts[i, ] <- c(convertX(gl$x, "native"), convertY(gl$y, "native"))
    }
  }
  return(pts)
}

################################################################################
#' Specify where to plot
#'
#' Switch to an existing plot device, or if not already open,
#' launch a new graphics device based on operating system used.
#' On Windows and macOS, if `x` is not provided, this will open or switch to the first
#' non-RStudio device, which is much faster than the \file{png}-based RStudio plot device.
#' Currently, this will not open anything new.
#'
#' For example, `dev(6)` switches the active plot device to device 6.
#' If it does not exist, it opens it. If devices 1-5 don't exist they will be opened too.
#'
#' @param x   The number of a plot device. If missing, will open a new
#'            non-RStudio plotting device
#'
#' @param ... Additional arguments passed to [newPlot()].
#'
#' @return Opens a new plot device on the screen.
#' Invisibly returns the device number selected.
#'
#' @author Eliot McIntire and Alex Chubaty
#' @export
#' @inheritParams Plot
#' @importFrom grDevices dev.list dev.set
#' @include plotting-classes.R
#' @rdname dev
#'
#' @examples
#' \dontrun{
#'   dev(4)
#' }
#'
dev <- function(x, ..., verbose = getOption("quickPlot.verbose")) {
  if (missing(x)) {
    xMissing <- TRUE
  } else if (is.infinite(x)) {
    xMissing <- TRUE
  } else {
    xMissing <- FALSE
  }
  if (xMissing) {
    if (is.null(dev.list())) {
      x <- 2L
    } else {
      namesDevList <- names(dev.list())
      isRstudioDev <- namesDevList == "RStudioGD"
      hasAPngForStudio <- (identical(namesDevList[min(length(namesDevList), which(isRstudioDev) + 1)], "png"))
      if (any(isRstudioDev)) {
        x <- min(min(dev.list()) + 1 + hasAPngForStudio,
                 which(isRstudioDev) + 3L)
        dev(x, verbose = verbose)
      } else {
        x <- min(dev.list())
        dev(x, verbose = verbose)
      }
    }
  }
  if (identical(getOption("device"), "RStudioGD")) {
    if (.Platform$OS.type == "unix" && !isRstudioServer()) {
      messageVerbose("setting graphics device away from Rstudio device. To return to Rstudio device: dev.useRSGD(TRUE)",
                     verbose = verbose)
      dev.useRSGD(FALSE)
    }
  }

  if (is.null(dev.list())) {
    newPlot(...)
  } else {
    while (dev.set(x) < x && !isRstudioServer()) newPlot(...)
  }
  return(invisible(dev.cur()))
}

################################################################################
#' Open a new plotting window
#'
#' @param noRStudioGD Logical Passed to `dev.new`. Default is TRUE to avoid using
#'                    RStudio graphics device, which is slow.
#'
#' @param useRSGD     Logical indicating whether the default device should be the
#'                    RStudio graphic device, or the platform default (`quartz`
#'                    on macOS; `windows` on Windows; `x11` on others, e.g., Linux).
#'
#' @param ...         Additional arguments.
#'
#' @note [dev.new()] is supposed to be the correct way to open a new
#' window in a platform-generic way; however, does not work in RStudio
#' ([SpaDES#116](https://github.com/PredictiveEcology/SpaDES/issues/116)).
#' Use `dev.useRSGD(FALSE)` to avoid RStudio for the remainder of this session,
#' and `dev.useRSGD(TRUE)` to use the RStudio graphics device.
#' (This sets the default device via the `device` option.)
#'
#' @seealso [dev()].
#'
#' @author Eliot McIntire and Alex Chubaty
#' @export
#' @importFrom grDevices dev.new
#' @inheritParams Plot
#' @rdname newPlot
#'
#' @examples
#' \dontrun{
#'   ## set option to avoid using Rstudio graphics device
#'   dev.useRSGD(FALSE)
#'
#'   ## open new plotting window
#'   newPlot()
#' }
#'
newPlot <- function(noRStudioGD = TRUE, ..., verbose = getOption("quickPlot.verbose")) {
  if (isRstudioServer()) {
    noRStudioGD <- FALSE
    messageVerbose("Using Rstudio server; not opening a new window", verbose = verbose)
    dev.useRSGD(TRUE)
  }

  dev.new(noRStudioGD = noRStudioGD, ...)
}

#' @export
#' @rdname newPlot
dev.useRSGD <- function(useRSGD = FALSE) { # nolint
  if (isTRUE(useRSGD)) {
    options(device = "RStudioGD")
  } else {
    if (Sys.info()["sysname"] == "Darwin")
      options(device = "quartz")
    else if (Sys.info()["sysname"] == "Windows")
      options(device = "windows")
    else (Sys.info()["sysname"] == "Linux")
    options(device = "x11")
  }
}

#' Determine if current session is RStudio Server
#'
#' @export
#' @examples
#' isRstudioServer() # returns FALSE or TRUE
isRstudioServer <- function() {
  isRstudioServer <- FALSE

  if (isTRUE("tools:rstudio" %in% search())) { ## running in Rstudio
    rsAPIFn <- get(".rs.api.versionInfo", as.environment("tools:rstudio"))
    versionInfo <- rsAPIFn()
    if (!is.null(versionInfo)) {
      isRstudioServer <- identical("server", versionInfo$mode)
    }
  }
  isRstudioServer
}

#' Default plotting parameters
#'
#' @keywords internal
#' @name .parOrig
#' @rdname parOrig
assign(
  ".parOrig",
  envir = .quickPlotEnv,
  structure(list(xlog = FALSE, ylog = FALSE, adj = 0.5, ann = TRUE,
                 ask = FALSE, bg = "white", bty = "o", cex = 1, cex.axis = 1,
                 cex.lab = 1, cex.main = 1.2, cex.sub = 1, col = "black",
                 col.axis = "black", col.lab = "black", col.main = "black",
                 col.sub = "black", crt = 0, err = 0L, family = "", fg = "black",
                 fig = c(0.5, 0.9866, 0.0233, 0.875),
                 fin = c(5.00285625, 2.155865625),
                 font = 1L, font.axis = 1L, font.lab = 1L, font.main = 2L,
                 font.sub = 1L, lab = c(5L, 5L, 7L), las = 0L, lend = "round",
                 lheight = 1, ljoin = "round", lmitre = 10, lty = "solid",
                 lwd = 1, mai = c(1.02, 0.82, 0.82, 0.42),
                 mar = c(5.1, 4.1, 4.1, 2.1), mex = 1, mfcol = c(1L, 1L),
                 mfg = c(1L, 1L, 1L, 1L), mfrow = c(1L, 1L), mgp = c(3, 1, 0),
                 mkh = 0.001, new = FALSE,
                 oma = c(0, 0, 0, 0), omd = c(0, 1, 0, 1), omi = c(0, 0, 0, 0),
                 pch = 1L, pin = c(3.6020565, 1.293519375),
                 plt = c(0.23, 0.95, 0.3, 0.9), ps = 12L, pty = "m",
                 smo = 1, srt = 0, tck = NA_real_,
                 tcl = -0.5, usr = c(0.64, 10.36, -1.74682466270393, 0.852684557824307
                 ), xaxp = c(2, 10, 4), xaxs = "r", xaxt = "s", xpd = FALSE,
                 yaxp = c(-1.5, 0.5, 4), yaxs = "r", yaxt = "s", ylbias = 0.2),
            .Names = c("xlog", "ylog", "adj", "ann", "ask", "bg", "bty", "cex", "cex.axis",
                       "cex.lab", "cex.main", "cex.sub", "col", "col.axis", "col.lab",
                       "col.main", "col.sub", "crt", "err", "family", "fg", "fig", "fin",
                       "font", "font.axis", "font.lab", "font.main", "font.sub", "lab",
                       "las", "lend", "lheight", "ljoin", "lmitre", "lty", "lwd", "mai",
                       "mar", "mex", "mfcol", "mfg", "mfrow", "mgp", "mkh", "new", "oma",
                       "omd", "omi", "pch", "pin", "plt", "ps", "pty", "smo", "srt",
                       "tck", "tcl", "usr", "xaxp", "xaxs", "xaxt", "xpd", "yaxp", "yaxs",
                       "yaxt", "ylbias")
  ))

isRstudioDevice <- function(dev = dev.cur()) {
  namesDevList <- names(dev)
  isRstudioDev <- namesDevList == "RStudioGD"
  return(isRstudioDev)
}

RstudioDeviceWarning <- function()
  paste0("Rstudio device may give inappropriate coordinates; values may be wrong. ",
          "Try using an external device?",
          "\nrePlot(toDev = dev(noRStudioGD = TRUE))")

SysInfo <-   Sys.info()

isWindows <- function() {
  tolower(SysInfo["sysname"]) == "windows"
}

isMacOSX <- function() {
  tolower(SysInfo["sysname"]) == "darwin"
}

isLinux <- function() {
  tolower(SysInfo["sysname"]) == "linux"
}
