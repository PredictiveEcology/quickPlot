################################################################################
#' Clear plotting device
#'
#' Under some conditions, a device and its metadata need to be cleared manually.
#' This can be done with either the \code{new = TRUE} argument within the call to
#' \code{Plot}.
#' Sometimes, the metadata of a previous plot will prevent correct plotting of
#' a new \code{Plot} call.
#' Use \code{clearPlot} to clear the device and all the associated metadata
#' manually.
#'
#' @param dev Numeric. Device number to clear.
#'
#' @param removeData Logical indicating whether any data that was stored in the
#' \code{.quickPlotEnv} should also be removed; i.e., not just the plot window wiped.
#'
#' @param force Logical or "all". Sometimes the graphics state cannot be fixed by a simple
#'              clearPlot(). If TRUE, this will close the device and reopen the same
#'              device number. If "all", then all quickPlot related data from all devices
#'              will be cleared, in addition to device closing and reopening.
#'
#' @author Eliot McIntire
#' @export
#' @importFrom grDevices dev.cur dev.off
#' @importFrom grid grid.newpage
#' @include plotting-classes.R
#' @rdname clearPlot
#'
#' @example inst/examples/example_Plot.R
#'
setGeneric("clearPlot", function(dev = dev.cur(), removeData = TRUE, force = FALSE) {
  standardGeneric("clearPlot")
})

#' @export
#' @rdname clearPlot
setMethod(
  "clearPlot",
  signature = c("numeric", "logical", "ANY"),
  definition = function(dev, removeData, force) {

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
      dev(dc)
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
#' Convert \code{grid.locator} units
#'
#' Internal function from example in \code{?grid.locator}.
#' Converts \code{grid.locator} units to meaningful units.
#' Used within \code{.clickCoord}
#'
#' @param grid.locator an object that was output by a call to \code{grid.locator}
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
#' These functions use \code{grid.locator}. The primary two user-level functions are
#' \code{clickValues} and \code{clickExtent}. These functions automatically select
#' the correct viewport (i.e., map) where the mouse clicks occurred so the user
#' does not have to manually specify which map is being clicked on.
#' This works for \code{Raster*}, \code{SpatialPoints*}, and \code{SpatialPolygons*} objects.
#'
#' \code{clickValues} is equivalent to running \code{X[SpatialPoints(locator(n))]}, where
#' X is the raster being clicked on, in base graphics. This function determines which place in the
#' grid.layout was clicked and makes all appropriate calculations to determine the value
#' on the raster(s) at that or those location(s). It should be noted that when zooming in
#' to rasters, plotting of rasters will only allow for complete pixels to be plotted, even
#' if the extent is not perfectly in line with pixel edges. As a result, when values
#' returned by this function may be slightly off (<0.5 pixel width).
#'
#' \code{clickExtent} is for drawing an extent with two mouse clicks on a given Plotted map.
#'
#' \code{clickCoordinates} is the workhorse function that determines which plot has been
#' clicked on and passes this plot name and the clicked coordinates to \code{.clickCoord}.
#'
#' \code{.clickCoord} is intended for internal use and is called by other functions here.
#'
#' @param n The number of mouse clicks to do.
#'
#' @return \code{clickValues} returns the layer names and values at the clicked points.
#' \code{clickExtent} invisibly returns the extent object, and optionally plots
#' it in a new device window.
#' \code{clickCoordinates} returns the xy coordinates in the units of the plot clicked on.
#'
#' @author Eliot McIntire
#' @export
#' @importFrom raster is.factor factorValues cellFromXY
#' @include plotting-classes.R
#' @rdname quickPlotMouseClicks
#'
#' @examples
#' \dontrun{
#'   library(igraph)
#'   library(raster)
#'
#'   files <- system.file("maps", package = "quickPlot") %>%
#'     dir(., full.names = TRUE, pattern = "tif")
#'   maps <- lapply(files, function(x) raster(x))
#'   names(maps) <- sapply(basename(files), function(x) {
#'     strsplit(x, split = "\\.")[[1]][1]
#'   })
#'   landscape <- stack(maps$DEM, maps$forestCover, maps$habitatQuality)
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
#'
clickValues <- function(n = 1) {
  coords <- clickCoordinates(n = n)
  objLay <- strsplit(coords$map, "\\$")
  objNames <- unlist(lapply(objLay, function(x) x[1]))
  layNames <- unlist(lapply(objLay, function(x) x[2]))
  for (i in 1:n) {
    ras1 <- eval(parse(text = objNames[i]), envir = coords$envir[[i]])
    if (!is.na(layNames[i])) {
      coords$coords$value <- unlist(lapply(seq_len(n), function(i) {
        ras1[[layNames[i]]][cellFromXY(ras1[[layNames[i]]], coords$coords[i, 1:2])]
      }))
    } else {
      coords$coords$value <- unlist(lapply(seq_len(n), function(i) {
        ras1[cellFromXY(ras1, coords$coords[i, 1:2])]
      }))
    }
  }
  if (any(raster::is.factor(ras1)) & all(ras1[]%%1==0)) {
    for (i in which(raster::is.factor(ras1)))
      coords$coords$value <- factorValues(ras1[[i]], coords$coords$value)
  }
  return(coords$coords)
}

#' @param devNum The device number for the new plot to be plotted on.
#'
#' @param plot.it Logical. If \code{TRUE} a new plotting window is made for the
#'                new extent. Default \code{TRUE}.
#'
#' @export
#' @importFrom grDevices dev.cur
#' @importFrom raster crs<- crs
#' @importFrom fpCompare %==%
#' @include plotting-classes.R
#' @rdname quickPlotMouseClicks
#'
#' @details
#' \code{clickExtent} will place the new, zoomed in plot over top of the existing
#' object. To recover original full object, double click anywhere during an
#' active \code{clickExtent}. See example.
#'
clickExtent <- function(devNum = NULL, plot.it = TRUE) {
  corners <- clickCoordinates(2)
  zoom <- extent(c(sort(corners[[3]]$x), sort(corners[[3]]$y)))

  if (plot.it) {

    objLay <- strsplit(corners$map, "\\$")
    objNames <- unique(unlist(lapply(objLay, function(x) x[1])))
    layNames <- unique(unlist(lapply(objLay, function(x) x[2])))

    devActive <- dev.cur()
    if (!(is.null(devNum))) {
      dev(devNum)
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
      Plot(theObj, addTo = theName, title = theName, new = TRUE, zoomExtent = extent(theObj[[1]]))
    } else {
      Plot(theObj, addTo = theName, title = theName, zoomExtent = zoom, new = TRUE)
    }

    if (!(is.null(devNum))) {
      dev(devActive)
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
  nulls <- as.character(gl$widths)[grepNullsW] %>%
    strsplit(., "null") %>%
    unlist() %>%
    as.numeric()
  npcs <- as.character(gl$widths)[grepNpcsW] %>%
    strsplit(., "npc") %>%
    unlist() %>%
    as.numeric()
  remaining <- 1 - sum(npcs)
  npcForNulls <- nulls * remaining / sum(nulls)
  widthNpcs <- c(npcs, npcForNulls)[order(c(grepNpcsW, grepNullsW))]

  grepNullsH <- grep("null$", gl$heights)
  grepNpcsH <- grep("npc$", gl$heights)
  nulls <- as.character(gl$heights)[grepNullsH] %>%
    strsplit(., "null") %>%
    unlist() %>%
    as.numeric()
  npcs <- as.character(gl$heights)[grepNpcsH] %>%
    strsplit(., "npc") %>%
    unlist() %>%
    as.numeric()
  remaining <- 1 - sum(npcs)
  npcForNulls <- nulls * remaining / sum(nulls)
  heightNpcs <- c(npcs, npcForNulls)[order(c(grepNpcsH, grepNullsH))]

  clickCoords <- data.frame(x = NA_real_, y = NA_real_, stringsAsFactors = FALSE)
  mapNames <- character(n)
  envs <- list()

  grobLoc <- list()

  for (i in 1:n) {
    seekViewport("top", recording = FALSE)
    gloc <- grid.locator(unit = "npc")
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
#' @param gl An object created by a call to \code{grid.locator}.
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
#' launch a new graphics device based on operating system used. On Windows and
#' Mac, if no x is provided, then this will open or switch to the first non
#' R Studio device, which is much faster than the png-based R Studio Plot device.
#' Currently, this will not open anything new
#'
#' For example, \code{dev(6)} switches the active plot device to device #6.
#' If it doesn't exist, it opens it. NOTE: if devices 1-5 don't exist
#' they will be opened too.
#'
#' @param x   The number of a plot device. If missing, will open a new
#'            non-RStudio plotting device
#'
#' @param ... Additional arguments passed to \code{\link{newPlot}}.
#'
#' @return Opens a new plot device on the screen. Invisibly returns the
#' device number selected.
#'
#' @author Eliot McIntire and Alex Chubaty
#' @export
#' @importFrom grDevices dev.list dev.set
#' @include plotting-classes.R
#' @rdname dev
#'
#' @examples
#' \dontrun{
#'   dev(4)
#' }
#'
dev <- function(x, ...) {
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
      if (any(names(dev.list()) == "RStudioGD")) {
        x <- min(min(dev.list()) + 1,
                 which(names(dev.list()) == "RStudioGD") + 3L)
        dev(x)
      } else {
        x <- min(dev.list())
        dev(x)
      }
    }
  }
  if (identical(getOption("device"), "RStudioGD")) {
    if (.Platform$OS.type == "unix" && !isRstudioServer()) {
      message("setting graphics device away from Rstudio device. To return to Rstudio device: dev.useRSGD(TRUE)")
      dev.useRSGD(FALSE)
    }
  }

  if (is.null(dev.list())) {
    newPlot(...)
  } else {
    while (dev.set(x) < x) newPlot(...)
  }
  #if (.Platform$OS.type != "unix") {
  #}
  return(invisible(dev.cur()))
}

################################################################################
#' Open a new plotting window
#'
#' @param noRStudioGD Logical Passed to dev.new. Default is TRUE to avoid using
#'                    RStudio graphics device, which is slow.
#'
#' @param useRSGD     Logical indicating whether the default device should be the
#'                    RStudio graphic device, or the platform default (\code{quartz}
#'                    on macOS; \code{windows} on Windows; \code{x11} on others, e.g., Linux).
#'
#' @param ...         Additional arguments.
#'
#' @note \code{\link{dev.new}} is supposed to be the correct way to open a new
#' window in a platform-generic way; however, doesn't work in RStudio
#' (\href{https://github.com/PredictiveEcology/SpaDES/issues/116}{SpaDES#116}).
#' Use \code{dev.useRSGD(FALSE)} to avoid RStudio for the remainder of this session,
#' and \code{dev.useRSGD(TRUE)} to use the RStudio graphics device.
#' (This sets the default device via the \code{device} option.)
#'
#' @seealso \code{\link{dev}}.
#'
#' @author Eliot McIntire and Alex Chubaty
#' @export
#' @importFrom grDevices dev.new
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
newPlot <- function(noRStudioGD = TRUE, ...) {
  if (isRstudioServer()) {
    noRStudioGD <- FALSE
    message("Using Rstudio server; not opening a new window")
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
