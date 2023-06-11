utils::globalVariables(c("groups", "thin", "whGrobNamesi", "xmax", "xmin", "ymax", "ymin"))

################################################################################
#' `Plot`: Fast, optimally arranged, multi-panel plotting
#'
#' This can take objects of type `Raster*`, `SpatialPoints*`, `SpatialPolygons*`,
#' and any combination of those.
#' These can be provided as individual objects, or a named list.
#' If a named list, the names either represent a different original object in the
#' calling environment and that will be used, or if the names don't exist in the
#' calling environment, then they will be copied to `.quickPlotEnv` for reuse later.
#' It can also handle `ggplot2` objects or `base::histogram` objects
#' created via call to `exHist <- hist(1:10, plot = FALSE)`. It can also take
#' arguments as if it were a call to `plot`. In this latter
#' case, the user should be explicit about naming the plot area using `addTo`.
#' Customization of the `ggplot2` elements can be done as a normal
#' `ggplot2` plot, then added with `Plot(ggplotObject)`.
#'
#' **NOTE:** Plot uses the \pkg{grid} package; therefore, it is NOT compatible
#' with base R graphics. Also, because it does not by default wipe the plotting device
#' before plotting, a call to [clearPlot()] is helpful to resolve
#' many errors. Careful use of the other device tools, such as `dev.off()` and
#' `dev.list()` might also clear problems that may arise.
#'
#' If `new = TRUE`, a new plot will be generated, but only in the figure region that
#' has the same name as the object being plotted.
#' This is different than calling `clearPlot(); Plot(Object)`,
#' i.e,. directly before creating a new Plot. `clearPlot()` will clear the entire
#' plotting device.
#' When `new = FALSE`, any plot that already exists will be overplotted,
#' while plots that have not already been plotted will be added.
#' This function rearranges the plotting device to maximize the size of all the
#' plots, minimizing white space.
#' If using the RStudio IDE, it is recommended to make and use a new device
#' with `dev()`, because the built in device is not made for rapid redrawing.
#' The function is based on the grid package.
#'
#' Each panel in the multipanel plot must have a name.
#' This name is used to overplot, rearrange the plots, or overlay using
#' `addTo` when necessary.
#' If the `...` are named `spatialObjects`, then `Plot` will use
#' these names. However, this name will not persist when there is a future call
#' to `Plot` that forces a rearrangement of the plots.
#' A more stable way is to use the object names directly, and any layer names
#' (in the case of `RasterLayer` or `RasterStack` objects).
#' If plotting a `RasterLayer` and the layer name is "layer" or the same as the
#' object name, then, for simplicity, only the object name will be used.
#' In other words, only enough information is used to uniquely identify the plot.
#'
#' For modularity, `Plot` must have access to the original objects that were plotted.
#' These objects will be used if a subsequent Plot event forces a rearrangement of the plot device.
#' Rather than saving all the plot information (including the data) at each `Plot` call
#' (this is generally too much data to constantly make copies),
#' the function saves a pointer to the original R object. If the plot needs
#' to be rearranged because of a future addition, then `Plot` will search for that
#' original object that created the first plots, and replot them.
#' This has several consequences.
#' First, that object must still exist and in the same environment.
#' Second, if that object has changed between the first time it is plot and any
#' subsequent time it is replotted (via a forced rearrangement), then it will take
#' the object *as it exists*, not as it existed. Third, if passing a named list
#' of objects, Plot will either create a link to objects with those names in the
#' calling environment (e.g., `.GlobalEnv`) or, if they do not exist, then `Plot`
#' will make a copy in the hidden `.quickPlotEnv` for later reuse.
#'
#'
#' `cols` is a vector of colours that can be understood directly, or by
#' [`colorRampPalette()`][grDevices::colorRamp], such as `c("orange", "blue")`,
#' will give a colour range from orange to blue, interpolated.
#' If a list, it will be used, in order, for each item to be plotted.
#' It will be recycled if it is shorter than the objects to be plotted.
#' Note that when this approach to setting colours is used, any overplotting
#' will revert to the `colortable` slot of the object, or the default
#' for rasters, which is `terrain.color()`
#'
#' `cols` can also accept `RColorBrewer` colours by keyword if it is
#' character vector of length 1. i.e., this cannot be used to set many objects by keyword in
#' the same Plot call. Default `terrain.color()`. See Details.
#'
#' Some colouring will be automatic. If the object being plotted is a Raster, then
#' this will take the `colorTable` slot (can be changed via `setColors()` or other ways).
#' If this is a `SpatialPointsDataFrame`, this function will use a column called `colors`
#' and apply these to the symbols.
#'
#' For `SpatialPolygons`, `cols` can accept `RColorBrewer` colours by keyword as a
#' character vector of length 1. For more control, pass a vector of colours to `cols` or
#' to `gp = gpar(fill = vectorOfColours)`.
#' In this second approach, the length of the `vectorOfColours` can be either less then or equal
#' to the number of polygons in the `SpatialPolygons` object -- each polygon within
#' a `Polygons` object will share the same colour -- or it can be greater than this number
#' to give a different colour to each `Polygon` (of which there can be MANY more than
#' `Polygons`. `Plot` will recycle these colours if there are not enough. The order
#' provided will be the order assigned to each `Polygons` or `Polygon` object.
#'
#'
#' Silently, one hidden object is made, `.quickPlot` in the
#' `.quickPlotEnv` environment, which is used for arranging plots in the
#' device window, and identifying the objects to be replotted if rearranging
#' is required, subsequent to a `new = FALSE` additional plot.
#'
#' This function is optimized to allow modular Plotting.
#' This means that several behaviours will appear unusual.
#' For instance, if a first call to `Plot` is made, the legend will reflect
#' the current colour scheme. If a second or subsequent call to `Plot` is
#' made with the same object but with different colours (e.g., with `cols`),
#' the legend will not update. This behaviour is made with the decision that the
#' original layer takes precedence and all subsequent plots to that same frame
#' are over-plots only.
#'
#' `speedup` is not a precise number because it is faster to plot an
#' non-resampled raster if the new resampling is close to the original number of
#' pixels.
#' At the moment, for rasters, this is set to 1/3 of the original pixels.
#' In other words, `speedup` will not do anything if the factor for
#' speeding up is not high enough (i.e., >3). If no sub-sampling is desired,
#' use a speedup value less than 0.1.
#'
#' These `gp*` parameters will specify plot parameters that are available
#' with `gpar()`. `gp` will adjust plot parameters, `gpText`
#' will adjust title and legend text, `gpAxis` will adjust the axes.
#' `size` adjusts point size in a `SpatialPoints` object.
#' These will persist with the original `Plot` call for each individual object.
#' Multiple entries can be used, but they must be named list elements and they
#' must match the `...` items to plot.
#' This is true for a `RasterStack` also, i.e., the list of named elements
#' must be the same length as the number of layers being plotted.
#' The naming convention used is: `RasterStackName$layerName`, i.e,
#' `landscape$DEM`.
#'
#' @param ... A combination of `spatialObjects` or non-spatial objects.
#'            For many object classes, there are specific `Plot` methods. Where
#'            there are no specific ones, the base plotting will be used internally.
#'            This means that for objects with no specific `Plot` methods,
#'            many arguments, such as `addTo`, will not work.
#'            See details.
#'
#' @param new Logical. If `TRUE`, then the previous named plot area is wiped
#'            and a new one made; if `FALSE`, then the `...` plots will be
#'            added to the current device, adding or rearranging the plot layout
#'            as necessary. Default is `FALSE`. This currently works best if
#'            there is only one object being plotted in a given `Plot` call. However,
#'            it is possible to pass a list of logicals to this, matching the
#'            length of the `...` objects. Use `clearPlot` to clear the whole
#'            plotting device. NOTE if `TRUE`: *Everything that was there,
#'            including the legend and the end points of the colour palette, will
#'            be removed and re-initiated*.
#'
#' @param addTo Character vector, with same length as `...`.
#'              This is for overplotting, when the overplot is not to occur on
#'              the plot with the same name, such as plotting a
#'              `SpatialPoints*` object on a `RasterLayer`.
#'
#' @param gp A `gpar` object, created by [`gpar()`],
#'           to change plotting parameters (see \pkg{grid} package).
#'
#' @param gpText A `gpar` object for the title text.
#'               Default `gpar(col = "black")`.
#'
#' @param gpAxis A `gpar` object for the axes.
#'               Default `gpar(col = "black")`.
#'
#' @param axes Logical or `"L"`, representing the left and bottom axes, over all plots.
#'
#' @param speedup Numeric. The factor by which the number of pixels is divided
#'                by to plot rasters. See Details.
#'
#' @param size Numeric. The size, in points, for `SpatialPoints` symbols,
#'             if using a scalable symbol.
#'
#' @param cols (also `col`) Character vector or list of character vectors of colours.
#'             See details.
#'
#' @param col (also `cols`) Alternative to `cols` to be consistent with `plot`.
#'            `cols` takes precedence, if both are provided.
#'
#' @param zoomExtent An `Extent` object. Supplying a single extent that is
#'                   smaller than the rasters will call a crop statement before
#'                   plotting. Defaults to `NULL`.
#'                   This occurs after any downsampling of rasters, so it may
#'                   produce very pixelated maps.
#'
#' @param visualSqueeze Numeric. The proportion of the white space to be used
#'                      for plots. Default is 0.75.
#'
#' @param legend Logical indicating whether a legend should be drawn.
#'               Default is `TRUE`.
#'
#' @param legendRange Numeric vector giving values that, representing the lower
#'                    and upper bounds of a legend (i.e., `1:10` or
#'                    `c(1,10)` will give same result) that will override
#'                    the data bounds contained within the `grobToPlot`.
#'
#' @param legendText Character vector of legend value labels.
#'                   Defaults to `NULL`, which results in a pretty numeric
#'                   representation.
#'                   If `Raster*` has a Raster Attribute Table (`rat`;
#'                   see \pkg{raster} package), this will be used by default.
#'                   Currently, only a single vector is accepted.
#'                   The length of this must match the length of the legend, so
#'                   this is mostly useful for discrete-valued rasters.
#'
#' @param na.color Character string indicating the colour for `NA` values.
#'                 Default transparent.
#'
#' @param zero.color Character string indicating the colour for zero values,
#'                   when zero is the minimum value, otherwise, zero is
#'                   treated as any other colour. Default transparent.
#'
#' @param pch see `?par`.
#'
#' @param title Logical or character string. If logical, it
#'              indicates whether to print the object name as the title
#'              above the plot. If a character string, it will print this
#'              above the plot. NOTE: the object name is used with `addTo`,
#'              not the title. Default NULL, which means print the object
#'              name as title, if no other already exists on the plot, in
#'              which case, keep the previous title.
#'
#' @param length Numeric. Optional length, in inches, of the arrow head.
#'
#' @param arr A vector of length 2 indicating a desired arrangement of plot
#'            areas indicating number of rows, number of columns.
#'            Default NULL, meaning
#'            let Plot function do it automatically.
#'
#' @param plotFn An optional function name to do the plotting internally, e.g.,
#'               "barplot" to get a barplot() call. Default "plot".
#' @param verbose Numeric or logical. If `TRUE` or `>0`, then messages will be
#'   shown. If `FALSE` or `0`, most messages will be suppressed.
#'
#' @return Invisibly returns the `.quickPlot` class object.
#' If this is assigned to an object, say `obj`, then this can be plotted
#' again with `Plot(obj)`.
#' This object is also stored in the locked `.quickPlotEnv`, so can simply be
#' replotted with `rePlot()` or on a new device with `rePlot(n)`,
#' where `n` is the new device number.
#'
#' @seealso [clearPlot()], [rePlot()], [gpar()], `raster::raster()`,
#' [par()], `sp::SpatialPolygons()`, [grid.polyline()],
#' `ggplot2::ggplot()`, [dev()], `terra::vect()`, `terra::rast()`
#'
#' @author Eliot McIntire
#' @export
#' @importFrom grDevices dev.cur dev.size
#' @importFrom grid current.parent grid.rect grid.xaxis grid.yaxis gpar pushViewport upViewport
#' @include environment.R
#' @include plotting-classes.R
#' @include plotting-colours.R
#' @include plotting-helpers.R
#' @include plotting-other.R
#'
#' @rdname Plot
#'
#' @example inst/examples/example_Plot.R
#'
setGeneric(
  "Plot",
  signature = "...",
  function(..., new = FALSE, addTo = NULL,
           gp = gpar(), gpText = gpar(), gpAxis = gpar(), axes = FALSE,
           speedup = 1, size = 5, cols = NULL, col = NULL, zoomExtent = NULL,
           visualSqueeze = NULL, legend = TRUE, legendRange = NULL,
           legendText = NULL, pch = 19, title = NULL, na.color = "#FFFFFF00", # nolint
           zero.color = NULL, length = NULL, arr = NULL, plotFn = "plot",
           verbose = getOption("quickPlot.verbose")) { # nolint
    standardGeneric("Plot")
})

#' @rdname Plot
#' @export
setMethod(
  "Plot",
  signature("ANY"),
  definition = function(..., new, addTo, gp, gpText, gpAxis, axes, speedup,
                        size, cols, col, zoomExtent, visualSqueeze, legend,
                        legendRange, legendText, pch, title, na.color, # nolint
                        zero.color, length, arr, plotFn, verbose = getOption("quickPlot.verbose")) { # nolint
    # Section 1 - extract object names, and determine which ones need plotting,
    # which ones need replotting etc.

    news <- unlist(lapply(new, function(x) x))
    # this covers the case where R thinks that there is nothing, but
    #  there may in fact be something.
    if (sum(news) > 0) {
      if (length(ls(.quickPlotEnv)) +
          numLayers(.quickPlotEnv[[paste0("quickPlot", dev.cur())]]) - 1 <= sum(news) |
          length(ls(.quickPlotEnv)) == 0) {
        clearPlot(dev.cur())
      }}

    # Determine object names that were passed and layer names of each
    scalls <- sys.calls()

    # This testthat is a work around:
    # A test_that call can be very long, with many function calls, including Plot and do.call,
    # even if they don't have anything to do with each other
    isDoCall <- grepl("^do.call", scalls) & grepl("\\<Plot\\>", scalls) & !grepl("test_that", scalls)

    dots <- list(...)
    if (is(dots[[1]], "list") & !isQuickPlottables(dots[[1]]) &
        # some reason, `inherits` doesn't work here for ggplot
        !inherits(dots[[1]], "communities") & !inherits(dots[[1]], "igraph") &
        !inherits(dots[[1]], "histogram")) {
      dots <- dots[[1]]
      # dots <- unlist(dots, recursive = FALSE)
      listNames <- names(dots)
      isList <- TRUE
      if (is.null(names(dots)))
        stop("If providing a list of objects to Plot, it must be a named list.")
    } else {
      isList <- FALSE
    }

    # Determine where the objects are located
    #  We need to know exactly where they are, so that they can be replotted later, if needed
    #whFrame <- grep(scalls, pattern = "^Plot|^quickPlot::Plot")
    if (any(isDoCall)) {
      doCallCall <- as.list(match.call(do.call, call = scalls[isDoCall][[1]]))
      argNames <- names(doCallCall$args[-1]) # don't need "list()"
      nonDots <- match.arg(several.ok = TRUE, arg = argNames, choices = formalArgs(Plot))
      nonDotsWh <- match(nonDots, argNames)
      plotArgs <- as.list(doCallCall$args)[nonDots]
      plotFrame <- sys.frame(which(isDoCall) - 1)
      dotNames <- setdiff(argNames, nonDots)
      noName <- !nzchar(dotNames)
      #dotObjs <- mget(dotNames[!noName], envir = plotFrame, inherits = TRUE)
      if (isTRUE(any(noName))) {
        dotObjs2 <- as.list(doCallCall$args)[-1][-nonDotsWh]
        dotNames <- as.character(dotObjs2)
        if (is.null(title)) plotArgs$title <- ifelse(nzchar(names(dotObjs2)), names(dotObjs2), dotNames)
      }
      dotObjs <- mget(dotNames, envir = plotFrame, inherits = TRUE)

      #stop("Currently, Plot can not be called within a do.call. ",
      #     "Try passing a named list of objects to Plot instead.")
    } else {
      dotObjs <- dots
    }

    whFrame <- max(1, grep(scalls, pattern = "standardGeneric.*Plot"))
    whFramePlot <- max(1, grep(scalls, pattern = "standardGeneric.*Plot") - 1)

    for (fr in rev(whFrame)) {
      plotFrame <- sys.frame(fr - 1)
      plotArgsTmp <-
        tryCatch(
          mget(names(formals("Plot")), plotFrame),
          error = function(x)
            TRUE
        )
      if (isTRUE(plotArgsTmp)) {
        conti <- TRUE
      } else {
        plotArgsTmp <- plotArgsTmp[-1]
        conti <- FALSE
      }
      if (!conti) {
        if (exists("plotArgs", inherits = FALSE)) {
          plotArgsTmp[names(plotArgs)] <- plotArgs
        }
        plotArgs <- plotArgsTmp
        break
      }
    }

    # if user uses col instead of cols
    if (!is.null(arr)) {
      plotArgs$arr <- NULL
    }
    if (is.null(cols)) {
      if (!is.null(col)) {
        cols <- col
        plotArgs$cols <- cols
      }
    } else {
      if (!is.null(col)) {
        messageVerbose("cols and col both supplied. Using cols", verbose = verbose)
      }
    }

    if (!is.null(dots$env)) {
      dotObjs$env <- NULL
    }

    whichQuickPlottables <- sapply(dotObjs, function(x) {
      isQuickPlottables(x) # `inherits` doesn't work for gg objects, need `is`
    })

    if (!(all(!whichQuickPlottables) | all(whichQuickPlottables)))
      stop("Can't mix base plots with .quickPlottables objects in one function call. ",
           "Please call Plot for base plots separately.")

    # Create plotObjs object, which is a cleaned up version of the objects passed into Plot
    if (all(!whichQuickPlottables)) {
      ## if not a .quickPlottables then it is a pass to plot or points
      if (!exists(paste0("basePlots_", dev.cur()), envir = .quickPlotEnv))
        .assignQuickPlot(paste0("basePlots_", dev.cur()),
                         new.env(hash = FALSE, parent = .quickPlotEnv))
      mc <- match.call(get(plotArgs$plotFn), call(plotArgs$plotFn, quote(...)))
      mcPlot <- match.call(Plot, call = sys.call(whFramePlot))
      plotArgs$userProvidedPlotFn <- ("plotFn" %in% names(mcPlot))

      basePlotDots <- list()
      for (i in names(mc)[-1]) basePlotDots[[i]] <- eval(mc[[i]])
      plotObjs <- list(list(basePlotDots))
      plotXYArgs <- substitute(list(...))
      xAndY <- c("x", "y") %in% names(basePlotDots)
      xAndYLab <- c("xlab", "ylab") %in% names(basePlotDots)
      xAndYSum <- sum(xAndY)
      if (!is.null(basePlotDots$xlab) | !is.null(basePlotDots$ylab)) {
        plotArgs$axisLabels <- list(c(basePlotDots$xlab, basePlotDots$ylab))
        names(plotArgs$axisLabels[[1]]) <- c("x", "y")[xAndYLab]
      } else {
        plotArgs$axisLabels <- list(unlist(lapply(plotXYArgs[1:xAndYSum + 1], deparse)))
      }

      if (is.null(addTo)) {
        addTo <- "basePlot1"
      }
      plotArgs$addTo <- addTo

      if (is.null(mcPlot$axes)) {
        plotArgs$axes <- "L"
      }

      if (is.null(plotArgs$title)) {
        plotArgs$title <- mc$main
      }

      if (is.null(mcPlot$col)) {
        if (!any(unlist(lapply(dotObjs, function(x) {
          any(unlist(lapply(c("histogram", "igraph", "communities"), function(y) inherits(x, y))))
        })))) #default for histogram is NULL
          plotArgs$col <- NULL#"black"
      }
      plotArgs$main <- ""
      plotObjs[[1]][[1]]$main <- plotArgs$main
      basePlotDots$main <- plotArgs$main

      if (addTo %in% ls(.getQuickPlot(paste0("basePlots_", dev.cur())))) {
        plotObjsName <- paste0(addTo, "_",
                               length(ls(.getQuickPlot(paste0("basePlots_", dev.cur())))) + 1)
      } else {
        plotObjsName <- addTo
      }
      names(plotObjs) <- plotObjsName
      assign(plotObjsName, basePlotDots, envir = .getQuickPlot(paste0("basePlots_", dev.cur())))
      objFrame <- .getQuickPlot(paste0("basePlots_", dev.cur()))
    } else {
      # non base plots
      canPlot <- if (!is.null(names(whichQuickPlottables))) {
        whichQuickPlottables[names(whichQuickPlottables) != "env"]
      } else {
        whichQuickPlottables
      }

      if (!all(canPlot)) {
        if ((sum(canPlot) - length(grep(pattern = "col", names(canPlot)))) > 0) { # nolint
          # don't message if col is passed
          messageVerbose(verbose = verbose, paste(
            "Plot can only plot objects of class .quickPlottables.",
            "Use 'showClass(\".quickPlottables\")' to see current available",
            "classes"
          ))
        }
      }
      plotObjs <- dotObjs[whichQuickPlottables]
    }

    if (any(whichQuickPlottables)) {
      # perhaps push objects into an environment, if they are only in the list
      devCurEnvName <- paste0("Dev", dev.cur())
      devCurEnv <- get0(devCurEnvName, envir = .quickPlotEnv)
      if (isList) {
        if (is.null(devCurEnv)) {
          devCurEnv <- new.env(hash = FALSE, parent = .quickPlotEnv)
          assign(devCurEnvName, devCurEnv, envir = .quickPlotEnv)
        }

        objFrame <- get(devCurEnvName, envir = .quickPlotEnv)
        dots$env <- list2env(dots, envir = objFrame)
      } else {
        if (any(isDoCall)) {
          objNames <- names(dotObjs)
        } else {
          objNames <- lapply(substitute(placeholderFunction(...))[-1],
                             deparse, backtick = TRUE)
        }
        objFrame <- try(whereInStack(objNames[[1]]), silent = TRUE)
        # sanity check -- maybe won't exist
        allGood <- try(eval(parse(text = objNames[[1]]), envir = objFrame), silent = TRUE)
        if (is(allGood, "try-error"))
          objFrame <- try(whereInStack2(objNames[[1]]), silent = TRUE)

        if (is(objFrame, "try-error")) {
          if (is.null(devCurEnv)) {
            devCurEnv <- new.env(hash = FALSE, parent = .quickPlotEnv)
            assign(devCurEnvName, devCurEnv, envir = .quickPlotEnv)
          }
          objFrame <- get(devCurEnvName, envir = .quickPlotEnv)
          objNames[[1]] <- paste0("Object_", length(ls(devCurEnv)) + 1)
          names(dots) <- objNames
          dots$env <- list2env(dots, envir = objFrame)
        }

        names(plotObjs)[whichQuickPlottables] <- objNames
      }
    }

    nonPlotArgs <- dotObjs[!whichQuickPlottables]
    nonPlotArgs <- append(nonPlotArgs, list(verbose = verbose))
    if (any(grepl(pattern = "col", names(nonPlotArgs)))) {
      nonPlotArgs$col <- "black"
    }

    # intercept cases that don't make sense, and give meaningful error
    if (!is.null(addTo)) {
      if (!tryCatch(
        addTo %in%
        unlist(layerNames(get(paste0("quickPlot", dev.cur()), envir = .quickPlotEnv))),
        error = function(x) FALSE)) {
        plotArgs$addTo <- NULL
      }
    }

    isQuickPlot <- sapply(plotObjs, function(x) inherits(x, ".quickPlot"))
    isQuickPlotLong <- rep(isQuickPlot, unlist(lapply(plotObjs, numLayers)))

    # Create a .quickPlot object from the plotObjs and plotArgs
    plotArgs$verbose <- NULL
    newQuickPlots <- .makeQuickPlot(plotObjs, plotArgs, whichQuickPlottables, env = objFrame)

    # names sanity check
    nams1 <- names(newQuickPlots@quickPlotGrobList)
    if (!identical(nams1, unique(nams1)))
      stop("quickPlot cannot currently plot objects where `names(obj)` != unique(names(obj)); maybe rename layer(s)?")

    if (exists(paste0("quickPlot", dev.cur()), envir = .quickPlotEnv)) {
      currQuickPlots <- .getQuickPlot(paste0("quickPlot", dev.cur()))

      visualSqueeze <- if (is.null(visualSqueeze)) {
        currQuickPlots$curr@arr@layout$visualSqueeze
      } else {
        visualSqueeze
      }

      updated <- .updateQuickPlot(newQuickPlots, currQuickPlots)

      # Do all the plots fit into the device?
      devResized <- !identical(currQuickPlots$curr@arr@ds, dev.size())
      newArr <- (
        length(updated$curr@quickPlotGrobList) >
          prod(currQuickPlots$curr@arr@columns, currQuickPlots$curr@arr@rows)
      ) | devResized
      if (!is.null(arr))
        if (!identical(c(updated$curr@arr@rows, updated$curr@arr@columns), arr))
          newArr <- TRUE

      if (newArr) {
        updated$needPlotting <- lapply(updated$needPlotting, function(x) {
          sapply(x, function(y) TRUE)
        })
        updated$isReplot <- lapply(updated$isReplot, function(x) {
          sapply(x, function(y) TRUE)
        })
        if (devResized)
          messageVerbose("Device resized, replotting", verbose = verbose)
        clearPlot(removeData = FALSE)
      }
    } else if (all(isQuickPlot)) {
      currQuickPlots <- .makeQuickPlot()
      newQuickPlots <- plotObjs[[1]]

      visualSqueeze <- if (is.null(visualSqueeze)) {
        newQuickPlots@arr@layout$visualSqueeze
      } else {
        visualSqueeze
      }

      updated <- .updateQuickPlot(newQuickPlots)
      newArr <- TRUE
    } else {
      currQuickPlots <- .makeQuickPlot()
      updated <- .updateQuickPlot(newQuickPlots)
      newArr <- TRUE
    }

    # Section 2 # Optimal Layout and viewport making
    # Create optimal layout, given the objects to be plotted, whether legend and axes are to be
    #  plotted, and visualSqueeze
    if (newArr) {
      if (is.null(visualSqueeze)) {
        visualSqueeze <- 0.75
      }
      updated$curr@arr <- .arrangeViewports(updated$curr, arr = arr)
      updated$curr@arr@layout <- .makeLayout(
        updated$curr@arr, sapply(visualSqueeze, max), sapply(legend, any),
        sapply(axes, function(x) !any(x == TRUE))
      )
    }

    # Create the viewports as per the optimal layout
    if (length(newQuickPlots@quickPlotGrobList) > 0) {
      vps <- .makeViewports(updated$curr, newArr = newArr)
      upViewport(0)
      pushViewport(vps$wholeVp, recording = FALSE)
      upViewport(0)
    }
    updated$curr@arr@extents <- vps$extents
    updated$curr@arr@names <- names(updated$curr@quickPlotGrobList)

    quickSubPlots <- updated$curr@quickPlotGrobList

    # Section 3 - the actual Plotting
    # Plot each element passed to Plot function, one at a time
    for (subPlots in names(quickSubPlots)) {
      quickPlotGrobCounter <- 0
      for (sGrob in quickSubPlots[[subPlots]]) {

        quickPlotGrobCounter <- quickPlotGrobCounter + 1
        needPlot <- updated$needPlotting[[subPlots]][[quickPlotGrobCounter]]

        if (needPlot) {
          isNewPlot <- updated$isNewPlot[[subPlots]][[quickPlotGrobCounter]]
          isReplot <- updated$isReplot[[subPlots]][[quickPlotGrobCounter]]
          isBaseSubPlot <- updated$isBaseLayer[[subPlots]][[quickPlotGrobCounter]]

          whPlotFrame <- match(sGrob@plotName, names(quickSubPlots))

          # Check that the extents are equal.
          # If not, then x and y axes are written where necessary.
          xyAxes <- .xyAxes(sGrob, arr = updated$curr@arr, whPlotFrame)

          layerFromPlotObj <- (names(newQuickPlots@quickPlotGrobList) %in%
                                 sGrob@plotName)
          whLayerFromPO <- which(layerFromPlotObj)

          objNames <- unique(unname(unlist(lapply(newQuickPlots@quickPlotGrobList,
                                                  function(x) x[[1]]@objName))))
          whPlotObj <- which(objNames %in% sGrob@objName)

          layerFromPlotObj <- if (length(whLayerFromPO) == 0) {
            FALSE
          } else if (isQuickPlotLong[whLayerFromPO]) {
            FALSE
          } else {
            layerFromPlotObj[whLayerFromPO]
          }

          # if whPlotObj is length 0, it means that the object is being taken from sGrob@envir
          if (length(whPlotObj) == 0 | !layerFromPlotObj) {
            grobToPlot <-
              eval(parse(text = sGrob@objName), sGrob@envir)
            layerFromPlotObj <- FALSE
          } else {
            grobToPlot <- plotObjs[[whPlotObj]]
          }
          grobToPlot <- .identifyGrobToPlot(grobToPlot, sGrob, layerFromPlotObj)

          isPlotFnAddable <- FALSE

          if (!isQuickPlotObject(grobToPlot)) {
            if (!inherits(grobToPlot, ".quickPlot")) {
              if (sGrob@plotArgs$userProvidedPlotFn && !isTRUE(grobToPlot[["add"]])) {
                isPlotFnAddable <- TRUE
              }
            }
          }

          if (sGrob@plotArgs$new | inherits(grobToPlot, "igraph") |
              inherits(grobToPlot, "histogram") | isPlotFnAddable) {
            ## draw a white rectangle to clear plot
            sGrob <- .refreshGrob(sGrob, subPlots, legendRange,
                                  grobToPlot, plotArgs = sGrob@plotArgs,
                                  nColumns = updated$curr@arr@columns,
                                  nRows = updated$curr@arr@rows,
                                  whLayerFromPO)
            wipe <- TRUE # can't overplot a histogram
          } else {
            wipe <- FALSE
          }

          sGrob <- .updateGrobGPTextAxis(sGrob, arr = updated$curr@arr, newArr = newArr)


          prevMinMax <- tryCatch(updated$curr@quickPlotGrobList[[subPlots]][[subPlots]]@plotArgs[c("minz", "maxz")],
                                 error = function(x) list(minz = NULL, maxz = NULL))
          zMat <- .preparePlotGrob(grobToPlot, sGrob, layerFromPlotObj,
                                   arr = updated$curr@arr, newArr,
                                   prevMinMax = prevMinMax,
                                   quickPlotGrobCounter, subPlots, cols)
          sGrob@plotArgs$minz <- zMat$minz
          sGrob@plotArgs$maxz <- zMat$maxz

          # Add legendRange if not provided
          if (inherits(grobToPlot, "Raster")) {
            if (is.null(sGrob@plotArgs$legendRange)) {
              sGrob@plotArgs$legendRange <-
                c(zMat$minz, zMat$maxz)
            }}

          # Plot any grobToPlot to device, given all the parameters
          sGrob2 <- try(.Plot(sGrob, grobToPlot, subPlots, quickSubPlots, quickPlotGrobCounter,
                         isBaseSubPlot, isNewPlot, isReplot, zMat, wipe, xyAxes, legendText,
                         vps, nonPlotArgs, arr = updated$curr@arr))
          if (!is(sGrob2, "try-error")) {
            sGrob <- sGrob2
          } else {
          }

        } # needPlot
        updated$isNewPlot[[subPlots]][[quickPlotGrobCounter]] <- FALSE
        updated$curr@quickPlotGrobList[[subPlots]][[quickPlotGrobCounter]]@plotArgs <-
          sGrob@plotArgs
      } # sGrob
    } # subPlots

    seekViewport("top", recording = FALSE)
    .assignQuickPlot(paste0("quickPlot", dev.cur()), updated)
    return(invisible(updated$curr))
})

################################################################################
#' Re-plot to a specific device
#'
#' @param toDev    Numeric. Which device should the new replot be plotted to.
#'                 Default is current device.
#'
#' @param fromDev  Numeric. Which device should the replot information be taken from.
#'                 Default is current device
#'
#' @param clearFirst Logical. Should `clearPlot` be run before replotting. Default TRUE.
#'
#' @export
#' @include plotting-classes.R
#' @importFrom grDevices dev.cur
#' @rdname Plot
#'
rePlot <- function(toDev = dev.cur(), fromDev = dev.cur(), clearFirst = TRUE, ...,
                   verbose = getOption("quickPlot.verbose")) {
  if (exists(paste0("quickPlot", fromDev), envir = .quickPlotEnv)) {
    currQuickPlots <- .getQuickPlot(paste0("quickPlot", dev.cur()))
    dev(toDev, verbose = verbose)
    if (clearFirst) clearPlot(toDev)
    suppressWarnings(Plot(currQuickPlots$curr,
                          new = rep(TRUE, length(currQuickPlots$curr@arr@names)), ...))
  } else {
    stop(
      paste(
        "Nothing to rePlot. Need to call Plot first,",
        "or change to correct active device with dev(x),",
        "where x is the active device number."
      )
    )
  }
}

#' Find the environment in the call stack that contains an object by name
#'
#' This is similar to `pryr::where`, except instead of working up the search() path
#' of packages, it searches up the call stack for an object. Ostensibly similar
#' to `base::dynGet`, but it will only return the environment, not the object
#' itself and it will try to extract just the object name from `name`,
#' even if supplied with a more complicated name
#' (e.g., if `obj$firstElement@slot1$size` is
#' supplied, the function will only search for obj). The function is fairly fast.
#' This function is an important component to the `Plot` function.
#'
#' @param name An object name to find in the call stack
#' @param whFrame A numeric indicating which `sys.frame` (by negative number) to start searching in.
#'
#' @details
#' The difference between this and what `get` and `exists` do, is that these other
#' functions
#' search up the enclosing environments, i.e., it matters where the functions were defined.
#' `whereInStack` looks up the call stack environments. See the example for the difference.
#'
#' @return
#' The environment that is in the call stack where the object exists, that is closest to the
#' frame in which this function is called.
#' @export
#' @examples
#' b <- 1
#' inner <- function(y) {
#'   objEnv <- whereInStack("b")
#'   get("b", envir = objEnv)
#' }
#' findB <- function(x) {
#'   b <- 2
#'   inner()
#' }
#' findB() # Finds 2 because it is looking up the call stack, i.e., the user's perspective
#'
#' # defined outside of findB2, so its enclosing environment is the same as findB2
#' innerGet <- function(y) {
#'    get("b")
#' }
#' findB2 <- function(x) {
#'   b <- 2
#'   innerGet()
#' }
#' findB2() # Finds 1 because b has a value of 1 in the enclosing environment of innerGet
#' b <- 3
#' findB2() # Finds 3 because b has a value of 3 in the enclosing environment of innerGet,
#'          #  i.e., the environment in which innerGet was defined
#' findB() # Still finds 2 because the call stack hasn't changed
#'
#' # compare base::dynGet
#' findB3 <- function(x) {
#'   b <- 2
#'   dynGet("b")
#' }
#' findB3() # same as findB(), but marginally faster, because it omits the stripping on
#'          #   sub elements that may be part of the name argument
#'
#'
#' b <- list()
#' findB3 <- function(x) {
#'   b$a <- 2
#'   dynGet("b$a")
#' }
#' testthat::expect_error(findB3()) # fails because not an object name
#'
#' findB <- function(x) {
#'   b$a <- 2
#'   env <- whereInStack("b$a")
#'   env
#' }
#' findB() # finds it
#'
whereInStack <- function(name, whFrame = -1) {
  pat <- "@|\\$|\\[\\["
  objectName <- if (grepl(name, pattern = pat)) {
    strsplit(name, split = pat)[[1]][1]
  } else {
    name
  }

  snframe <- sys.nframe()

  while (!(exists(objectName, envir = sys.frame(whFrame), inherits = FALSE))) {

    whFrame <- whFrame - 1
    if (snframe < (-whFrame)) {
      stop(objectName, " is not on the call stack.", call. = FALSE)
    }
  }
  # Success case
  sys.frame(whFrame)
}


whereInStack2 <- function(name, whFrame = -1) {
  snframe <- sys.nframe()
  keepGoing <- TRUE
  while (keepGoing) {
    frm <- try(eval(parse(text = name), envir = sys.frame(whFrame)), silent = TRUE)
    keepGoing <- is(frm, "try-error")
    if (isFALSE(keepGoing))
      break
    whFrame <- whFrame - 1
    if (snframe < (-whFrame)) {
      stop(name, " is not on the call stack.", call. = FALSE)
    }
  }
  # Success case
  sys.frame(whFrame)
}



# .quickPlotClasses <- c(spatialClasses, "gg")

quickPlotClasses <- c(".quickPlotObjects", ".quickPlot")

isQuickPlotObject <- function(x) {
  isSpatialAny(x) || is(x, "gg")
}

isQuickPlottables <- function(x) {
  (isQuickPlotObject(x) || inherits(x, ".quickPlot")) # && !is(x, "SpatVector") && !isSF(x)
}

isSpatial <- function(x) {
  res <- inherits(x, "Spatial")
  if (res)
    if (!requireNamespace("sp", quietly = TRUE)) stop("Please install.packages('sp') to use sp objects")
  res
}

isSpatialPolygons <- function(x) {
  res <- isSpatial(x) && is(x, "SpatialPolygons")
}

isSpatVector <- function(x) inherits(x, "SpatVector")
isSpat <- function(x) inherits(x, c("SpatRaster", "SpatVector"))
isGridded <- function(x) inherits(x, "SpatRaster") || isRaster(x)
isVector <-  function(x) isSpatVector(x) || isSpatial(x) || isSF(x)
isSpatialAny <- function(x) isGridded(x) || isVector(x)
isSF <- function(x) {
  res <- inherits(x, c("sf", "sfc"))
  if (res)
    if (!requireNamespace("sf", quietly = TRUE)) stop("Please install.packages('sf') to use sf objects")
  res
}


isRaster <- function(x) {
  res <- inherits(x, "Raster")
  if (res)
    if (!requireNamespace("raster", quietly = TRUE)) stop("Please install.packages('raster') to use Raster objects")
  res
  }
isQuickPlotClass <- function(x) isSpatialAny(x) || inherits(x, "gg")
