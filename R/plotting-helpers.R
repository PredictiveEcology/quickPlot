### deal with spurious data.table warnings
if (getRversion() >= "3.1.0") {
  utils::globalVariables(c(".", ".N", "keepAll", "..rmCols", "L1"))
}

#' Find the number of layers in an object
#'
#' A unified function for `raster::nlayers`, `terra::nlyrs`, or lists of these.
#' Default function returns `1L` for all other classes.
#'
#' @param x An object or list of objects.
#'
#' @return The number of layers in the object.
#'
#' @author Eliot McIntire
#' @export
#' @include plotting-classes.R
#' @rdname numLayers
#'
#' @examples
#' library(terra)
#'
#' files <- system.file("maps", package = "quickPlot")
#' files <- dir(files, full.names = TRUE, pattern = "tif")
#' maps <- lapply(files, function(x) rast(x))
#' names(maps) <- sapply(basename(files), function(x) {
#'   strsplit(x, split = "\\.")[[1]][1]
#' })
#' stck <- rast(maps)
#'
#' numLayers(maps)
#' numLayers(stck)
#'
numLayers <- function(x) {
  UseMethod("numLayers")
}

#' @export
numLayers.default <- function(x) {
  if (inherits(x, ".quickPlot")) {
    length(x@arr@extents)
  } else if (isGridded(x)) {
    if (is(x, "SpatRaster")) {
      terra::nlyr(x)
    } else {
      out <- try(raster::nlayers(x), silent = TRUE)
      if (is(out, "try-error"))
        out <- 1L
      out
    }

  } else {
    1L
  }
}

#' @export
numLayers.list <- function(x) {
  sum(unlist(lapply(x, function(y) {
    if (isSpatialAny(y)) {
      numLayers(y)
    } else {
      1L # e.g., histogram
    }
  })))
}

# @export
# @rdname numLayers
# setMethod(
#   "numLayers",
#   signature = "list",
#   definition = function(x) {
#     sum(unlist(lapply(x, function(x) {
#       if (inherits(x, "RasterStack")) {
#         numLayers(x)
#       } else {
#         1L
#       }
#     })))
# })

# @rdname numLayers
# setMethod(
#   "numLayers",
#   signature = ".quickPlot",
#   definition = function(x) {
#     return(length(x@arr@extents))
# })

# @export
# @rdname numLayers
# setMethod(
#   "numLayers",
#   signature = "Raster",
#   definition = function(x) {
#     return(nlayers(x))
# })

# @export
# @rdname numLayers
# setMethod(
#   "numLayers",
#   signature = "Spatial",
#   definition = function(x) {
#     return(1L)
# })

# @export
# @rdname numLayers
# setMethod(
#   "numLayers",
#   signature = "ANY",
#   definition = function(x) {
#     return(1L)
# })



#' Extract the layer names of Spatial Objects
#'
#' There are already methods for `Raster*` objects. This adds methods for
#' `SpatialPoints*`, `SpatialLines*`, and `SpatialPolygons*`,
#' returning an empty character vector of length 1.
#' This function was created to give consistent, meaningful results for all
#' classes of objects plotted by `Plot`.
#'
#' @param object  A `Raster*`, `SpatialPoints*`, `SpatialLines*`,
#'                or `SpatialPolygons*` object; or list of these.
#'
#' @author Eliot McIntire
#' @export
#' @include plotting-classes.R
#' @rdname layerNames
#'
#' @examples
#' library(terra)
#'
#' ## RasterLayer objects
#' files <- system.file("maps", package = "quickPlot")
#' files <- dir(files, full.names = TRUE, pattern = "tif")
#' maps <- lapply(files, function(x) terra::rast(x))
#' names(maps) <- sapply(basename(files), function(x) {
#'   strsplit(x, split = "\\.")[[1]][1]
#' })
#' layerNames(maps)
#'
#' ## SpatVector objects
#' caribou <- terra::vect(cbind(x = stats::runif(1e2, -50, 50),
#'                              y = stats::runif(1e2, -50, 50)))
#' layerNames(caribou)
#'
setGeneric(
  "layerNames",
  function(object) {
    standardGeneric("layerNames")
  })


#' @export
#' @rdname layerNames
setMethod(
  "layerNames",
  signature = "ANY",
  definition = function(object) {
  out <- ""
  if (is(object, "list")) {
    out <- unlist(lapply(object, layerNames))
  } else if (isGridded(object)) {
    out <- names(object)
  } else if (inherits(object, ".quickPlot")) {
    out <- unlist(lapply(object@quickPlotGrobList, function(x) {
      unlist(lapply(x, function(y) y@plotName))[[1]]
    }))
  }

  if (is.null(out))
    out <- ""

  out
})


#' Assess whether a list of extents are all equal
#'
#' @param extents list of extents objects
#'
#' @author Eliot McIntire
#' @export
#' @rdname equalExtent
#'
#' @examples
#' library(terra)
#'
#' files <- system.file("maps", package = "quickPlot")
#' files <- dir(files, full.names = TRUE, pattern = "tif")
#' maps <- lapply(files, function(x) terra::rast(x))
#' names(maps) <- sapply(basename(files), function(x) {
#'   strsplit(x, split = "\\.")[[1]][1]
#' })
#' extnts <- lapply(maps, terra::ext)
#' equalExtent(extnts) ## TRUE
#'
setGeneric("equalExtent", function(extents) {
  standardGeneric("equalExtent")
})

#' @export
#' @rdname equalExtent
setMethod(
  "equalExtent",
  signature = "list",
  definition = function(extents) {
    a <- lapply(extents, extent)
    all(mapply(MoreArgs = list(ext1 = a[[1]]), extOther = a[-1], function(ext1, extOther) {
      isTRUE(all.equal(ext1, extOther))
    }, SIMPLIFY = TRUE))
})

#' Make a `.quickPlot` class object
#'
#' Builds a `.quickPlot` object from a list of objects.
#'
#' @param plotObjects list. Any plot objects.
#'
#' @param plotArgs list. Any arguments that the the grid package can accept for
#' the specific grob types, e.g., `rasterGrob`, `polygonGrob`, etc.
#'
#' @param whichQuickPlottables  Logical indicating which objects in the
#' `Plot` call can be plotted by `Plot`.
#'
#' @param ... additional arguments. Currently nothing.
#'
#' @return A [.quickPlot()] object, which has 2 slots, one for the plot arrangement
#' (i.e., layout and dimensions) and one for all of the `quickPlotGrobs`
#' (stored as a `quickPlotGrobList` of lists `.quickPlotGrob` objects).
#'
#' @author Eliot McIntire
#' @export
#' @include plotting-classes.R
#' @include plotting-helpers.R
#' @keywords internal
#' @rdname makeQuickPlot
#'
setGeneric(".makeQuickPlot", function(plotObjects, plotArgs, whichQuickPlottables, ...) {
  standardGeneric(".makeQuickPlot")
})

#' @export
#' @rdname makeQuickPlot
setMethod(
  ".makeQuickPlot",
  signature = c(plotObjects = "list", plotArgs = "list"),
  definition = function(plotObjects, plotArgs, whichQuickPlottables, ...) {

    isSpatialObjects <- unlist(lapply(plotObjects, function(x) {
      isSpatialAny(x)
    }))

    env <- list(...)$env
    suppliedNames <- names(plotObjects)
    if (any(!nzchar(suppliedNames, keepNA = TRUE))) {
      suppliedNames <- NULL
    }
    if (is.null(suppliedNames)) {
      objs <- .objectNames()[whichQuickPlottables]
    } else {
      objs <- lapply(suppliedNames, function(x) {
        list(objs = x, envs = env)
      })
    }

    names(plotObjects) <- unlist(lapply(objs, function(x) x$objs))

    if (!is.null(suppliedNames)) {
      if (all(unlist(lapply(suppliedNames, nzchar, keepNA = TRUE)))) {
        names(plotObjects)[!is.na(suppliedNames)] <- suppliedNames
      }
    }
    numberLayers <- pmax(1, unlist(lapply(plotObjects, numLayers)))

    lNamesPlotObj <- layerNames(plotObjects)

    isQuickPlot <- unlist(lapply(plotObjects, function(x) inherits(x, ".quickPlot")))

    # The second component will test for a 3 dimensional array
    isStack <- unlist(lapply(plotObjects, function(x)
      inherits(x, "RasterStack") | isTRUE(dim(x)[3] > 1)))

    # Stacks are like lists in that they are a single object, with many
    # layers.  Plot must treat these as any other layers, except that
    # they are stored in single objects. The following set of objects
    # are the "long" versions of the layers, i.e,. a call to say
    # Plot(stack1, layerB) would have two objects, but maybe 5 layers,
    # if the stack had 4 layers in it.
    isQuickPlotLong <- rep(isQuickPlot, numberLayers)
    isStackLong <- rep(isStack, numberLayers)
    isSpatialObjects <- rep(isSpatialObjects, numberLayers)

    lN <- rep(names(plotObjects), numberLayers)
    lN[isQuickPlotLong] <- layerNames(plotObjects[isQuickPlot])
    objectNamesLong <- rep(names(plotObjects), numberLayers)

    # Full layer names, including object name.
    # If layer name is same as object name omit it, and if layer name
    # is "layer", omit it if within a RasterLayer
    lN[isStackLong] <- paste(objectNamesLong[isStackLong],
                             lNamesPlotObj[isStackLong],
                             sep = "$")
    names(lN) <- rep(names(plotObjects), numberLayers)
    names(lN)[isQuickPlotLong] <- layerNames(plotObjects)[isQuickPlotLong]

    # Create long version of environments
    lEnvs <- rep(unlist(lapply(objs, function(x) x$envs)), numberLayers)

    plotArgs <- .makeList(plotArgs, length(lN))

    # Make new .quickPlot object.
    # This will be merged to existing later.
    newPlots <- new(".quickPlot")

    newPlots@arr <- new(".arrangement")

    newPlots@quickPlotGrobList <- lapply(1:length(lN), function(x) {
      quickPlotGrobList <- list()

      if (isQuickPlotLong[x]) {
        quickPlotGrobList[[lN[x]]] <-
          plotObjects[[match(
            names(isQuickPlotLong)[x],
            names(plotObjects)
          )]]@quickPlotGrobList[[match(
            lN[x], lNamesPlotObj[isQuickPlotLong]
          )]][[1]]
      } else {
        quickPlotGrobList[[lN[x]]] <- new(".quickPlotGrob")
        quickPlotGrobList[[lN[x]]]@plotArgs <- lapply(plotArgs, function(y) y[[x]])
        quickPlotGrobList[[lN[x]]]@plotArgs$zoomExtent <- plotArgs$zoomExtent[[x]]
        quickPlotGrobList[[lN[x]]]@plotArgs$gpText <- plotArgs$gpText[x]
        quickPlotGrobList[[lN[x]]]@plotArgs$gpAxis <- plotArgs$gpAxis[x]
        quickPlotGrobList[[lN[x]]]@plotArgs$gp <- plotArgs$gp[x]
        quickPlotGrobList[[lN[x]]]@plotName <- lN[x]
        quickPlotGrobList[[lN[x]]]@objName <- objectNamesLong[x]
        quickPlotGrobList[[lN[x]]]@envir <- lEnvs[[x]]
        quickPlotGrobList[[lN[x]]]@layerName <- lNamesPlotObj[x]

        theObj <- try(eval(parse(text = objectNamesLong[x]), lEnvs[[x]]), silent = TRUE)

        if (is(theObj, "try-error"))
          theObj <- get(objectNamesLong[x], lEnvs[[x]])

        quickPlotGrobList[[lN[x]]]@objClass <- class(theObj)
        quickPlotGrobList[[lN[x]]]@isSpatialObjects <- isSpatialObjects[x]
      }
      return(quickPlotGrobList)
    })

    names(newPlots@quickPlotGrobList) <- lN
    return(newPlots)
})

#' @rdname makeQuickPlot
setMethod(
  ".makeQuickPlot",
  signature = c(plotObjects = "list", plotArgs = "missing"),
  definition = function(plotObjects, ...) {
    plotArgs <- formals("Plot")[-1]
    newPlots <- .makeQuickPlot(plotObjects, plotArgs, ...)
    return(newPlots)
})

#' @rdname makeQuickPlot
setMethod(
  ".makeQuickPlot",
  signature = c(plotObjects = "missing", plotArgs = "missing"),
  definition = function(...) {
    newPlots <- new(".quickPlot")
    newPlots@quickPlotGrobList <- lapply(1:1, function(x) {
      quickPlotGrobList <- list()
      quickPlotGrobList[[1]] <- new(".quickPlotGrob")
      return(quickPlotGrobList)
    })
    return(newPlots)
})

#' Convert `plotArgs` to list of lists
#'
#' Internal function. Take the inputs as `plotArgs` to the Plot function, and make
#' them a list of length `numQuickPlotObjects` entries of lists.
#'
#' @inheritParams .makeQuickPlot
#'
#' @param numQuickPlotObjects Numeric. The number of `.quickPlotObjects`.
#'                 This can't easily be deduced from the `plotArgs` because
#'                 of the `RasterStack`s. So passed manually.
#'
#' @author Eliot McIntire
#' @include plotting-classes.R
#' @keywords internal
#' @rdname makeList
#'
setGeneric(".makeList", function(plotArgs, numQuickPlotObjects) {
  standardGeneric(".makeList")
})

#' @rdname makeList
setMethod(
  ".makeList",
  signature = c("list"),
  definition = function(plotArgs, numQuickPlotObjects) {
    p <- plotArgs
    n <- numQuickPlotObjects

    p$new <- if (is.list(p$new)) {
      if (length(p$new) != n) {
        rep(p$new, length.out = n)
      } else {
        p$new
      }
    } else {
      if (length(p$new) == n) {
        as.list(p$new)
      } else {
        rep(list(p$new), length.out = n)
      }
    }

    # character or logical or numeric of length 1 per entry
    p$addTo <- if (is.list(p$addTo)) {
      if (length(p$addTo) != n) {
        rep(p$addTo, length.out = n)
      } else {
        p$addTo
      }
    } else {
      if (length(p$addTo) == n) {
        as.list(p$addTo)
      } else {
        rep(list(p$addTo), length.out = n)
      }
    }

    p$gp <- if (inherits(p$gp, "gpar")) {
      rep(list(p$gp), n)
    } else {
      if (is.list(p$gp)) {
        rep(p$gp, n)
      }
    }

    p$gpText <- if (inherits(p$gpText, "gpar")) {
      rep(list(p$gpText), n)
    } else {
      if (is.list(p$gpText)) {
        rep(p$gpText, n)
      }
    }

    p$gpAxis <- if (inherits(p$gpAxis, "gpar")) {
      rep(list(p$gpAxis), n)
    } else {
      if (is.list(p$gpAxis)) {
        rep(p$gpAxis, n)
      }
    }

    p$axes <- if (is.list(p$axes)) {
      if (length(p$axes) != n) {
        rep(p$axes, length.out = n)
      } else {
        p$axes
      }
    } else {
      if (length(p$axes) == n) {
        as.list(p$axes)
      } else {
        rep(list(p$axes), length.out = n)
      }
    }

    p$speedup <- if (is.list(p$speedup)) {
      if (length(p$speedup) != n) {
        rep(p$speedup, length.out = n)
      }
      else {
        p$speedup
      }
    } else {
      if (length(p$speedup) == n) {
        as.list(p$speedup)
      } else {
        rep(list(p$speedup), length.out = n)
      }
    }

    p$size <- if (is.list(p$size)) {
      if (length(p$size) != n) {
        rep(p$size, length.out = n)
      } else {
        p$size
      }
    } else {
      if (length(p$size) == n) {
        as.list(p$size)
      } else {
        rep(list(p$size), length.out = n)
      }
    }

    p$visualSqueeze <- if (is.list(p$visualSqueeze)) {
      if (length(p$visualSqueeze) != n) {
        rep(p$visualSqueeze, length.out = n)
      } else {
        p$visualSqueeze
      }
    } else {
      if (length(p$visualSqueeze) == n) {
        as.list(p$visualSqueeze)
      } else {
        rep(list(p$visualSqueeze), length.out = n)
      }
    }

    p$legend <- if (is.list(p$legend)) {
      if (length(p$legend) != n) {
        rep(p$legend, length.out = n)
      } else {
        p$legend
      }
    } else {
      if (length(p$legend) == n) {
        as.list(p$legend)
      } else {
        rep(list(p$legend), length.out = n)
      }
    }

    p$pch <- if (is.list(p$pch)) {
      if (length(p$pch) != n) {
        rep(p$pch, length.out = n)
      } else {
        p$pch
      }
    } else {
      if (length(p$pch) == n) {
        as.list(p$pch)
      } else {
        rep(list(p$pch), length.out = n)
      }
    }

    p$title <- if (is.list(p$title)) {
      if (length(p$title) != n) {
        rep(p$title, length.out = n)
      } else {
        p$title
      }
    } else {
      if (length(p$title) == n) {
        as.list(p$title)
      } else {
        rep(list(p$title), length.out = n)
      }
    }

    p$na.color <- if (is.list(p$na.color)) { # nolint
      if (length(p$na.color) != n) {
        rep(p$na.color, length.out = n)
      } else {
        p$na.color
      }
    } else {
      if (length(p$na.color) == n) {
        as.list(p$na.color)
      } else {
        rep(list(p$na.color), length.out = n)
      }
    }

    p$zero.color <- if (is.list(p$zero.color)) { # nolint
      if (length(p$zero.color) != n) {
        rep(p$zero.color, length.out = n)
      } else {
        p$zero.color
      }
    } else {
      if (length(p$zero.color) == n) {
        as.list(p$zero.color)
      } else {
        rep(list(p$zero.color), length.out = n)
      }
    }

    p$cols <- if (is.list(p$cols)) {
      p$cols
    } else {
      rep(list(p$cols), length.out = n)
    }

    p$zoomExtent <- if (is.list(p$zoomExtent)) {
      p$zoomExtent
    } else {
      rep(list(p$zoomExtent), length.out = n)
    }

    p$legendText <- if (is.list(p$legendText)) {
      p$legendText
    } else {
      rep(list(p$legendText), length.out = n)
    }

    p$legendRange <- if (is.list(p$legendRange)) {
      p$legendRange
    } else {
      rep(list(p$legendRange), length.out = n)
    }

    p$plotFn <- if (is.list(p$plotFn)) {
      p$plotFn
    } else {
      rep(list(p$plotFn), length.out = n)
    }

    return(p)
})

#' Make `SpatialLines` object from two `SpatialPoints` objects
#'
#' The primary conceived usage of this is to draw arrows following the
#' trajectories of agents.
#'
#' @param from  Starting spatial coordinates (`SpatialPointsDataFrame`).
#'
#' @param to    Ending spatial coordinates (`SpatialPointsDataFrame`).
#'
#' @return A `SpatialLines` object. When this object is used within a
#'         `Plot` call and the `length` argument is specified, then
#'         arrow heads will be drawn. See examples.
#'
#' @author Eliot McIntire
#' @export
#' @include plotting-classes.R
#' @rdname makeLines
#'
#' @example inst/examples/example_makeLines.R
#'
makeLines <- function(from, to) {
  UseMethod("makeLines")
}

#' @export
makeLines.default <-
  #signature = c("SpatialPoints", "SpatialPoints"),
  # definition =
  function(from, to) {
    fromSpatial <- isSpatial(from)
    toSpatial <- isSpatial(to)
    # If one is Spatial, convert to SpatVector; if both Spatial, then leave
    if (fromSpatial && !toSpatial)
      from <- terra::vect(from)
    if (!fromSpatial && toSpatial)
      to <- terra::vect(to)

    if (fromSpatial && toSpatial) {
      sp::SpatialLines(lapply(seq_len(length(from)), function(x) {
        ccrds <- rbind(sp::coordinates(from)[x, ], sp::coordinates(to)[x, ])
        sp::Lines(list(sp::Line(
          coords = ccrds
        )), ID = x)
      }), proj4string = terra::crs(from)) # nolint
    } else {
      ccrds <- rbind(cbind(object = seq(NROW(from)), terra::crds(from)),
                     cbind(object = seq(NROW(to)), terra::crds(to)))
      ccrds <- ccrds[order(ccrds[, "object"]), ]
      terra::vect(ccrds, type = "lines", crs = terra::crs(from))
    }
  }


#' Parse arguments and find environments
#'
#' Internal function used within `objectNames`.
#'
#' @param y  A character representation of the arguments passed to a function, e.g., `Plot`.
#'
#' @param e  Environment in which the function (e.g., `Plot`) was called.
#'
#' @param eminus1  The parent environment of `e`.
#'
#' @return A list of length 2, with names `objs` and `envs` giving the
#' standardized representation (i.e., replacing `[[]]` with `$` notation for objects) of objects
#' and their layers (if `RasterStack`s).
#'
#' @author Eliot McIntire and Alex Chubaty
#' @importFrom grDevices dev.cur
#' @include plotting-classes.R
#' @keywords internal
#' @rdname parseArgs
.parseArgs <- function(y, e, eminus1) {
  elems <- list()
  i <- 1
  parseTxt <- parse(text = y)[[1]]
  elems[[i]] <- parseTxt
  lastOneDone <- TRUE

  while (length(parse(text = deparse(parseTxt))[[1]]) != 1) {
    if (length(parseTxt) == 2) {
      stop("Please pass an object directly, or use get(x, envir = envName) or ",
           "eval(x, envir = envName). ",
           "Plot can not yet accept functions or complex objects internally.")
    }

    lastOneDone <- FALSE
    if (grepl(deparse(parseTxt[[1]]), pattern = "^eval")) {
      callEnv <- tryCatch(
        eval(
          match.call(definition = eval, call = parseTxt)$envir,
          envir = eminus1
        ),
        error = function(x) {
          tryCatch(
            eval(
              match.call(definition = eval, call = parseTxt)$envir,
              envir = e
            ),
            error = function(x) .GlobalEnv
          )
        }
      )

      parseTxt[[3]] <- match.call(definition = eval, call = parseTxt)$expr
      if (is.name(match.call(definition = parse, call = parseTxt[[3]])$text)) {
        parseTxt <- parseTxt[[3]]
        parseTxt[[3]] <- match.call(definition = parse, call = parseTxt)$text
      }
      lastOneDone <- TRUE
    }
    if (is.call(parseTxt[[3]])) {
      parseTxt[[3]] <- tryCatch(
        eval(parseTxt[[3]], envir = e),
        error = function(x) {
          eval(parseTxt[[3]], envir = eminus1)
        }
      )
    }
    if (as.character(parseTxt[[1]]) == "[[") {
      parseTxt[[3]] <- tryCatch(
        eval(parseTxt[[3]], envir = e),
        error = function(x) {
          eval(parseTxt[[3]], envir = eminus1)
        }
      )
    }
    if (grepl(deparse(parseTxt[[1]]), pattern = "^get")) {
      callEnv <- tryCatch(
        eval(
          match.call(definition = eval,
                     call = parseTxt)$envir,
          envir = eminus1
        ),
        error = function(x) {
          tryCatch(
            eval(
              match.call(definition = eval, call = parseTxt)$envir,
              envir = e
            ),
            error = function(x) .GlobalEnv
          )
        }
      )
      parseTxt[[3]] <- match.call(definition = get, call = parseTxt)$x
      tmpParseTxt3 <- tryCatch(
        eval(parseTxt[[3]], envir = e),
        error = function(x) {
          eval(parseTxt[[3]], envir = eminus1)
        }
      )

      lastOneDone <- TRUE
      parseTxt[[3]] <- tmpParseTxt3
    }
    if (is.character(parseTxt[[3]])) {
      parseTxt[[3]] <- as.name(parseTxt[[3]])
    }
    if (is.numeric(parseTxt[[3]])) {
      if (!is.null(names(eval(parseTxt[[2]], envir = e)))) {
        parseTxt[[3]] <- names(eval(parseTxt[[2]], envir = e))[parseTxt[[3]]]
        if (is.na(parseTxt[[3]])) {
          stop("Please pass an object directly, or use get(x, envir = envName) ",
               "or eval(x, envir = envName). ",
               "Plot can not yet accept functions or complex objects internally.")
        }
      }

    }

    # override previous elems entry if length(parseTxt)>1:
    elems[[i]] <- parseTxt[[3]]

    # if evaluating the parsed text is a character,
    # then this is likely then name we want to keep:
    isChar <- tryCatch(
      is.character(eval(elems[[i]], envir = eminus1)),
      error = function(x) FALSE
    )
    if (isChar) {
      elems[[i]] <- as.name(eval(elems[[i]], envir = eminus1))
    }
    parseTxt <- parse(text = deparse(parseTxt[[2]]))[[1]]
    i <- i + 1
  }

  deparsedTxt <- deparse(parseTxt)
  sframes <- sys.frames()
  # envs <- append(.GlobalEnv, sframes) %>%
  #   .[c(TRUE, unlist(lapply(sframes, function(x) {
  #     exists(deparsedTxt, envir = x, inherits = FALSE)
  #   })))] %>%
  #   .[[length(.)]]

  envs <- append(.GlobalEnv, sframes)
  envs <- envs[c(TRUE, unlist(lapply(sframes, function(x) {
      exists(deparsedTxt, envir = x, inherits = FALSE)
    })))]
  envs <- envs[[length(envs)]]

  inGlobal <- identical(envs, .GlobalEnv)
  possEnv <- eval(parse(text = deparsedTxt), envir = envs)
  if (is.environment(possEnv)) {
    notPoss <- tryCatch(get(deparse(rev(elems)[[1]]), envir = possEnv), error = function(x) FALSE)
    if (!isFALSE(notPoss))
      envs <- possEnv
  } else {
    if (!lastOneDone) elems[[i]] <- parseTxt
  }


  if (exists("callEnv", inherits = FALSE)) {
    envs <- callEnv
  }

  if (!inGlobal) {
    devCur <- paste0("dev", dev.cur())
    if (!exists(devCur, envir = .quickPlotEnv)) {
      .quickPlotEnv[[devCur]] <- new.env(parent = emptyenv())
    }

    tmp <- get(deparse(rev(elems)[[1]]), envir = envs) ## the sim object
    .quickPlotEnv[[devCur]] <- .parseElems(tmp = tmp, elems = elems, envir = envs)
  }

  if (unlist(lapply(elems[[1]], is.numeric))) {
    return(list(objs = paste0(paste0(unlist(lapply(rev(elems), deparse)),
                                     collapse = "[["), "]]"),
                envs = envs))
  }
  return(list(objs = paste(unlist(lapply(rev(elems), deparse, backtick = TRUE)), collapse = "$"),
              envs = envs))
}

#' Parsing of elements
#'
#' This is a generic definition that can be extended according to class. Intended
#' only for development use.
#'
#' @return An object, parsed from a character string and and environment
#'
#' @param tmp A evaluated object
#' @param elems A character string to be parsed
#' @param envir An environment
#'
#' @export
#' @rdname parseElems
#' @author Eliot McIntire
setGeneric(".parseElems", function(tmp, elems, envir) {
  standardGeneric(".parseElems")
})

#' @export
#' @rdname parseElems
setMethod(
  ".parseElems",
  signature = "ANY",
  definition = function(tmp, elems, envir) {
    eval(parse(text = paste(unlist(lapply(rev(elems), deparse)), collapse = "$")), envir = envir)
})

################################################################################
#' Extracts the object names
#'
#' Internal function primarily used from `Plot`.
#'
#' @param calledFrom  character vector of length 1, indicating which function
#'                    call is desired. Defaults to `Plot`.
#'
#' @param argClass    character vector of length 1, indicating which class is
#'                    being searched for among the arguments.
#'                    Defaults to `.quickPlotObjects`.
#'
#' @param argName     character vector of length 1, or `NULL`, indicating
#'                    if the arguments to select have a name, no name (empty
#'                    string), or do not use name (`NULL`).
#'
#' @return `NULL`. This function is invoked for its side effects.
#'
#' @author Eliot McIntire
#' @export
#' @include plotting-classes.R
#' @keywords internal
#' @rdname objectNames
#'
.objectNames <- function(calledFrom = "Plot", argClass = ".quickPlotObjects",
                         argName = "") {
  scalls <- sys.calls()
  # Extract from the sys.calls only the function "calledFrom"
  frameCalledFrom <- which(unlist(lapply(scalls, function(x) {
    grepl(x, pattern = paste0("^", calledFrom, "$"))[1]
  })))
  e <- sys.frame(frameCalledFrom[1])
  eminus1 <- sys.frame(frameCalledFrom - 1)

  if (!nzchar(argName, keepNA = TRUE)) {
    callNamedArgs <- as.character(substitute(list(...), env = e))[-1]
  } else {
    callNamedArgs <- as.character(substitute(parse(text = sim), env = e))[-1]
  }
  objs <- lapply(callNamedArgs, .parseArgs, e, eminus1)
  return(objs)
}

#' Importing some grid functions
#'
#' Currently only the `gpar` function is imported. This is a convenience so that users
#' can change `Plot` arguments without having to load the entire grid package.
#'
#' @inheritParams grid::gpar
#'
#' @aliases gpar
#' @export
#' @importFrom grid gpar
#' @name gpar
#' @rdname grid-functions
#' @seealso [grid::gpar()]
#'
gpar <- grid::gpar

#' Internal functions used by `Plot`
#'
#' Extract colours, legends and other things from object, and convert to a `plotGrob`.
#'
#' @param grobToPlot Graphical object to plot
#' @param sGrob `quickPlotGrob` object
#' @param takeFromPlotObj Logical indicating whether data for `grobToPlot` should be found in
#'        current call to Plot or from disk
#' @param arr An arrangement object
#' @param newArr Logical, whether there is a new arrangement happening
#' @param quickPlotGrobCounter Numeric. A counter. No meaning outside `Plot` function.
#' @param subPlots Character. Name of plot area.
#' @param cols Colour vector.
#'
#' @aliases PlotHelpers
#' @author Eliot McIntire
#' @include plotting-classes.R
#' @keywords internal
#' @name .preparePlotGrob
#' @rdname Plot-internal
#'
.preparePlotGrob <- function(grobToPlot, sGrob, takeFromPlotObj, arr, newArr, prevMinMax,
                                        quickPlotGrobCounter, subPlots, cols) {
  UseMethod(".preparePlotGrob")
}

.preparePlotGrob.default <-
  function(grobToPlot, sGrob, takeFromPlotObj, arr, newArr, prevMinMax,
                        quickPlotGrobCounter, subPlots, cols) {

    cn <- colnames(grobToPlot)
    if (is(grobToPlot, "SpatialLines")) {
      if (!is.null(sGrob@plotArgs$zoomExtent)) {
        grobToPlot <- terra::crop(grobToPlot, sGrob@plotArgs$zoomExtent)
      }
      zMat <- list(z = grobToPlot, minz = 0, maxz = 0,
                   cols = sGrob@plotArgs$cols, real = FALSE)
    } else if (isSpatial(grobToPlot) || isSpatVector(grobToPlot)
               || !is.null(cn)) { # the last one about column names will capture e.g., agentMatrix
      if (!is.null(sGrob@plotArgs$zoomExtent) &&
          !identical(extent(grobToPlot), arr@extents[[subPlots]])) {
        grobToPlot <- terra::crop(grobToPlot, sGrob@plotArgs$zoomExtent)
      }



      # This handles SpatialPointsDataFrames with column "colour"
      if (any(grepl(pattern = "color", names(grobToPlot))) & is.null(cols))
        sGrob@plotArgs$cols <- unlist(getColors(grobToPlot))

      zMat <- list(z = grobToPlot, minz = 0, maxz = 0,
                   cols = sGrob@plotArgs$cols, real = FALSE)
    } else if (isGridded(grobToPlot)) {

      # Rasters may be zoomed into and subsampled and have unique legend
      #            if (sGrob@plotArgs$new)
      pR <- .prepareRaster(grobToPlot, sGrob@plotArgs$zoomExtent,
                           sGrob@plotArgs$legendRange, takeFromPlotObj,
                           arr, sGrob@plotArgs$speedup, newArr = newArr)
      zMat <- .makeColorMatrix(grobToPlot, pR$zoom, pR$maxpixels,
                               pR$legendRange,
                               prevMinMax = prevMinMax,
                               na.color = sGrob@plotArgs$na.color,
                               zero.color = sGrob@plotArgs$zero.color,
                               cols = sGrob@plotArgs$cols,
                               skipSample = pR$skipSample)
    } else {
      if (any(grepl(pattern = "color", colnames(grobToPlot))) & is.null(cols))
        sGrob@plotArgs$cols <- grobToPlot$color

      zMat <- list(z = grobToPlot, minz = 0, maxz = 0,
                   cols = sGrob@plotArgs$cols, real = FALSE)

    }

    return(zMat)
}


#' @param whPlotFrame Numeric. Which plot within the `quickPlotGrobPlots` object.
#'
#' @include plotting-classes.R
#' @aliases PlotHelpers
#' @keywords internal
#' @name .xyAxes
#' @rdname Plot-internal
#'
setGeneric(".xyAxes", function(sGrob, arr, whPlotFrame) {
  standardGeneric(".xyAxes")
})

#' @rdname Plot-internal
#' @aliases PlotHelpers
#' @keywords internal
setMethod(
  ".xyAxes",
  signature = c(".quickPlotGrob", ".arrangement"),
  definition = function(sGrob, arr, whPlotFrame) {
    if (sGrob@plotArgs$axes == "L") {
      if (any(sGrob@objClass %in% c("Raster", "SpatRaster")) &&
          identical(arr@extents[(whPlotFrame - 1) %% arr@columns + 1][[1]],
           arr@extents[max(
             which(
               (1:length(arr@names) - 1) %% arr@columns + 1 ==
               (whPlotFrame - 1) %% arr@columns + 1
             ))][[1]])) {
        if (whPlotFrame > (length(arr@names) - arr@columns)) {
          xaxis <- TRUE
        } else {
          xaxis <- FALSE
        }
      } else {
        # not the same extent as the final one in the column
        xaxis <- TRUE
      }
    } else {
      xaxis <- sGrob@plotArgs$axes
    }

    if (sGrob@plotArgs$axes == "L") {
      if (any(sGrob@objClass %in% c("Raster", "SpatRaster")) &
          identical(arr@extents[whPlotFrame][[1]],
           arr@extents[(ceiling(whPlotFrame / arr@columns) - 1) * arr@columns + 1][[1]])) {
        if ((whPlotFrame - 1) %% arr@columns == 0) { # nolint
          yaxis <- TRUE
        } else {
          yaxis <- FALSE
        }
      } else {
        yaxis <- TRUE
      }
    } else {
      yaxis <- sGrob@plotArgs$axes
    }

    return(list(x = xaxis, y = yaxis))
})

#' @param quickSubPlots List of many `quickPlotGrobs`
#' @param isBaseSubPlot Logical. Is the currently being plotted object a base layer
#' @param isNewPlot Logical. Is the currently being plotted object a new, additional plot
#' @param isReplot Logical. Is the currently being plotted object a replot due to something
#'                 like a rearrangement
#' @param zMat List resulting from `.preparePlotGrob`
#' @param wipe Logical. Is the currently being plotted object require a white rectangle to
#'             be plotted first, and subsequent other changes.
#' @param xyAxes List of length 2, resulting from `.xyAxes`
#' @inheritParams Plot
#' @param vps A viewport tree resulting from `.makeViewports`
#' @param nonPlotArgs Arguments passed to `Plot` that are not `.quickPlottables`,
#'                    but are passed along with `.quickPlottables`.
#' @param arr An arragement object.
#'
#' @include plotting-classes.R
#' @importFrom grid seekViewport grid.text
#' @aliases PlotHelpers
#' @keywords internal
#' @name .Plot
#' @rdname Plot-internal
#'
setGeneric(".Plot", function(sGrob, grobToPlot, subPlots, quickSubPlots, quickPlotGrobCounter,
                             isBaseSubPlot, isNewPlot, isReplot, zMat, wipe, xyAxes, legendText,
                             vps, nonPlotArgs, arr) {
  standardGeneric(".Plot")
})

#' @rdname Plot-internal
#' @aliases PlotHelpers
setMethod(
  ".Plot",
  signature = c(".quickPlotGrob"),
  definition = function(sGrob, grobToPlot, subPlots, quickSubPlots, quickPlotGrobCounter,
                        isBaseSubPlot, isNewPlot, isReplot, zMat, wipe, xyAxes, legendText,
                        vps, nonPlotArgs, arr) {

    seekViewport(subPlots, recording = FALSE)

    if (is(grobToPlot, "list") || is(grobToPlot, "gg")) {
      # This is for base plot calls... the grobToPlot is a call i.e,. a name
      # Because base plotting is not set up to overplot,
      # must plot a white rectangle

      #gf <- try(gridBase::gridFIG())
      gf <- c(0.0033, 0.9767, 0.0233, 0.8750)
      wh <- which(names(arr) %in% subPlots)
      gf <- adjustGridFIG(gf, nCols = arr@columns, nRows = arr@rows, wh = wh)


      if (is(gf, "try-error")) {
        if (identical(names(dev.cur()), "RStudioGD")) {
          stop("quickPlot sometimes has trouble with plotting base plots ",
               "in an RStudio window; try ",
               "using a new device with: ",
               "\nrePlot(toDev = dev(noRStudioGD = TRUE))")
        }
      }
      par(fig = gf)
      sGrob@plotArgs[names(grobToPlot)] <- grobToPlot

      # clear out all arguments that don't have meaning in plot.default
      if (inherits(grobToPlot, "gg")) {
        print(grobToPlot, vp = subPlots)
        a <- try(seekViewport(subPlots, recording = FALSE))
      } else {
        # plot y and x axes should use deparse(substitute(...)) names
        if (!identical(FALSE, sGrob@plotArgs$axes)) {
          if (!is.na(sGrob@plotArgs$axisLabels["x"])) {
            sGrob@plotArgs$xlab <- sGrob@plotArgs$axisLabels["x"]
          } else {
            if (!is.na(sGrob@plotArgs$axisLabels[1]))
              sGrob@plotArgs$xlab <- sGrob@plotArgs$axisLabels[1]
            else
              sGrob@plotArgs$xlab <- NULL
          }
          if (!is.na(sGrob@plotArgs$axisLabels["y"])) {
            sGrob@plotArgs$ylab <- sGrob@plotArgs$axisLabels["y"]
          } else {
            if (!is.na(sGrob@plotArgs$axisLabels[2]))
              sGrob@plotArgs$ylab <- sGrob@plotArgs$axisLabels[2]
            else
              sGrob@plotArgs$ylab <- NULL
          }
        } else {
          sGrob@plotArgs$xlab <- ""
          sGrob@plotArgs$ylab <- ""
        }

        isHist <- FALSE
        if (!is.null(grobToPlot$x)) {
          if (inherits(grobToPlot$x, "histogram")) {
            isHist <- TRUE
            sGrob@plotArgs$ylab <- if (is.null(sGrob@plotArgs$ylab)) "Frequency"
          } else if (is.numeric(grobToPlot$x)) {
            if (length(sGrob@plotArgs$axisLabels) == 1) {
              sGrob@plotArgs$ylab <- sGrob@plotArgs$xlab
              sGrob@plotArgs$xlab <- "Index"
            }
          }
        }

        argsPlot1 <- sGrob@plotArgs[!(names(sGrob@plotArgs) %in%
                                         c("new", "addTo", "gp", "gpAxis", "axisLabels",
                                         "zoomExtent", "gpText", "speedup", "size",
                                         "cols", "visualSqueeze", "legend", "legendRange",
                                         "legendText", "zero.color", "length", "arr",
                                         "na.color", "title", "userProvidedPlotFn",
                                         "minz", "maxz"))]
        argsPlot1$axes <- isTRUE(sGrob@plotArgs$axes)
        makeSpaceForAxes <- as.numeric(
          !identical(FALSE, quickSubPlots[[subPlots]][[1]]@plotArgs$axes)
        )
        par(plt = c(0.18 + makeSpaceForAxes * 0.05, # left
                    0.95,                          # right
                    0.25 + makeSpaceForAxes * 0.05, # bottom
                    0.9))                         # top

        plotFn <- argsPlot1$plotFn
        argsPlot1$plotFn <- NULL

        # base plots can't use minz, maxz
        # if (isBaseSubPlot)
        # argsPlot1 <- argsPlot1[setdiff(names(argsPlot1), c("minz", "maxz"))]

        # The actuall plot calls for base plotting
        if (inherits(grobToPlot, "igraph")) {
          # this next is a work around that I can't understand
          if (names(dev.cur()) == "null device") {
            plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
            clearPlot()
          }
          suppressWarnings(par(new = TRUE))
          plotCall <- list(x = grobToPlot)
          suppressWarnings(do.call(plot, args = plotCall))

        } else if (quickPlotGrobCounter == 1 | wipe | isHist) {
          suppressWarnings(par(new = TRUE))

          # This is a work around because I am not able to generically
          #  assess the formals of a function to remove any that aren't
          #  defined for that method... i.e., plot is the generic, but
          #  plot.igraph has different formals. Some of the functions
          #  are not exported, so their formals can't be found algorithmically
          tryCatch(do.call(plotFn, args = argsPlot1), error = function(x) {
            parsRm <- unlist(strsplit(gsub(x,
                                           pattern = ".*Unknown plot parameters: ",
                                           replacement = ""), split = ", "))
            parsRm <- gsub(parsRm, pattern = "\n", replacement = "")
            argsPlot1 <- argsPlot1[!(names(argsPlot1) %in% parsRm)]
            do.call(plotFn, args = argsPlot1)
          })
        } else {
          # adding points to a plot
          tmpPlotFn <- if (plotFn == "plot") "points" else plotFn
          argsPlot1[c("axes", "xlab", "ylab", "plotFn")] <- NULL
          suppressWarnings(do.call(tmpPlotFn, args = argsPlot1))
        }
      }

      if (any(unlist(xyAxes)) & (isBaseSubPlot & (isNewPlot | isReplot) | wipe)) {
        if (xyAxes$x | xyAxes$y & (isBaseSubPlot & (isNewPlot | isReplot) | wipe)) {
          axesArgs <- sGrob@plotArgs
          axesArgs$side <- 1
          axesArgs <- axesArgs[names(axesArgs) %in% c(
            "at", "labels", "tick", "line", "pos", "outer", "font",
            "lty", "lwd", "lwd.ticks", "col.ticks", "hadj", "padj")]
        }

        if (xyAxes$x & (isBaseSubPlot & (isNewPlot | isReplot) | wipe)) {
          axesArgsX <- append(list(side = 1), axesArgs)
          suppressWarnings(do.call(axis, args = axesArgsX))
        }
        if (xyAxes$y & (isBaseSubPlot & (isNewPlot | isReplot) | wipe)) {
          axesArgsY <- append(list(side = 2), axesArgs)
          suppressWarnings(do.call(axis, args = axesArgsY))
        }
      }
    } else {
      # This is for Rasters and Sp objects only
      # Extract legend text if the raster is a factored raster
      if (is.null(legendText)) {
        if (is.null(sGrob@plotArgs$legendTxt)) {
          if (any(terra::is.factor(grobToPlot))) {
            if (all(na.omit(grobToPlot[]%%1)==0)) {
              sGrob@plotArgs$legendTxt <- terra::levels(grobToPlot)[[1]]
            }
          }
        }
      } else {
        sGrob@plotArgs$legendTxt <- legendText
        cTxt <- legendText
      }

      if (!isBaseSubPlot) {
        sGrob@plotArgs$legendTxt <- NULL
      }

      plotGrobCall <- list(grobToPlot = zMat$z, col = zMat$cols,
                           size = unit(sGrob@plotArgs$size, "points"),
                           real = zMat$real,
                           minv = zMat$minz, maxv = zMat$maxz,
                           pch = sGrob@plotArgs$pch, name = subPlots,
                           vp = vps,
                           legend = #sGrob@plotArgs$legend  &  isBaseSubPlot &
                             #isReplot |
                             sGrob@plotArgs$legend & (isBaseSubPlot &
                                                        (isNewPlot | wipe | isReplot)),
                           legendText = sGrob@plotArgs$legendTxt,
                           gp = sGrob@plotArgs$gp,
                           gpText = sGrob@plotArgs$gpText,
                           speedup = sGrob@plotArgs$speedup,
                           length = sGrob@plotArgs$length)
      plotGrobCall <- append(x = plotGrobCall, nonPlotArgs)

      seekViewport(subPlots, recording = FALSE)
      suppressWarnings(do.call(.plotGrob, args = plotGrobCall))

      if (any(unlist(xyAxes)) & (isBaseSubPlot & (isNewPlot | isReplot) | wipe)) {
        seekViewport(paste0("outer", subPlots), recording = FALSE)
        if (xyAxes$x & (isBaseSubPlot & (isNewPlot | isReplot) | wipe)) {
          grid.xaxis(name = "xaxis", gp = sGrob@plotArgs$gpAxis)
        }
        if (xyAxes$y & (isBaseSubPlot & (isNewPlot | isReplot) | wipe)) {
          grid.yaxis(name = "yaxis", gp = sGrob@plotArgs$gpAxis,
                     vp = vps$wholeVp$children[[paste0("outer", subPlots)]])
        }
        seekViewport(subPlots, recording = FALSE)
      }
    } #gg vs histogram vs spatialObject

    # print Title on plot
    if (is.null(sGrob@plotArgs$title)) {
      sGrob@plotArgs$title <- TRUE
    }
    if (!identical(FALSE, sGrob@plotArgs$title) && (isBaseSubPlot & (isNewPlot | isReplot))) {
      plotName <- if (isTRUE(sGrob@plotArgs$title)) sGrob@plotName else sGrob@plotArgs$title
      a <- try(seekViewport(paste0("outer", subPlots), recording = FALSE))
      suppressWarnings(grid.text(plotName, name = "title",
                                 y = 1.08 - is.list(grobToPlot) * 0.02,
                                 vjust = 0.5, # tweak... not good practice.
                                              # Should find original reason why this is
                                              # not same y for rasters and all others
                                 gp = sGrob@plotArgs$gpText))
      a <- try(seekViewport(subPlots, recording = FALSE))
    }
    return(sGrob)
})

#' @param nColumns Numeric, length 1, indicating how many columns are in the device arrangement
#' @param nRows Numeric, length 1, indicating how many rows are in the device arrangement
#' @param whPlotObj Numeric. Length 1, indicating which of the currently objects passed into
#'                  `Plot` is currently being plotted, i.e., a counter of sorts.
#' @param whExistingObj Numeric. Like `whPlotObj`, but for whole existing plot, not just supplied in
#'   current call.
#'
#' @include plotting-classes.R
#' @inheritParams .makeQuickPlot
#' @aliases PlotHelpers
#' @keywords internal
#' @name .refreshGrob
#' @rdname Plot-internal
#'
setGeneric(".refreshGrob", function(sGrob, subPlots, legendRange,
                                    grobToPlot, plotArgs, nColumns, nRows, whPlotObj,
                                    whExistingObj) {
  standardGeneric(".refreshGrob")
})

#' @rdname Plot-internal
#' @aliases PlotHelpers
#' @keywords internal
setMethod(
  ".refreshGrob",
  signature = c(".quickPlotGrob"),
  definition = function(sGrob, subPlots, legendRange,
                        grobToPlot, plotArgs, nColumns, nRows, whPlotObj, whExistingObj) {
    # seekViewport(paste0("outer", subPlots), recording = FALSE)
    needsNewTitle <- sGrob@plotArgs$new != FALSE
    seekViewport("top", recording = FALSE)
    gf <- adjustGridFIG(c(0, 1, 0, 1), nCols = nColumns, nRows = nRows, whExistingObj)
    grid.rect(x = unit(gf[1], "npc"), y = unit(gf[3], "npc"),
              height = unit(gf[4] - gf[3], "npc"),
              width = unit(gf[2] - gf[1], "npc"),
              gp = gpar(fill = "white", col = "white"),
              just = c(0,0))

    # grid.rect(x = 0, height = unit(1 + needsNewTitle * inherits(grobToPlot, "Raster") * 0.20 / (nRows / 2), "npc"),
    #           width = unit(1 + inherits(grobToPlot, "Raster") * 0.20 / (nColumns / 2), "npc"),
    #           gp = gpar(fill = "white", col = "white"), just = "left")
    plotArgsByPlot <- lapply(plotArgs, function(x) {
      if (is.list(x)) {
        if (length(x) > 1) {
          return(x[whPlotObj])
        }}
      x
    })
    sGrob@plotArgs[names(plotArgs)] <- plotArgsByPlot
    sGrob@plotArgs$new <- FALSE
    sGrob@plotArgs$legendRange <- if (is.null(legendRange)) {
      NULL
    } else if (is.list(legendRange) & length(legendRange) > 1) {
      legendRange[[whPlotObj]]
    } else {
      legendRange
    }
    sGrob@plotArgs$legendTxt <- NULL
    seekViewport(subPlots, recording = FALSE)
    return(sGrob)
})

#' @include plotting-classes.R
#' @aliases PlotHelpers
#' @keywords internal
#' @name .updateGrobGPTextAxis
#' @rdname Plot-internal
setGeneric(".updateGrobGPTextAxis", function(sGrob, arr, newArr) {
  standardGeneric(".updateGrobGPTextAxis")
})

#' @aliases PlotHelpers
#' @rdname Plot-internal
setMethod(
  ".updateGrobGPTextAxis",
  signature = c(".quickPlotGrob"),
  definition = function(sGrob, arr, newArr) {

    if (!inherits(sGrob@plotArgs$gpText, "gpar")) {
      sGrob@plotArgs$gpText <- as(sGrob@plotArgs$gpText, "gpar")
    }
    if (!inherits(sGrob@plotArgs$gpAxis, "gpar")) {
      sGrob@plotArgs$gpAxis <- as(sGrob@plotArgs$gpAxis, "gpar")
    }
    if (!inherits(sGrob@plotArgs$gp, "gpar")) {
      sGrob@plotArgs$gp <- as(sGrob@plotArgs$gp, "gpar")
    }

    if (is.null(sGrob@plotArgs$gpText$cex) | newArr) {
      # pipe won't work here :S
      sGrob@plotArgs$gpText$cex <- max(
        0.6,
        min(1.2, sqrt(prod(arr@ds) / prod(arr@columns, arr@rows)) * 0.3)
      )
    }
    if (is.null(sGrob@plotArgs$gpAxis$cex) | newArr) {
      # pipe won't work here :S
      sGrob@plotArgs$gpAxis$cex <- max(
        0.6,
        min(1.2, sqrt(prod(arr@ds) / prod(arr@columns, arr@rows)) * 0.3)
      )
    }
    return(sGrob)
})

#' Identify where to get the grob from
#'
#' Internal function.
#'
#' Because the `Plot` function can use the global environment as a source of
#' objects to plot, not just the call itself, this function identifies where the
#' data for the grob should come from, the current call or the global environment.
#'
#' @param toPlot The object to plot. Should be a single layer if from a multi-layer
#'               object such as a `RasterStack`.
#' @param sGrob `quickPlot` grob object
#' @param takeFromPlotObj Logical. Should the data come from the argument passed
#'                        into `Plot` (`TRUE`), or from the (`.quickPlotEnv`) (`FALSE`).
#'
#' @author Eliot McIntire
#' @include plotting-classes.R
#' @keywords internal
#' @export
#' @rdname identifyGrobToPlot
setGeneric(".identifyGrobToPlot", function(toPlot, sGrob, takeFromPlotObj) {
  standardGeneric(".identifyGrobToPlot")
})

#' @rdname identifyGrobToPlot
setMethod(
  ".identifyGrobToPlot",
  signature = c("ANY", ".quickPlotGrob"),
  function(toPlot, sGrob, takeFromPlotObj) {
    ## get the object name associated with this grob

    if (length(toPlot) == 0) takeFromPlotObj <- FALSE

    # Does it already exist on the plot device or not
    if (nzchar(sGrob@layerName, keepNA = TRUE)) {
      # means it is in a raster
      if (takeFromPlotObj) {
          grobToPlot <- unlist(toPlot, recursive = FALSE)[[sGrob@layerName]]
      } else {
          grobToPlot <- eval(parse(text = sGrob@objName),
                           sGrob@envir)[[sGrob@layerName]]
      }
    } else {
      if (takeFromPlotObj) {
        if (!inherits(toPlot, "gg") && !inherits(toPlot, "igraph") && is(toPlot, "list")) {
          grobToPlot <- unlist(toPlot, recursive = FALSE)
        } else {
          grobToPlot <- toPlot
        }
      } else {
        grobToPlot <- eval(parse(text = sGrob@objName), sGrob@envir)
      }
    }
    return(grobToPlot)
})

#' Prepare raster for plotting
#'
#' Internal function. Takes a raster `.quickPlotGrob`, and converts `zoomExtent` into
#' a zoom, and `legendRange` into a legend.
#' Then calculates the `maxpixels` to plot for speed.
#'
#' @param grobToPlot A `.quickPlotGrob` object.
#'
#' @param zoomExtent an extent object
#'
#' @param legendRange a numeric vector of length >=2 indicating the desired legend range.
#'
#' @param takeFromPlotObj logical. Should the object be found in the `Plot` call or
#'   `.GlobalEnv`
#'
#' @param arr an `.arrangement` object
#'
#' @param speedup numeric, greater than 1 will usually speed up plotting at the
#'                expense of resolution
#'
#' @param newArr logical, whether this is a new arrangement or just adding to a previous one
#'
#' @author Eliot McIntire
#' @keywords internal
#' @include plotting-classes.R
#' @rdname prepareRaster
.prepareRaster <- function(grobToPlot, zoomExtent, legendRange,
                           takeFromPlotObj, arr, speedup, newArr) {
  if (is.null(zoomExtent)) {
    zoom <- extent(grobToPlot)
    npixels <- terra::ncell(grobToPlot)
  } else {
    zoom <- zoomExtent
    npixels <- terra::ncell(terra::crop(grobToPlot, zoom))
  }
  if (is.null(legendRange)) {
    legendRange <- NA
  }

  if (speedup > 0.1) {
    maxpixels <- min(5e5, 3e4 / (arr@columns * arr@rows) * prod(arr@ds))
    maxpixels <- maxpixels/speedup
    maxpixels <- min(maxpixels, npixels)
    # maxpixels <- min(5e5, 3e4 / (arr@columns * arr@rows) * prod(arr@ds)) %>%
    #   `/`(., speedup) %>%
    #   min(., npixels)
  } else {
    maxpixels <- npixels
  }
  skipSample <- if (is.null(zoomExtent)) {
    maxpixels >= npixels
  } else {
    FALSE
  }

  return(list(maxpixels = maxpixels, skipSample = skipSample,
              legendRange = legendRange, zoom = zoom))
}

#' Merge two quickPlot objects
#'
#' Merges two `.quickPlot` objects
#'
#' @param newSP  The "new" `.quickPlot` object.
#'               I.e., the new merges and overwrites into current.
#'
#' @param curr   The "current" `.quickPlot` object.
#'               I.e., the one to be merged into.
#'
#' @param ...    Additional arguments. Currently none implemented.
#'
#' @importFrom stats na.omit
#' @include plotting-classes.R
#' @author Eliot McIntire
#' @keywords internal
#' @rdname updateQuickPlot
setGeneric(".updateQuickPlot", function(newSP, curr, ...) {
  standardGeneric(".updateQuickPlot")
})

#' @rdname updateQuickPlot
setMethod(
  ".updateQuickPlot",
  signature = c(newSP = ".quickPlot", curr = "list"),
  definition = function(newSP, curr, ...) {
    newNames <- names(newSP@quickPlotGrobList)
    currNames <- names(curr$curr@quickPlotGrobList)

    addToPlots <- unlist(lapply(newSP@quickPlotGrobList, function(x) {
      !is.null(x[[1]]@plotArgs$addTo)
    }))

    addToPlotsNames <- unlist(lapply(newSP@quickPlotGrobList, function(x) {
      x[[1]]@plotArgs$addTo
    })) |> unlist() # nolint

    if (length(addToPlotsNames) == length(newNames)) {
      overplots <- integer(0)
    } else {
      overplots <- na.omit(match(currNames, newNames))
      addToPlots1 <- na.omit(match(currNames, addToPlotsNames))
      overplots <- overplots[!(overplots %in% addToPlots1)]
    }

    needNew <- -c(overplots, which(addToPlots))
    if (length(needNew) == 0) {
      needNew <- 1:length(newNames)
    }

    whichParamsChanged <- lapply(newNames[overplots], function(x) {
      plotArgsNames <- names(newSP@quickPlotGrobList[[x]][[1]]@plotArgs)
      aa <- unlist(lapply(plotArgsNames, function(y) {
        if (!is.null(newSP@quickPlotGrobList[[x]][[1]]@plotArgs[[y]])) {
          !identical(newSP@quickPlotGrobList[[x]][[1]]@plotArgs[[y]],
                     curr$curr@quickPlotGrobList[[x]][[1]]@plotArgs[[y]])
        } else {
          FALSE
        }
      }))
      names(aa) <- plotArgsNames
      aa
    })
    names(whichParamsChanged) <- newNames[overplots]

    # # Set FALSE as default for needPlotting
    needPlotting <- lapply(curr$curr@quickPlotGrobList, function(x) {
      lapply(x, function(y) FALSE)
    })
    #
    # # Set FALSE as default for isReplot
    isReplot <- lapply(curr$curr@quickPlotGrobList, function(x) {
      lapply(x, function(y) FALSE)
    })

    isNewPlot <- lapply(curr$curr@quickPlotGrobList, function(x) {
      lapply(x, function(y) FALSE)
    })

    isBaseLayer <- curr$isBaseLayer

    # For overplots
    for (plots in newNames[overplots]) {
      # update only those plotArgs that have changed.
      idNames <- names(whichParamsChanged[[plots]])[whichParamsChanged[[plots]]]
      curr$curr@quickPlotGrobList[[plots]][[1]]@plotArgs[idNames] <-
        newSP@quickPlotGrobList[[plots]][[1]]@plotArgs[idNames]

      needPlotting[[plots]][[plots]] <- TRUE
      isReplot[[plots]][[plots]] <- FALSE
      isNewPlot[[plots]][[plots]] <- FALSE
    }

    # put addTo plots into list of quickPlotGrobs that it will be added to
    if (!is.null(addToPlotsNames)) {
      for (plots in 1:length(addToPlotsNames)) {
        a2p <- addToPlotsNames[plots]
        nA2P <- names(a2p)

        curr$curr@quickPlotGrobList[[a2p]][nA2P] <- newSP@quickPlotGrobList[[nA2P]]

        # change the name of the plotName to the parent object
        curr$curr@quickPlotGrobList[[a2p]][[nA2P]]@plotName <-
          curr$curr@quickPlotGrobList[[a2p]][[1]]@plotName
        needPlotting[[a2p]][[nA2P]] <- TRUE
        isReplot[[a2p]][[nA2P]] <- FALSE
        isBaseLayer[[a2p]][[nA2P]] <- FALSE
        isNewPlot[[a2p]][[nA2P]] <- FALSE
      }
    }

    # for new plots
    for (plots in newNames[needNew]) {
      curr$curr@quickPlotGrobList[[plots]] <- newSP@quickPlotGrobList[[plots]]
      needPlotting[[plots]] <- TRUE
      isReplot[[plots]] <- FALSE
      isBaseLayer[[plots]] <- TRUE
      isNewPlot[[plots]] <- TRUE
    }
    return(
      list(
        curr = curr$curr, whichParamsChanged = whichParamsChanged,
        needPlotting = needPlotting, isReplot = isReplot,
        isBaseLayer = isBaseLayer, isNewPlot = isNewPlot
      )
    )
})

#' @rdname updateQuickPlot
setMethod(
  ".updateQuickPlot",
  signature = c(newSP = ".quickPlot", curr = "missing"),
  definition = function(newSP, ...) {

    return(list(
      curr = newSP, whichParamsChanged = NULL,
      needPlotting = lapply(newSP@quickPlotGrobList, function(x) {
        lapply(x, function(y) TRUE)
      }),
      isReplot = lapply(newSP@quickPlotGrobList, function(x) {
        lapply(x, function(y) FALSE)
      }),
      isNewPlot = lapply(newSP@quickPlotGrobList, function(x) {
        lapply(x, function(y) TRUE)
      }),
      isBaseLayer = lapply(newSP@quickPlotGrobList, function(x) {
        lapply(x, function(y) TRUE)
      })
    ))
})

#' Determine optimal plotting arrangement of plot objects
#'
#' Internal function. Assesses the device geometry, the map geometry, and the
#' number of spatial objects to plot and builds an object that will be used by
#' the Plot functions to plot them efficiently.
#'
#' @param sPlot A `.quickPlot` object.
#' @inheritParams Plot
#'
#' @author Eliot McIntire
#' @export
#' @importFrom grDevices dev.cur dev.new dev.size
#' @include plotting-classes.R
#' @keywords internal
#' @rdname arrangeViewports
#'
setGeneric(".arrangeViewports", function(sPlot, arr=NULL) {
  standardGeneric(".arrangeViewports")
})

#' @export
#' @rdname arrangeViewports
setMethod(
  ".arrangeViewports",
  signature = c(".quickPlot"),
  definition = function(sPlot, arr) {
    ds <- dev.size()
    ds.ratio <- ds[1] / ds[2] # nolint

    sgl <- sPlot@quickPlotGrobList

    dimx <- apply(do.call(
      rbind, sapply(1:length(sgl), function(x) {
        lapply(sgl[[x]][[1]]@isSpatialObjects, function(z) {
          .hasBbox(z, sgl[[x]][[1]]@objClass,
                   sgl[[x]][[1]]@objName,
                   sgl[[x]][[1]]@envir)

        })
      })), 2, max)

    dimensionRatio <- dimx[1] / dimx[2]

    dsDimensionRatio <- ds.ratio / dimensionRatio # nolint
    if (is.null(arr)) {

      nPlots <- length(sgl)
      names <- names(sgl)

      if (dev.cur() == 1) {
        dev.new(height = 8, width = 10)
      }

      colByRow <- data.frame(matrix(ncol = 2, nrow = nPlots))

      colByRow[, 1] <- ceiling(nPlots / (1:nPlots))
      colByRow[, 2] <- ceiling(nPlots / colByRow[, 1])

      ## rewritten for clarity/brevity with pipes below -- rewritten again because too much piping not clear
      whBest <- apply(colByRow, 1, function(x) x[1] / x[2])
      whBest <- (whBest - dsDimensionRatio) |>
        abs() |>
        which.min()

      # whBest <- apply(colByRow, 1, function(x) x[1] / x[2]) %>%
      #   `-`(., dsDimensionRatio) %>%
      #   abs() %>%
      #   which.min()

      columns <- colByRow[whBest, 1]
      rows <- colByRow[whBest, 2]
    } else {
      columns <- arr[2]
      rows <- arr[1]
    }

    actualRatio <- columns / rows

    out <- new(
      ".arrangement", rows = rows, columns = columns,
      actual.ratio = actualRatio,
      ds.dimensionRatio = dsDimensionRatio,
      ds = ds
    )
    return(out)
})

#' Plot spatial grobs (using \pkg{grid} package)
#'
#' Internal function. Plot a raster Grob, a points Grob, polygon Grob.
#'
#' `speedup` is only used for `SpatialPolygons`, `SpatialPoints`,
#' and `SpatialLines` in this function.
#' Attempts have been made to subsample at a good level that optimizes speed of
#' plotting, without losing visible quality. Nevertheless, to force all points to
#' be plotted, use a speedup value less than 0.1.
#' From a speed perspective, there appears to be an optimal subsampling when
#' using `thin` from the \pkg{fastshp} package.
#' Presumably, too much thinning requires large distance matrices to be
#' calculated, slowing plotting down.
#' Too little thinning causes an overabundance of points to be plotted, slowing
#' plotting down.
#'
#' The suggested package `fastshp` can be installed with:
#' `install.packages("fastshp", repos = "https://rforge.net", type = "source")` or
#' for binary `install.packages("fastshp", repos = "https://PredictiveEcology.r-universe.dev")`
#'
#' NOTE: you may get errors relating to not having installed the software tools
#' required for building R packages on your system.
#' For building on Windows, you'll need to install `Rtools` from
#' <https://cran.r-project.org/bin/windows/Rtools/>.
#'
#' @param grobToPlot  `Raster*`, `SpatialLines*`,
#'                    `SpatialPoints*`, or `SpatialPolygons*` object.
#'
#' @param col     Currently only used for the legend of a `Raster*` object.
#'
#' @param size    The size of the `SpatialPoints`.
#'
#' @param gp      `grid` parameters, usually the output of a call to [gpar()].
#'
#' @param gpText  `gpar` object for legend label text.
#'
#' @param legend  Logical indicating whether a legend should be drawn.
#'                Default `TRUE`.
#'
#' @param legendText  Vector of values to use for legend value labels.
#'                    Defaults to `NULL` which results in a pretty numeric
#'                    representation. If `Raster*` has a Raster Attribute
#'                    Table (rat; see \pkg{raster} package), this will be used
#'                    by default. Currently, only a single vector is accepted.
#'
#' @param length  Numeric.
#'
#' @param minv    The minimum value on a `Raster*`. Required because not
#'                all Rasters have this defined internally.
#'
#' @param maxv    The maximum value on a `Raster*`. Required because not
#'                all Rasters have this defined internally.
#'
#' @param pch     Point character for `SpatialPoints`, as `par`.
#'
#' @param real    Logical indicating whether the data are `real` numbers
#'                (i.e., as opposed to `integer` or `factor`).
#'
#' @param speedup Numeric. The factor by which the number of vertices in
#'                `SpatialPolygons` and `SpatialLines*` will be
#'                subsampled. The vertices are already subsampled by default to
#'                make plotting faster.
#'
#' @param vp      whole viewport tree of `quickPlotGrob`
#'
#' @param name    Character string of name of object being plotted.
#'
#' @param ...     Additional arguments. None currently implemented.
#'
#' @author Eliot McIntire
#' @export
#' @inheritParams Plot
#' @importFrom data.table ':=' data.table
#' @importFrom grDevices as.raster
#' @importFrom grid gpar gTree gList rasterGrob textGrob grid.draw
#' @keywords internal
#' @rdname plotGrob
#'
# setGeneric(
.plotGrob <-
  function(grobToPlot, col = NULL, real = FALSE, size = unit(5, "points"), minv, maxv,
           legend = TRUE, legendText = NULL, length = NULL, gp = gpar(), gpText = gpar(),
           pch = 19, speedup = 1, name = character(), vp = list(), ...,
           verbose = getOption("quickPlot.verbose")) {
    UseMethod(".plotGrob")
    # standardGeneric(".plotGrob")
  }# )

#' @export
.plotGrob.default <- function(grobToPlot, col, real, size, minv, maxv,
                              legend, legendText, length, gp = gpar(), gpText,
                              pch, speedup, name, vp, ..., verbose = getOption("quickPlot.verbose")) {
  isColorMatrix <- FALSE
  if (is.matrix(grobToPlot)) {
    notNA <- !is.na(grobToPlot[1,1])
    if (any(notNA)) { # quick version if possible
      firstNonNA <- grobToPlot[1,1]
    } else { # for cases where corner is NA
      notNA <- !is.na(grobToPlot[])
      firstNonNA <- grobToPlot[notNA][1]
    }
    isColorMatrix <- any(nchar(firstNonNA) %in% c(7, 9)) && any(grepl("\\#", firstNonNA)) ||
      is.character(grobToPlot)

  }
  isPolygon <- if (isSF(grobToPlot)) {
    any(grepl("POLYGON", sf::st_geometry_type(grobToPlot))) # also capture MULTIPOLYGON
  } else {
    isSpatialPolygons(grobToPlot) ||
      (is(grobToPlot, "SpatVector") && identical("polygons", terra::geomtype(grobToPlot)))
  }
  if (isPolygon) {
    outGrob <- pgSpatialPolygons(grobToPlot, col, size,
                                 legend, gp = gpar(), pch, speedup, name, vp, ..., verbose = verbose)
  } else if (is(grobToPlot, "SpatialLines") ||
             (is(grobToPlot, "SpatVector") && identical("lines", terra::geomtype(grobToPlot)))) {
    outGrob <- pgSpatialLines(grobToPlot, col, size, legend, length, gp = gpar(),
                              pch, speedup, name, vp, ..., verbose = verbose)
  } else if (isColorMatrix) {
    outGrob <- pgmatrix(grobToPlot, col, real, size, minv, maxv,
                        legend, legendText, gp, gpText, pch, name, vp, ...)
  } else { # for SpatialPoints and points SpatVector or other e.g., agentMatrix
    speedupScale <- speedupScale(grobToPlot, lonlatSU = 40 * 4.8e5, SU = 40)
    xyOrd <- coordinates(grobToPlot)

    if (!is.null(col)) {
      if (!is.null(gp)) {
        gp$col <- col # Accept col argument
      } else {
        gp <- gpar(col) #
      }
    }

    if (NROW(xyOrd) > 1e3) {
      # thin if greater than 1000 pts
      if (speedup > 0.1) {
        if (requireNamespace("fastshp", quietly = TRUE)) {
          thinned <- data.table(
            thin = fastshp::thin(xyOrd[, 1], xyOrd[, 2],
                                 tolerance = speedupScale * speedup)
          )
          xyOrd <- xyOrd[thinned$thin, ]
        } else {
          messageVerbose(messFastshape("polygon"), verbose = verbose)
          if (Sys.info()[["sysname"]] == "Windows") {
            messageVerbose(verbose = verbose,
              paste(
                "You may also need to download and install Rtools from:\n",
                " https://cran.r-project.org/bin/windows/Rtools/"
              )
            )
          }
        }
      }
    }

    outGrob <- gTree(
      grobToPlot = grobToPlot,
      children = gList(
        pointsGrob(
          x = xyOrd[, 1], y = xyOrd[, 2],
          pch = pch, size = size
        )
      ),
      gp = gp,
      cl = "plotPoint"
    )
    grid.draw(outGrob, recording = FALSE)
  }

  return(invisible(outGrob))
}

pgmatrix <- function(grobToPlot, col, real, size, minv, maxv,
                     legend, legendText, gp, gpText, pch, name, vp, ...) {

  pr <- if (real) {
    pretty(range(minv, maxv))
  } else {
    if (!is.null(legendText)) {
      nrowLegText <- NROW(legendText)
      if (NCOL(legendText) > 1) {
        # means it was a factor
        legendText$contigValue <- 1:nrowLegText
        if (nrowLegText > 20) {
          pr <- pretty(legendText$contigValue)
          pr <- pr - min(pr) + 1 # start it at one
        } else {
          legendText$contigValue
        }

      } else {
        unique(round(pretty(range(minv, maxv), n = length(legendText))))
      }
    } else {
      unique(round(pretty(range(minv, maxv))))
    }
  }

  if (NCOL(legendText) == 1) {
    # means it was not a factor
    pr <- pr[pr <= maxv & pr >= minv]
  } else {
    pr <- pr[pr <= nrowLegText & pr >= 1]
  }
  if (length(pr) == 0) pr <- seq(minv, maxv, by = 2)
  maxcol <- length(col)
  mincol <- 2

  gpText$cex <- gpText$cex * 0.6
  if (length(gpText) == 0)
    gpText <- gpar(col = "black", cex = 0.6)

  rastGrob2 <- gTree(
    grobToPlot = grobToPlot, pr = pr, col = col,
    children = gList(
      rasterGrob(
        as.raster(grobToPlot),
        interpolate = FALSE,
        name = "raster"
      ),
      if (legend) {
        if (NCOL(legendText) > 1) {
          # for factors
          colForLegend <- col[rev(legendText$contigValue - min(legendText$contigValue) + 2)]
        } else {
          colForLegend <- col[(maxcol):mincol]
        }
        rasterGrob(
          as.raster(colForLegend),
          x = 1.04, y = 0.5,
          height = 0.5, width = 0.03,
          interpolate = FALSE,
          name = "legend"
        )
      },
      if (legend) {
        txt <- if (is.null(legendText)) {
          pr
        } else {
          # factor legends
          if (NCOL(legendText) > 1) {
            legendIndex <- pr
            legendText[legendIndex, 2]
          } else {
            legendIndex <- pr - min(pr) + 1
            legendText[legendIndex]
          }
        }
        textGrob(
          txt,
          x = 1.08,
          y = if (!real) {
            # factors
            if (NCOL(legendText) > 1) {
              maxv <- legendText$contigValue[nrowLegText]
              minv <- legendText$contigValue[1]
            }
            ((pr - minv) / ((maxv + 1) - minv)) / 2 + 0.25 + 1 / # nolint
              (diff(range(minv, maxv)) + 1) / 4
          } else {
            ((pr - minv) / ((maxv) - minv)) / 2 + 0.25 # nolint
          },
          gp = gpText,
          just = "left", check.overlap =
            TRUE,
          name = "legendText"
        )
      }
    ),
    gp = gp, cl = "plotRast2"
  )

  seekViewport(paste0("outer", name), recording = FALSE)
  grid.draw(rastGrob2, recording = FALSE)

  return(invisible())
}

pgSpatialPolygons <- function(grobToPlot, col, size,
                              legend, gp = gpar(), pch, speedup, name, vp, ...,
                              verbose = getOption("quickPlot.verbose")) {
  speedupScale <- speedupScale(grobToPlot, lonlatSU = 1.2e10, SU = 2.4e4)

  # For speed of plotting
  xyOrd <- thin(grobToPlot, tolerance = speedupScale * speedup,
                           returnDataFrame = TRUE, minCoordsToThin = 1e5, ..., verbose = verbose)

  numPolys <- length(unique(xyOrd$xyOrd$poly))
  numSubPolys <- length(unique(xyOrd$xyOrd$groups))
  if (!is.null(col)) {
    gp$fill <- col
  }
  theCols <- if ((length(gp$fill) == 1 && gp$fill %in% rownames(RColorBrewer::brewer.pal.info)) ||
                 is.null(gp$fill)) {
    if (is.null(gp$fill)) {
      pal <- "Set2"
    } else {
      pal <- gp$fill
    }
    if (pal %in% rownames(RColorBrewer::brewer.pal.info)) {
      numCols <- RColorBrewer::brewer.pal.info[pal,"maxcolors"]
    }
    rep(RColorBrewer::brewer.pal(numCols, pal), length.out = numPolys)
  } else {
    if (length(gp$fill) < numPolys) {
      messageVerbose("not enough colours for each polygon (there are ",numPolys,"), recycling",
                     verbose = verbose)
      rep(gp$fill, length.out = numPolys)
    } else if (length(gp$fill) > numPolys) {
      messageVerbose("more colours than number of polygons (there are ",numPolys,"), setting unique colors to sub-polygons (there are "
              ,numSubPolys,")", verbose = verbose)
      if (length(gp$fill) != numSubPolys) {
        messageVerbose("Incorrect number of colours for number of sub-polygons; recycling",
                       verbose = verbose)
      }
      rep(gp$fill, length.out = numSubPolys)
    } else {
      gp$fill
    }
  }

  if (length(gp$fill) <= numPolys) {
    dtFill <- data.table(poly = seq(numPolys),
                         col = theCols)
    dtFill2 <- unique(xyOrd$xyOrd[, c("groups", "poly")])
    dtFill2 <- dtFill[dtFill2, on = "poly"]
    gp$fill <- dtFill2$col
  } else if (length(gp$fill) < NROW(xyOrd$idLength)) {
    gp$fill <- theCols
  }

  polyGrob <- .createPolygonGrob(gp = gp, xyOrd = xyOrd)
  grid.draw(polyGrob, recording = FALSE)
  return(invisible(polyGrob))
}

pgSpatialLines <- function(grobToPlot, col, size,
                           legend, length, gp = gpar(), pch, speedup, name, vp, ...,
                           verbose = getOption("quickPlot.verbose")) {
  speedupScale <- speedupScale(grobToPlot, lonlatSU = 1.2e10, SU = 2.4e4)
  # speedupScale <- if (grepl(proj4string(grobToPlot), pattern = "longlat")) {
  #   pointDistance(
  #     p1 = c(xmax(extent(grobToPlot)), ymax(extent(grobToPlot))),
  #     p2 = c(xmin(extent(grobToPlot)), ymin(extent(grobToPlot))),
  #     lonlat = TRUE
  #   ) / 1.2e10
  # } else {
  #   max(ymax(extent(grobToPlot)) - ymin(extent(grobToPlot)),
  #       xmax(extent(grobToPlot)) - xmin(extent(grobToPlot))) / 2.4e4
  # }

  if (isSpat(grobToPlot)) {
    xy <- terra::geom(grobToPlot)
    idLength <- as.data.table(xy)[, .N, by = c("geom", "part")]$N
    xy <- xy[, c("x", "y")]
  } else {
    # For speed of plotting
    xy <- lapply(1:length(grobToPlot), function(i) {
      grobToPlot@lines[[i]]@Lines[[1]]@coords
    })
    idLength <- unlist(lapply(xy, length)) / 2
    xy <- do.call(rbind, xy)
  }

  if (NROW(xy) > 1e3) {
    # thin if fewer than 1000 pts
    if (speedup > 0.1) {
      if (requireNamespace("fastshp", quietly = TRUE)) {
        thinned <- fastshp::thin(xy[, 1], xy[, 2],
                                 tolerance = speedupScale * speedup)

        # keep first and last points of every polyline,
        # if there are fewer than 10,000 vertices
        if (sum(thinned) < 1e4) {
          lastIDs <- cumsum(idLength)

          # Ensure first and last points of each line are kept:
          thinned[c(1, lastIDs + 1)[-(1 + length(lastIDs))]] <- TRUE # nolint
          thinned[lastIDs] <- TRUE
        }
        xy <- xy[thinned, ]
        idLength <- tapply(thinned, rep(1:length(idLength), idLength), sum)
      } else {
        messageVerbose(messFastshape("lines"), verbose = verbose)
        if (Sys.info()[["sysname"]] == "Windows") {
          messageVerbose(
            paste(
              "You may also need to download and install Rtools from:\n",
              "  https://cran.r-project.org/bin/windows/Rtools/"
            )
          )
        }
      }
    }
  }

  if (is.null(length)) {
    lineGrob <- gTree(children = gList(
      polylineGrob(
        x = xy[, 1], y = xy[, 2], id.lengths = idLength,
        gp = gp, default.units = "native"
      )
    ),
    gp = gp,
    cl = "plotLine")
  } else {
    lineGrob <- gTree(children = gList(
      polylineGrob(
        x = xy[, 1], y = xy[, 2], id.lengths = idLength,
        gp = gp, default.units = "native",
        arrow = arrow(length = unit(length, "inches"))
      )
    ),
    gp = gp,
    cl = "plotLine")
  }

  grid.draw(lineGrob, recording = FALSE)
  return(invisible(lineGrob))
}

#' Make an optimal layout of plots
#'
#' Internal function. Using the size of the current device, and number and
#' dimension ratios of the plots, place them optimally in the plotting region.
#'
#' @param arr an object of class `.arrangement`.
#'
#' @param visualSqueeze Numeric. The proportion of the white space to be used
#'                      for plots. Default is 0.75.
#'
#' @param legend Logical indicating whether legend should be included as part of
#'               layout calculation. Default is `TRUE`.
#'
#' @param axes Logical indicating whether the axes should be included as part of
#'             layout calculation. Default is `TRUE`.
#'
#' @param title Logical indicating whether the names of each plot should be
#'              written above plots and should be included as part of layout
#'               calculation. Default is `TRUE`.
#'
#' @author Eliot McIntire
#' @importFrom grid unit unit.c
#' @include plotting-classes.R
#' @keywords internal
#' @rdname makeLayout
#'
.makeLayout <- function(arr, visualSqueeze,
                        legend = TRUE, axes = TRUE, title = TRUE) {
  columns <- arr@columns
  rows <- arr@rows

  # Reduce by 40% of remaining space if each of the following is not wanted
  if (legend == FALSE) {
    visualSqueeze <- visualSqueeze + 0.4 * (1 - visualSqueeze)
  }
  if (axes == FALSE) {
    visualSqueeze <- visualSqueeze + 0.4 * (1 - visualSqueeze)
  }
  if (title == FALSE) {
    visualSqueeze <- visualSqueeze + 0.4 * (1 - visualSqueeze)
  }

  # calculate the visualSqueeze for the width (i.e., vS.w)
  vS.w <- min( # nolint
    visualSqueeze / columns,
    visualSqueeze / columns * arr@actual.ratio / arr@ds.dimensionRatio
  )

  wdth <- unit.c(unit(0.2, "null"),
                 unit(rep(c(0.875, vS.w, 0.875), columns),
                      rep(c("null", "npc", "null"), columns)),
                 unit(0.2, "null"))

  # calculate the visualSqueeze for the height (i.e., vS.h)
  vS.h <- min(visualSqueeze / rows, # nolint
              visualSqueeze / rows * arr@ds.dimensionRatio / arr@actual.ratio)
  ht <- unit.c(unit(0.2, "null"),
               unit(rep(c(0.875, vS.h, 0.875), rows),
                    rep(c("null", "npc", "null"), rows)),
               unit(0.2, "null"))

  return(list(wdth = wdth, ht = ht, wdthUnits = vS.w, htUnits = vS.h,
              visualSqueeze = visualSqueeze))
}

#' Make viewports
#'
#' Given a set of extents, and a layout for these extents, this function will
#' output a viewport tree to allow plotting.
#'
#' This function will either create a totally new set of viewports, or simply
#' add some nested viewports to an existing arrangement, i.e., is there still
#' white space available to plot.
#'
#' @param sPlot An object of class `.quickPlot`.
#'
#' @param newArr  Logical indicating whether this function will create a
#'                completely new viewport. Default `FALSE`.
#'
#' @author Eliot McIntire
#' @include plotting-classes.R
#' @importFrom grid viewport vpTree vpList
#' @keywords internal
#' @rdname makeViewports
#'
.makeViewports <- function(sPlot, newArr = FALSE) {
  arr <- sPlot@arr
  sgl <- sPlot@quickPlotGrobList

  extents <- sapply(sgl, function(x) {
    unname(lapply(x[[1]]@isSpatialObjects, function(z) {
      hasZoomExtent <- FALSE
      obj <- if (z %in% TRUE) {
        ze <- x[[1]]@plotArgs$zoomExtent
        hasZoomExtent <- (!is.null(ze))
        if (hasZoomExtent)
          out <- extent(ze) # convert to list
      }
      if (hasZoomExtent %in% FALSE) {
        obj <- eval(parse(text = x[[1]]@objName), envir = x[[1]]@envir)
        if (z == TRUE) {
          # for spatial objects without zoomExtent
          out <- extent(obj)
        } else {
          if (is(obj, "SpatRaster")) {
            obj <- terra::rast(obj)
          }

          # if the object has an extent method
          if (hasMethod("extent", is(obj)[1]) & !is.list(obj)) {
            # list has an extent method, but too general
            out <- extent(obj)
          } else {
            # for non spatial objects
            out <- list(xmin = 0, xmax = 2, ymin = 0, ymax = 2)
          }
        }
      }
      out
    }))
  })

  columns <- arr@columns
  rows <- arr@rows
  gl1 <- grid.layout(
    nrow = rows * 3 + 2, ncol = columns * 3 + 2,
    widths = arr@layout$wdth, heights = arr@layout$ht
  )
  topVp <- viewport(layout = gl1, name = "top"
  )
  plotVps <- list()

  nam <- names(extents)

  # This is the biggest of the extents, and is used in .makeLayout
  #  Need to replicate it here because all plots are scaled to this
  biggestDims <- apply(do.call(rbind, sapply(1:length(sgl), function(x) {
    lapply(sgl[[x]][[1]]@isSpatialObjects, function(z) {
      .hasBbox(z, sgl[[x]][[1]]@objClass, sgl[[x]][[1]]@objName,
               sgl[[x]][[1]]@envir)
    })
  })), 2, max)

  for (extentInd in 1:length(extents)) {
    posInd <- match(nam[extentInd], names(sgl))
    lpc <- ceiling((posInd - 1) %% columns + 1) * 3 # nolint
    lpr <- ceiling(posInd / columns) * 3

    if (!sgl[[posInd]][[1]]@isSpatialObjects) {
      lpc <- c((lpc - 1):(lpc + 1)) # nolint
      lpr <- c((lpr):(lpr + 1)) # nolint
    }

    # Convert to list --> too many other formats
    extents[[extentInd]] <- .ExtentToList(extents[[extentInd]])

    # makes equal scale
    yrange <- extents[[extentInd]]$ymax - extents[[extentInd]]$ymin
    if (yrange > 0) {
      if (abs((yrange / (extents[[extentInd]]$xmax - extents[[extentInd]]$xmin)) - # nolint
        (biggestDims[1] / biggestDims[2])) > getOption("quickPlot.tolerance")) {
        dimensionRatio <- arr@layout$wdthUnits * arr@ds[1] /
          (arr@layout$htUnits * arr@ds[2])
        plotScaleRatio <- (extents[[extentInd]]$xmin - extents[[extentInd]]$xmax) /
          (extents[[extentInd]]$ymin - extents[[extentInd]]$ymax)

        vS.w <- min(1, plotScaleRatio / dimensionRatio) # nolint
        vS.h <- min(1, dimensionRatio / plotScaleRatio) # nolint

        addX <- abs((extents[[extentInd]]$xmax - extents[[extentInd]]$xmin) * 0.025) # nolint
        addY <- abs((extents[[extentInd]]$ymax - extents[[extentInd]]$ymin) * 0.025) # nolint
        # addY <- abs(extents[[extentInd]]$ymax - extents[[extentInd]]$ymin -
        #               (extents[[extentInd]]$ymax - extents[[extentInd]]$ymin) /
        #               vS.h) / 2
        # addX <- abs(extents[[extentInd]]$xmax - extents[[extentInd]]$xmin -
        #               (extents[[extentInd]]$xmax - extents[[extentInd]]$xmin) /
        #               vS.w) / 2
      } else {
        addY <- addX <- 0
      }
    } else {
      addX <- abs((extents[[extentInd]]$xmax - extents[[extentInd]]$xmin) * 0.025) # nolint
      addY <- abs((extents[[extentInd]]$ymax - extents[[extentInd]]$ymin) * 0.025) # nolint
    }
    # end equal scale
    plotVps[[nam[extentInd]]] <- viewport(
      clip = "on",
      name = nam[extentInd],
      layout.pos.col = lpc,
      layout.pos.row = lpr,
      xscale = c(extents[[extentInd]]$xmin - addX, extents[[extentInd]]$xmax + addX),
      yscale = c(extents[[extentInd]]$ymin - addY, extents[[extentInd]]$ymax + addY)
    )
    plotVps[[paste0("outer", nam[extentInd])]] <- viewport(#clip = "on",
      name = paste0("outer", nam[extentInd]),
      layout.pos.col = lpc,
      layout.pos.row = lpr,
      xscale = c(extents[[extentInd]]$xmin - addX, extents[[extentInd]]$xmax + addX),
      yscale = c(extents[[extentInd]]$ymin - addY, extents[[extentInd]]$ymax + addY)
    )
  }

  wholeVp <- vpTree(topVp, do.call(vpList, plotVps))

  return(list(wholeVp = wholeVp, extents = extents))
}

#' Test whether class has `bbox` method
#'
#' For internal use only.
#'
#' @rdname hasBbox
#' @param z Logical, whether this object is a `SpatialObject`
#' @param objClass The class of the object
#' @param objName The character string name of the object
#' @param objEnv The environment where the object can be found
.hasBbox <- function(z, objClass, objName, objEnv) {
  if (z == TRUE) {
    hasBbox <- TRUE
  } else {
    if (existsMethod("bbox", objClass[1])) {
      hasBbox <- TRUE
    } else {
      hasBbox <- FALSE
    }
  }
  if (hasBbox) {
    ## for spatial objects

    # ## TODO: temporary workaround to enable Plotting terra rasters
    theObj <- try(eval(parse(text = objName), envir = objEnv), silent = TRUE)
    if (is(theObj, "try-error")) {
      theObj <- get(objName, envir = objEnv)
    }
    # if (is(theObj, "SpatRaster")) {
    #   theObj <- terra::rast(theObj)
    # }
    # ## END WORKAROUND

    xyRange(theObj)
  } else {
    # for non spatial objects
    c(1, 1)
  }
}

xyRange <- function(obj) {
  out <- NULL
  if (isGridded(obj) || is(obj, "SpatVector")) {
    # can be error with e.g., worldMatrix b/c needs to use bbox
    out <- try(c(terra::xmax(obj) - terra::xmin(obj), terra::ymax(obj) - terra::ymin(obj)),
               silent = TRUE)
    if (is(out, "try-error")) {
      out <- try(extent(obj))
      if (!is(out, "try-error")) {
        if (is(out, "list")) {
          out <- c(out$xmax - out$xmin, out$ymax - out$ymin)
        } else {
          out <- c(terra::xmax(out) - terra::xmin(out), terra::ymax(out) - terra::ymin(out))
        }
      }

    }
  }
  else if (inherits(obj, "sf")) {
    bb <- sf::st_bbox(obj)
    out <- c(bb["xmax"] - bb["xmin"], bb["ymax"] - bb["ymin"])
  }

  if (is.null(out) || is(out, "try-error")) {
    for (i in 1:2) {
      out <- try(apply(bbox(obj), 1, function(y) {
        diff(range(y))
      }), silent = TRUE)
      if (!is(out, "try-error"))
        break
      if (is(obj, "Spatial")) {
        if (!requireNamespace("sp"))
          stop("Please install.packages('sp') to use ", class(obj))
        bbox <- sp::bbox
      } else {
        bbox <- suppressWarnings(findMethods("bbox", classes = class(obj)))
        if (length(bbox) == 0)
          stop("Could not find bbox for ", paste(class(obj), collapse = ", "),
               "\nPerhaps a package not installed or loaded (e.g., library(...) )?")
      }
    }

  }
  out
}

#' Convert pairs of coordinates to `SpatialLines`
#'
#' This will convert 2 objects whose coordinates can be extracted with `coordinates`
#' (e.g., `sp::SpatialPoints*`) to a single `SpatialLines` object.
#' The first object is treated as the "to" (destination), and the second object the "from" (source).
#' This can be used to represent directional `SpatialLines`, especially with with arrow heads,
#' as in `Plot(sl, length = 0.1)`.
#'
#' @param sp1 a `SpatialPoints*` object
#' @param from a `SpatialPoints*` object. Optional. If not provided, then the function will
#'             attempt to find the "previous" coordinates as columns (`prevX`, `prevY`)
#'             in the `sp1` object.
#'
#' @export
#' @examples
#' caribou <- terra::vect(x = cbind(x = stats::runif(1e1, -50, 50),
#'                                         y = stats::runif(1e1, -50, 50)))
#' caribouFrom <- terra::vect(x = cbind(x = stats::runif(1e1, -50, 50),
#'                                         y = stats::runif(1e1, -50, 50)))
#' caribouLines <- sp2sl(caribou, caribouFrom)
#' if (interactive()) {
#'   clearPlot()
#'   Plot(caribouLines, length = 0.1)
#' }
sp2sl <- function(sp1, from) {
  l <- vector("list", NROW(sp1))
  beginCoord <- coordinates(sp1)
  if (missing(from)) {
    endCoord <- sp1[, c("prevX", "prevY")]
  } else {
    endCoord <- coordinates(from)
  }

  if (is(sp1, "Spatial")) {
    for (i in seq_along(l)) {
      l[[i]] <- sp::Lines(list(sp::Line(rbind(beginCoord[i, ], endCoord[i, ]))), as.character(i))
    }
    sp::SpatialLines(l)
  } else {
    mat <- cbind(object = rep(seq(NROW(sp1)), 2),
                 part = 1,
                 rbind(beginCoord, endCoord))
    mat <- mat[order(mat[, "object"]), ]
    out <- terra::vect(mat, "lines")
  }
}

#' Thin a polygon using `fastshp::thin`
#'
#' For visualizing, it is sometimes useful to remove points in `Spatial*` objects.
#' This will change the geometry, so it is not recommended for computation.
#' This is similar to `sf::st_simplify`,
#' but faster (see examples) for large shapefiles, particularly if
#' `returnDataFrame` is `TRUE`.
#' *`thin` will not attempt to preserve topology.*
#' It is strictly for making smaller polygons for the (likely) purpose of visualizing more quickly.
#'
#' @param x A `Spatial*` object
#' @param tolerance Maximum allowable distance for a point to be removed.
#' @param returnDataFrame If `TRUE`, this will return a list of 3 elements,
#'        `xyOrd`, `hole`, and `idLength`.
#'        If `FALSE` (default), it will return a `SpatialPolygons` object.
#' @param minCoordsToThin If the number of coordinates is smaller than this number,
#'        then thin will just pass through, though it will take the time required to
#'        calculate how many points there are (which is not `NROW(coordinates(x))` for
#'        a `SpatialPolygon`)
#' @param ... Passed to methods (e.g., `maxNumPolygons`)
#' @param maxNumPolygons For speed, `thin` can also simply remove some of the
#'        polygons. This is likely only a reasonable thing to do if there are
#'        a lot of polygons being plotted in a small space. Current default is
#'        taken from `options('quickPlot.maxNumPolygons')`, with a message.
#'
#' @inheritParams Plot
#' @importFrom data.table as.data.table data.table set
#' @rdname thin
thin <- function(x, tolerance, returnDataFrame, minCoordsToThin, ...,
                 verbose = getOption("quickPlot.verbose")) {
  UseMethod("thin")
}

#' @rdname thin
thnSpatialPolygons <- function(x, tolerance = NULL, returnDataFrame = FALSE, minCoordsToThin = 1e5,
                               maxNumPolygons = getOption("quickPlot.maxNumPolygons", 3e3), ...,
                               verbose = getOption("quickPlot.verbose")) {
  # For speed of plotting
  xyOrd <- ffortify(x, matchFortify = FALSE,
                    simple = returnDataFrame, maxNumPolygons) # a list: out, hole, idLength
  if (is.null(tolerance)) {
    tolerance <- (terra::xmax(x) - terra::xmin(x)) * 0.0001
    messageVerbose("tolerance set to ", tolerance, verbose = verbose)
  }
  if (requireNamespace("fastshp", quietly = TRUE)) {
    if (NROW(xyOrd[["out"]]) > minCoordsToThin) {
      thinRes <- fastshp::thin(xyOrd[["out"]]$x, xyOrd[["out"]]$y, # can't use x or y because sometimes (sf) it is capitalized
                             tolerance = tolerance, id = xyOrd[["out"]]$groups)
      if (any(thinRes))
        messageVerbose("Some polygons have been simplified", verbose = verbose)

      set(xyOrd[["out"]], NULL, "thinRes", thinRes)
      xyOrd[["out"]][, keepAll := sum(thinRes) < 4, by = groups]

      xyOrd[["out"]] <- xyOrd[["out"]][thinRes | keepAll]

      #xyOrd[["out"]] <- xyOrd[["out"]][thinRes, ]# thin line
      if (returnDataFrame) {
        xyOrd[["idLength"]] <- xyOrd[["out"]][, list(V1 = .N), by = groups]
      } else {
        # clean up a bit
        set(xyOrd[["out"]], NULL, "order", NULL)
        set(xyOrd[["out"]], NULL, "groups", NULL)

        polyList <- split(xyOrd[["out"]],
                          by = grep("poly|Polygon|Polygons", value = TRUE, colnames(xyOrd[["out"]])),
                          flatten = FALSE, keep.by = FALSE)
        bb <- lapply(unique(xyOrd$out$Polygons), function(outerI) {
          poly <- lapply(seq(polyList[[outerI]]), function(innerI) {
            #Polygon(as.matrix(polyList[[outerI]][[innerI]][, c("x", "y")]),
            sp::Polygon(cbind(polyList[[outerI]][[innerI]]$x, polyList[[outerI]][[innerI]]$y),
                    hole = unique(as.logical(polyList[[outerI]][[innerI]]$hole)))
          })
          sp::Polygons(poly, ID = outerI)
        })

        names1 <- unique(xyOrd$out$Polygons)
        xyOrd <- sp::SpatialPolygons(bb, proj4string = sp::CRS(sp::proj4string(x)))
        if (is(x, "SpatialPolygonsDataFrame")) {
          if (length(x) > maxNumPolygons) {
            dat <- x@data[as.numeric(names1) + 1,]
          } else {
            dat <- x@data
          }
          xyOrd <- sp::SpatialPolygonsDataFrame(xyOrd, data = dat)
        }

        return(xyOrd)
      }
    }
  } else {
    messageVerbose(messFastshape("polygon"), verbose = verbose)
    if (Sys.info()[["sysname"]] == "Windows") {
      messageVerbose(verbose = verbose,
        paste(
          "You may also need to download and install Rtools from:\n",
          " https://cran.r-project.org/bin/windows/Rtools/"
        )
      )
    }
  }
  xyOrd <- list(xyOrd = xyOrd[["out"]], hole = xyOrd[["hole"]],
                idLength = xyOrd[["idLength"]])
  return(xyOrd)
}

#' @export
#' @rdname thin
thin.default <- function(x, tolerance, returnDataFrame, minCoordsToThin, maxNumPolygons, ...,
                         verbose = getOption("quickPlot.verbose")) {
  if ( isSpatialPolygons(x) || (isSpatVector(x) && identical("polygons", terra::geomtype(x))) ||
      isSF(x)) {
    x <- thnSpatialPolygons(x, tolerance = tolerance, returnDataFrame = returnDataFrame,
                            minCoordsToThin = minCoordsToThin,
                            maxNumPolygons = getOption("quickPlot.maxNumPolygons", 3e3), ...,
                            verbose = verbose)

  } else {
    messageVerbose("No method for that class of object exists. See methods('thin') to see current methods")
  }
}

#' Fortify
#'
#' Convert an arbitrary object to a `data.frame`-like object.
#'
#' @note This only deals with `SpatialPolygons` objects.
#'
#' @rdname fortify
#' @name fortify
#' @importFrom data.table setDT set
#' @keywords internal
ffortify <- function(x, matchFortify = TRUE, simple = FALSE,
                     maxNumPolygons = getOption("quickPlot.maxNumPolygons", 3e3),
                     verbose = getOption("quickPlot.verbose")) {
  if (isSpatVector(x) || isSF(x)) {
    if (isSpatVector(x)) {
      gg <- terra::geom(x)
      dt <- as.data.table(gg)
      colNameForGeom <- "geom"
      rmCols <- c("geom", "part", "hole")
      aa <- unique(dt[, c("geom", "hole")])
      set(aa, NULL, "group", seq(NROW(aa)))
      groups <- dt[aa, on = c("geom", "hole")]$group
      hole <- dt[, any(hole > 0), by = groups]$V1
    } else {
      cc <- sf::st_coordinates(x)
      dt <- as.data.table(cc)
      colNameForGeom <- "L2"
      rmCols <- c("L1", "L2")
      data.table::setnames(dt, old = c("X", "Y"), new = c("x", "y"))
      aa <- unique(dt[, ..rmCols])
      set(aa, NULL, "group", seq(NROW(aa)))
      groups <- dt[aa, on = rmCols]$group
      # groups <- as.integer(factor(paste0(dt[["L1"]], "_", dt[["L2"]])))
      hole <- dt[, any(L1 > 1), by = groups]$V1

    }
    idLength <- dt[, list(V1 = .N), by = groups][, "V1"]
    data.table::set(dt, NULL, "groups", groups)
    # data.table::setnames(dt, colNameForGeom, new = "groups")
    data.table::set(dt, NULL, "poly", dt[[colNameForGeom]])
    if (exists("rmCols", inherits = FALSE))
      set(dt, NULL, rmCols, NULL)
    out <- list(out = dt, hole = hole, idLength = idLength)
  } else if (isSF(x)) {
    # st_sfc(st_polygon(lapply(split(a, by = "L1"), function(y) as.matrix(y)[, 1:2]), dim = "XY"))
    idLength <- dt[, list(V1 = .N), by = "L2"][, "V1"]
    data.table::setnames(dt, "L2", new = "groups")
    data.table::set(dt, NULL, "poly", dt$groups)
    out <- list(out = dt, hole = hole, idLength = idLength)
  } else {

    ord <- if (isSpatial(x)) x@plotOrder else seq_along(x) # a seq vector of length(x)
    if (length(ord) > maxNumPolygons) {

      polygonSeq <- .polygonSeq(x, maxNumPolygons) #if (is.numeric(x@data$Shape_Area)) {
      ord <- ord[polygonSeq]
      .showingOnlyMessage(numShowing = maxNumPolygons,
                          totalAvailable = length(x@plotOrder), verbose = verbose)
    }
    ordSeq <- seq(ord)

    xy <- lapply(ordSeq, function(i) {
      lapply(x@polygons[[ord[i]]]@Polygons, function(j) {
        j@coords
      })
    })

    hole <- tryCatch(unlist(lapply(ordSeq, function(xx) {
      lapply(x@polygons[[ord[xx]]]@Polygons, function(yy)
        yy@hole)
    })), error = function(xx) FALSE)

    IDs <- tryCatch(unlist(lapply(ordSeq, function(xx) {
      x@polygons[[ord[xx]]]@ID
    })), error = function(xx) FALSE)

    ordInner <- lapply(ordSeq, function(xx) {
      x@polygons[[ord[xx]]]@plotOrder
    })

    xyOrd.l <- lapply(ordSeq, function(i) { # nolint
      xy[[ordSeq[i]]][ordInner[[ordSeq[i]]]]
    })

    idLength <- data.table(V1 = unlist(lapply(xyOrd.l, function(i) {
      lapply(i, length)
    })) / 2)

    polyLength <- data.table(V1 = unlist(lapply(xyOrd.l, function(i) {
      NROW(i)
    })))

    numPolygons <- unlist(length(xyOrd.l))
    numPolygon <- unlist(lapply(xyOrd.l, length))

    xyOrd <- do.call(rbind, lapply(xyOrd.l, function(i) {
      do.call(rbind, i)
    }))

    groups <- rep(1:NROW(idLength), idLength$V1)
    poly <- rep(1:NROW(polyLength), polyLength$V1)
    poly <- rep(poly, idLength$V1)

    if (!simple | matchFortify) {
      # Polygons <- rep(rep(seq(numPolygons), numPolygon), idLength$V1) # sequential numbering
      Polygons <- rep(rep(IDs, numPolygon), idLength$V1) # actual ID labelling
      Polygon <- rep(unlist(lapply(numPolygon, seq)), idLength$V1)
      holes <- rep(hole, idLength$V1)
      orders <- unlist(lapply(idLength$V1, seq))
    }

    if (matchFortify) {
      if (!simple) messageVerbose("for matchFortify = TRUE, simple is set to FALSE",
                                  verbose = verbose)
      return(data.frame(lat = xyOrd[,1], long = xyOrd[,2], order = orders,
                        hole = holes, id = Polygons, piece = Polygon,
                        #group = paste0(as.character(Polygons), ".", as.character(Polygon)))) # the actual fortify
                        group = groups))
    } else {
      out <- setDT(data.frame(x = xyOrd[,1], y = xyOrd[,2], groups = groups, poly = poly))
      if (!simple) {
        set(out, NULL, "order", orders)
        set(out, NULL, "hole", holes)
        set(out, NULL, "Polygons", Polygons)
        set(out, NULL, "Polygon", Polygon)
      }
      out <- list(out = out, hole = hole, idLength = idLength)

    }
  }
  return(out)
}

.polygonSeq <- function(polygon, maxNumPolygons) {
  if (is.numeric(polygon@data$Shape_Area)) {
    which(polygon@data$Shape_Area > (sort(polygon@data$Shape_Area, decreasing = TRUE)[maxNumPolygons]))
  } else {
    round(seq(1, length(polygon), length.out = maxNumPolygons))
  }
}

.createPolygonGrob <- function(gp, xyOrd) {
  # gp$fill[xyOrd[["hole"]]] <- "#FFFFFF00"
  gp$fill[xyOrd[["hole"]]] <- "white"
  polyGrob <- gTree(children = gList(
    polygonGrob(
      x = xyOrd[["xyOrd"]]$x, y = xyOrd[["xyOrd"]]$y,
      id.lengths = xyOrd[["idLength"]]$V1,
      gp = gp, default.units = "native"
    )
  ),
  gp = gp,
  cl = "plotPoly")
}

.showingOnlyMessage <- function(numShowing, totalAvailable, verbose = getOption("quickPlot.verbose")) {
  messageVerbose("Showing only ", numShowing, " of ",
          totalAvailable," polygons in this view. See options('quickPlot.maxNumPolygons')",
          verbose = verbose)
}



#' Get extent of a variety of spatial objects
#'
#' This is a wrapper around `terra::ext`, `sf::st_bbox`, and
#' `raster::extent`.
#' @rdname extent
#' @name extent
#' @param x The spatial object from which to extract the extent.
#' @param ... Not used.
#' @return Returns a list of length 4 with elements `xmin`, `xmax`, `ymin`, and `ymax`,
#'   in that order.
#'
#' @export
if (!isGeneric("extent", .GlobalEnv)) {
  setGeneric(
    "extent",
    function(x, ...) {
      standardGeneric("extent")
    }
  )
}


#' @rdname extent
#' @export
setMethod(
  "extent",
  signature("ANY"),
  definition = function(x, ...) {
  .ExtentToList(x)
})

.ExtentToList <- function(x) {
  if (!is(x, "list")) {
    x <- if (inherits(x, "sf")) {
      x <- as.list(sf::st_bbox(x))
    } else { #if (isGridded(x) || inherits(x, "Spatial") || isSpat(x) ||
      # is(x, "SpatExtent") || is(x, "Extent")) {
      if (!is(x, "SpatExtent")) {
        if (isSpat(x) || isSpatial(x))
          x <- terra::ext(x)
        else {
          if (!requireNamespace("raster", quietly = TRUE))
            stop("Need to install.packages('raster')")
          x <- raster::extent(x)
        }

      }
      list(xmin = terra::xmin(x), xmax = terra::xmax(x),
           ymin = terra::ymin(x), ymax = terra::ymax(x))
    }
  }
  x
}


#' Extract coordinates from a variety of spatial objects
#'
#' This will extract using `terra::crds`, `sf::st_coordinates` and
#' `raster::coordinates`. Other packages can create methods, as this is
#' generic.
#'
#' @param obj An object from which to extract the coordinates (e.g., `sf`, `sp`)
#' @param ... Ignored.
#' @return A 2 column matrix of coordinates (x and y)
#'
#' @export
#' @rdname coordinates
#' @name coordinates
#' @examples
#' library(terra)
#' caribou <- terra::vect(x = cbind(x = stats::runif(1e1, -50, 50),
#'                                         y = stats::runif(1e1, -50, 50)))
#' coordinates(caribou)
#'
if (!isGeneric("coordinates", .GlobalEnv)) {
  setGeneric(
    "coordinates",
    function(obj, ...) {
      standardGeneric("coordinates")
    }
  )
}

#' @rdname coordinates
#' @export
setMethod(
  "coordinates",
  signature("ANY"),

  definition = function(obj, ...) {
  if (isSpat(obj) && isVector(obj)) {
    terra::crds(obj)
  } else if (isSF(obj)) {
    sf::st_coordinates(obj)
  } else {
    raster::coordinates(obj)
  }

})

isLonLat <- function(x) {
  if (isSpat(x)) {
    isTRUE(terra::is.lonlat(x))
  } else {
    if (is(x, "Spatial")) {
      terra::is.lonlat(sp::proj4string(x))
    } else {
      terra::is.lonlat(terra::crs(x)  )
    }
  }
}

speedupScale <- function(grobToPlot, lonlatSU, SU) {
  extGTP <- extent(grobToPlot)
  isLongLat <- tryCatch(isLonLat(grobToPlot), error = function(e) FALSE) # for object classes with no isLonLat method
  speedupScale <- if (isTRUE(isLongLat)) {
    as.numeric(terra::distance(
      x = cbind(extGTP$xmax, extGTP$ymax),
      y = cbind(extGTP$xmin, extGTP$ymin),
      lonlat = FALSE
    )) / (lonlatSU)
  } else {
    max(extGTP$ymax - extGTP$ymin,
        extGTP$xmax - extGTP$xmin) /
      SU
  }
  speedupScale
}

gArea <- function(x) {
  sf::st_area(sf::st_as_sf(terra::as.polygons(terra::ext(x))))
}


minFn <- function(x) {
  minmaxFn(x, "min")
}

maxFn <- function(x) {
  minmaxFn(x, "max")
}

#' @importFrom utils head tail getFromNamespace
minmaxFn <- function(x, which = "max") {
  out <- NULL
  if (isRaster(x)) {
    if (!requireNamespace("raster")) stop()
    fn <- get(paste0(which, "Value"), envir = asNamespace("raster"))
    out <- fn(x)

  } else {
    if (!(requireNamespace("terra"))) stop()
    fn <- ifelse(identical(which, "max"), "tail", "head")
    fn <- utils::getFromNamespace(fn, ns = "utils")
    out <- fn(terra::minmax(x), 1)[1, ]

  }
  if (is.null(out))
    stop("To use maxFn or minFn, you need either terra or raster package installed")

  out
}

isFALSE <- function (x)
  is.logical(x) && length(x) == 1L && !is.na(x) && !x

messFastshape <- function(shape) {
  paste0(
    "To speed up ", shape, " plotting using Plot install the fastshp package:\n",
    "install.packages(\"fastshp\", repos=\"https://rforge.net\", type=\"source\")\n",
    "or for binary:\n",
    "install.packages('fastshp', repos = 'https://PredictiveEcology.r-universe.dev')"
  )

}

messageVerbose <- function(...,
                           verbose = getOption("quickPlot.verbose", 1L),
                           verboseLevel = 1) {
  if (verbose >= verboseLevel) {
    message(...)
  }
}

adjustGridFIG <- function(gf, nCols, nRows, wh) {
  whR <- ceiling(wh / nCols)
  whC <- (wh - 1) %% nCols + 1
  if (nRows > 1) {
    interRange <- (gf[4] - gf[3])
    each <- interRange / nRows
    gf[3] <- gf[4] - each * (whR)
    gf[4] <- gf[4] - each * (whR - 1)
  }
  if (nCols > 1) {
    interRange <- (gf[2] - gf[1])
    each <- interRange / nCols
    gf[2] <- gf[1] + each * (whC)
    gf[1] <- gf[1] + each * (whC - 1)
  }
  gf
}

