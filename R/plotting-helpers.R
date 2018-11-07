### deal with spurious data.table warnings
if (getRversion() >= "3.1.0") {
  utils::globalVariables(c(".", ".N", "keepAll"))
}

#' Find the number of layers in a Spatial Object
#'
#' There are already methods for \code{Raster*} in the raster package.
#' Adding methods for \code{list}, \code{SpatialPolygons}, \code{SpatialLines},
#' and \code{SpatialPoints}, \code{gg}, \code{histogram}, \code{igraph}.
#' These latter classes return \code{1}.
#'
#' @param x A \code{.quickPlotObjects} object or list of these.
#'
#' @return The number of layers in the object.
#'
#' @author Eliot McIntire
#' @export
#' @include plotting-classes.R
#' @rdname numLayers
#'
#' @examples
#' library(igraph)
#' library(raster)
#'
#' files <- system.file("maps", package = "quickPlot") %>%
#'   dir(., full.names = TRUE, pattern = "tif")
#' maps <- lapply(files, function(x) raster(x))
#' names(maps) <- sapply(basename(files), function(x) {
#'   strsplit(x, split = "\\.")[[1]][1]
#' })
#' stck <- stack(maps)
#'
#' numLayers(maps)
#' numLayers(stck)
#'
setGeneric("numLayers", function(x) standardGeneric("numLayers"))

#' @export
#' @rdname numLayers
setMethod(
  "numLayers",
  signature = "list",
  definition = function(x) {
    sum(unlist(lapply(x, function(x) {
      if (inherits(x, "RasterStack")) {
        numLayers(x)
      } else {
        1L
      }
    })))
})

#' @rdname numLayers
setMethod(
  "numLayers",
  signature = ".quickPlot",
  definition = function(x) {
    return(length(x@arr@extents))
})

#' @export
#' @importFrom raster nlayers
#' @rdname numLayers
setMethod(
  "numLayers",
  signature = "Raster",
  definition = function(x) {
    return(nlayers(x))
})

#' @export
#' @rdname numLayers
setMethod(
  "numLayers",
  signature = "Spatial",
  definition = function(x) {
    return(1L)
})

#' @export
#' @rdname numLayers
setMethod(
  "numLayers",
  signature = "ANY",
  definition = function(x) {
    return(1L)
})

#' Extract the layer names of Spatial Objects
#'
#' There are already methods for \code{Raster*} objects. This adds methods for
#' \code{SpatialPoints*}, \code{SpatialLines*}, and \code{SpatialPolygons*},
#' returning an empty character vector of length 1.
#' This function was created to give consistent, meaningful results for all
#' classes of objects plotted by \code{Plot}.
#'
#' @param object  A \code{Raster*}, \code{SpatialPoints*}, \code{SpatialLines*},
#'                or \code{SpatialPolygons*} object; or list of these.
#'
#' @author Eliot McIntire
#' @export
#' @include plotting-classes.R
#' @rdname layerNames
#'
#' @examples
#' library(igraph)
#' library(raster)
#'
#' ## RasterLayer objects
#' files <- system.file("maps", package = "quickPlot") %>%
#'   dir(., full.names = TRUE, pattern = "tif")
#' maps <- lapply(files, function(x) raster(x))
#' names(maps) <- sapply(basename(files), function(x) {
#'   strsplit(x, split = "\\.")[[1]][1]
#' })
#' layerNames(maps)
#'
#' ## Spatial* objects
#' caribou <- SpatialPoints(coords = cbind(x = stats::runif(1e2, -50, 50),
#'                                         y = stats::runif(1e2, -50, 50)))
#' layerNames(caribou)
#'
#' sr1 <- Polygon(cbind(c(2, 4, 4, 1, 2), c(2, 3, 5, 4, 2)) * 20 - 50)
#' sr2 <- Polygon(cbind(c(5, 4, 2, 5), c(2, 3, 2, 2)) * 20 - 50)
#' srs1 <- Polygons(list(sr1), "s1")
#' srs2 <- Polygons(list(sr2), "s2")
#' spP <- SpatialPolygons(list(srs1, srs2), 1:2)
#' layerNames(spP)
#'
#' l1 <- cbind(c(10, 2, 30), c(30, 2, 2))
#' l1a <- cbind(l1[, 1] + .05, l1[, 2] + .05)
#' l2 <- cbind(c(1, 20, 3), c(10, 1.5, 1))
#' sl1 <- Line(l1)
#' sl1a <- Line(l1a)
#' sl2 <- Line(l2)
#' s1 <- Lines(list(sl1, sl1a), ID = "a")
#' s2 <- Lines(list(sl2), ID = "b")
#' sl <- SpatialLines(list(s1, s2))
#' layerNames(sl)
#'
setGeneric("layerNames", function(object) {
  standardGeneric("layerNames")
})

#' @export
#' @rdname layerNames
setMethod(
  "layerNames",
  signature = "list",
  definition = function(object) {
    unlist(lapply(object, layerNames))
})

#' @export
#' @rdname layerNames
setMethod(
  "layerNames",
  signature = "ANY",
  definition = function(object) {
    return("")
})

#' @export
#' @rdname layerNames
setMethod(
  "layerNames",
  signature = "Raster",
  definition = function(object) {
    names(object)
})

#' @rdname layerNames
setMethod(
  "layerNames",
  signature = ".quickPlot",
  definition = function(object) {
    return(unlist(lapply(object@quickPlotGrobList, function(x) {
      unlist(lapply(x, function(y) y@plotName))[[1]]
    })))
})

#' @export
#' @rdname layerNames
setMethod(
  "layerNames",
  signature = "igraph",
  definition = function(object) {
    return("")
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
#' library(igraph)
#' library(raster)
#'
#' files <- system.file("maps", package = "quickPlot") %>%
#'   dir(., full.names = TRUE, pattern = "tif")
#' maps <- lapply(files, function(x) raster(x))
#' names(maps) <- sapply(basename(files), function(x) {
#'   strsplit(x, split = "\\.")[[1]][1]
#' })
#' extnts <- lapply(maps, extent)
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
    all(
      c(
        sapply(extents, function(x) x@xmin) == extents[[1]]@xmin,
        sapply(extents, function(x) x@xmax) == extents[[1]]@xmax,
        sapply(extents, function(x) x@ymin) == extents[[1]]@ymin,
        sapply(extents, function(x) x@ymax) == extents[[1]]@ymax
      )
    )
})

#' Make a \code{.quickPlot} class object
#'
#' Builds a \code{.quickPlot} object from a list of objects.
#'
#' @param plotObjects list. Any plot objects.
#'
#' @param plotArgs list. Any arguments that the the grid package can accept for
#' the specific grob types, e.g., rasterGrob, polygonGrob, etc.
#'
#' @param whichQuickPlottables  Logical indicating which objects in the
#' \code{Plot} call can be plotted by \code{Plot}.
#'
#' @param ... additional arguments. Currently nothing.
#'
#' @return A \code{\link{.quickPlot}} object, which has 2 slots, one for the plot arrangement
#' (i.e., layout and dimensions) and one for all of the \code{quickPlotGrobs}
#' (stored as a quickPlotGrobList of lists \code{.quickPlotGrob} objects).
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
      inherits(x, "spatialObjects")
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

    names(plotObjects) <- unlist(lapply(objs, function(x)
      x$objs))

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
        quickPlotGrobList[[lN[x]]]@objClass <- class(
          eval(parse(text = objectNamesLong[x]), lEnvs[[x]])
        )
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

#' Convert \code{plotArgs} to list of lists
#'
#' Internal function. Take the inputs as plotArgs to the Plot function, and make
#' them a list of length \code{numQuickPlotObjects} entries of lists.
#'
#' @inheritParams .makeQuickPlot
#'
#' @param numQuickPlotObjects Numeric. The number of \code{.quickPlotObjects}.
#'                 This can't easily be deduced from the \code{plotArgs} because
#'                 of the \code{RasterStack}s. So passed manually.
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

#' Make \code{SpatialLines} object from two \code{SpatialPoints} objects
#'
#' The primary conceived usage of this is to draw arrows following the
#' trajectories of agents.
#'
#' @param from  Starting spatial coordinates (\code{SpatialPointsDataFrame}).
#'
#' @param to    Ending spatial coordinates (\code{SpatialPointsDataFrame}).
#'
#' @return A \code{SpatialLines} object. When this object is used within a
#'         \code{Plot} call and the \code{length} argument is specified, then
#'         arrow heads will be drawn. See examples.
#'
#' @author Eliot McIntire
#' @export
#' @include plotting-classes.R
#' @importFrom raster crs
#' @importFrom sp coordinates Line Lines SpatialLines
#' @rdname makeLines
#'
#' @example inst/examples/example_makeLines.R
#'
setGeneric("makeLines", function(from, to) {
  standardGeneric("makeLines")
})

#' @export
#' @rdname makeLines
setMethod(
  "makeLines",
  signature = c("SpatialPoints", "SpatialPoints"),
  definition = function(from, to) {
    SpatialLines(lapply(seq_len(length(from)), function(x) {
      Lines(list(Line(
        coords = rbind(coordinates(from)[x, ], coordinates(to)[x, ])
      )), ID = x)
    }), proj4string = crs(from)) # nolint
})

#' Parse arguments and find environments
#'
#' Internal function used within objectNames.
#'
#' @param y  A character representation of the arguments passed to a function,
#'           e.g., \code{Plot}.
#'
#' @param e  Environment in which the function (e.g., \code{Plot}) was called.
#'
#' @param eminus1  The parent environment of \code{e}.
#'
#' @return A list of length 2, with names \code{objs} and \code{envs} giving the
#' standardized representation (i.e., replacing \code{[[]]} with \code{$}
#' notation for objects) of objects and their layers (if \code{RasterStacks}).
#'
#' @importFrom grDevices dev.cur
#' @include plotting-classes.R
#' @keywords internal
#' @rdname parseArgs
#' @author Eliot McIntire and Alex Chubaty
#'
# igraph exports %>% from magrittr
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
  envs <- append(.GlobalEnv, sframes) %>%
    .[c(TRUE, unlist(lapply(sframes, function(x) {
      exists(deparsedTxt, envir = x, inherits = FALSE)
    })))] %>%
    .[[length(.)]]

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
#' Internal function primarily used from \code{Plot}.
#'
#' @param calledFrom  character vector of length 1, indicating which function
#'                    call is desired. Defaults to \code{Plot}.
#'
#' @param argClass    character vector of length 1, indicating which class is
#'                    being searched for among the arguments.
#'                    Defaults to \code{.quickPlotObjects}.
#'
#' @param argName     character vector of length 1, or \code{NULL}, indicating
#'                    if the arguments to select have a name, no name (empty
#'                    string), or do not use name (\code{NULL}).
#'
#' @return \code{NULL}. This function is invoked for its side effects.
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
#' Currently only the gpar function is imported. This is a convenience so that users
#' can change \code{Plot} arguments without having to load the entire grid package.
#'
#' @inheritParams grid::gpar
#'
#' @aliases gpar
#' @export
#' @importFrom grid gpar
#' @name gpar
#' @rdname grid-functions
#' @seealso \code{\link[grid]{gpar}}
#'
setGeneric("gpar", function(...) {
  standardGeneric("gpar")
})

#' @export
#' @rdname grid-functions
setMethod("gpar",
          definition = function(...) {
            return(grid::gpar(...))
})

#' Internal functions used by Plot
#'
#' Extract colors, legends and other things from object, and convert to a plotGrob
#'
#' @param grobToPlot Graphical object to plot
#' @param sGrob quickPlotGrob object
#' @param takeFromPlotObj Logical indicating whether data for grobToPlot should be found in
#'        current call to Plot or from disk
#' @param arr An arrangement object
#' @param newArr Logical, whether there is a new arrangement happening
#' @param quickPlotGrobCounter Numeric. A counter. No meaning outside Plot function.
#' @param subPlots Character. Name of plot area.
#' @param cols Color vector.
#'
#' @aliases PlotHelpers
#' @author Eliot McIntire
#' @include plotting-classes.R
#' @keywords internal
#' @name .preparePlotGrob
#' @rdname Plot-internal
#'
setGeneric(".preparePlotGrob", function(grobToPlot, sGrob, takeFromPlotObj, arr, newArr,
                                                 quickPlotGrobCounter, subPlots, cols)
  standardGeneric(".preparePlotGrob")
)

#' @aliases PlotHelpers
#' @keywords internal
#' @rdname Plot-internal
setMethod(
  ".preparePlotGrob",
  signature = c("griddedClasses", ".quickPlotGrob"),
  definition = function(grobToPlot, sGrob, takeFromPlotObj, arr, newArr,
                        quickPlotGrobCounter, subPlots, cols) {

    # Rasters may be zoomed into and subsampled and have unique legend
    #            if (sGrob@plotArgs$new)
    pR <- .prepareRaster(grobToPlot, sGrob@plotArgs$zoomExtent,
                         sGrob@plotArgs$legendRange, takeFromPlotObj,
                         arr, sGrob@plotArgs$speedup, newArr = newArr)
    zMat <- .makeColorMatrix(grobToPlot, pR$zoom, pR$maxpixels,
                             pR$legendRange,
                             na.color = sGrob@plotArgs$na.color,
                             zero.color = sGrob@plotArgs$zero.color,
                             cols = sGrob@plotArgs$cols,
                             skipSample = pR$skipSample)
    return(zMat)
})

#' @importFrom rgeos gIntersects gArea
setMethod(
  ".preparePlotGrob",
  signature = c("Spatial", ".quickPlotGrob"),
  definition = function(grobToPlot, sGrob, takeFromPlotObj, arr, newArr,
                        quickPlotGrobCounter, subPlots, cols) {

    if (!is.null(sGrob@plotArgs$zoomExtent) &&
        !identical(extent(grobToPlot), arr@extents[[subPlots]])) {
        #!identical(arr@extents[[subPlots]], sGrob@plotArgs$zoomExtent)) {
      useCrop <- FALSE
      if (!useCrop) {
        zoom <- sGrob@plotArgs$zoomExtent
        extPolygon <- as(zoom, "SpatialPolygons")
        crs(extPolygon) <- crs(grobToPlot)
        extPolygon <- list(extPolygon)
        names(extPolygon) <- sGrob@plotName

        fullArea <-
          rgeos::gArea(as(extent(grobToPlot), "SpatialPolygons"))
        zoomArea <-
          rgeos::gArea(as(extent(zoom), "SpatialPolygons"))
        numPolys <- length(grobToPlot)
        ratio <- fullArea / zoomArea
        if (numPolys / ratio * 5 > getOption("quickPlot.maxNumPolygons", 3e3)) {
          polySeq <-
            .polygonSeq(grobToPlot,
                        maxNumPolygons = getOption("quickPlot.maxNumPolygons", 3e3))
          .showingOnlyMessage(numShowing = getOption("quickPlot.maxNumPolygons", 3e3),
                              totalAvailable = length(grobToPlot))
          grobToPlot <- grobToPlot[polySeq,]

        }
        message("Cropping to new extent")
        a <- rgeos::gIntersects(grobToPlot, extPolygon[[1]], byid = TRUE)
        grobToPlot <- grobToPlot[a[1,],]
      } else {
        grobToPlot <- crop(grobToPlot, sGrob@plotArgs$zoomExtent)
      }

    }

    # This handles SpatialPointsDataFrames with column "color"
    if (any(grepl(pattern = "color", names(grobToPlot))) & is.null(cols))
      sGrob@plotArgs$cols <- unlist(getColors(grobToPlot))

    zMat <- list(z = grobToPlot, minz = 0, maxz = 0,
                 cols = sGrob@plotArgs$cols, real = FALSE)
    return(zMat)
})

setMethod(
  ".preparePlotGrob",
  signature = c("ANY", ".quickPlotGrob"),
  definition = function(grobToPlot, sGrob, takeFromPlotObj, arr, newArr,
                        quickPlotGrobCounter, subPlots, cols) {
    if (any(grepl(pattern = "color", colnames(grobToPlot))) & is.null(cols))
      sGrob@plotArgs$cols <- grobToPlot$color

    list(z = grobToPlot, minz = 0, maxz = 0,
         cols = sGrob@plotArgs$cols, real = FALSE)
})

setMethod(
  ".preparePlotGrob",
  signature = c("SpatialLines", ".quickPlotGrob"),
  definition = function(grobToPlot, sGrob, takeFromPlotObj, arr, newArr,
                        quickPlotGrobCounter, subPlots, cols) {
  if (!is.null(sGrob@plotArgs$zoomExtent)) {
    grobToPlot <- crop(grobToPlot, sGrob@plotArgs$zoomExtent)
  }
  zMat <- list(z = grobToPlot, minz = 0, maxz = 0,
               cols = sGrob@plotArgs$cols, real = FALSE)
  return(zMat)
})

#' @param whPlotFrame Numeric. Which plot within the quickPlotGrobPlots object.
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
      if (sGrob@objClass == "Raster" &
          (arr@extents[(whPlotFrame - 1) %% arr@columns + 1][[1]] ==
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
      if (sGrob@objClass == "Raster" &
          (arr@extents[whPlotFrame][[1]] ==
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

#' @param quickSubPlots List of many quickPlotGrobs
#' @param isBaseSubPlot Logical. Is the currently being plotted object a base layer
#' @param isNewPlot Logical. Is the currently being plotted object a new, additional plot
#' @param isReplot Logical. Is the currently being plotted object a replot due to something
#'                 like a rearrangement
#' @param zMat List resulting from \code{.preparePlotGrob}
#' @param wipe Logical. Is the currently being plotted object require a white rectangle to
#'             be plotted first, and subsequent other changes.
#' @param xyAxes List of length 2, resulting from \code{.xyAxes}
#' @inheritParams Plot
#' @param vps A viewport tree resulting from \code{.makeViewports}
#' @param nonPlotArgs Arguments passed to \code{Plot} that are not \code{.quickPlottables},
#'                    but are passed along with \code{.quickPlottables}.
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
                             vps, nonPlotArgs) {
  standardGeneric(".Plot")
})

#' @rdname Plot-internal
#' @importFrom raster crop is.factor
#' @aliases PlotHelpers
setMethod(
  ".Plot",
  signature = c(".quickPlotGrob"),
  definition = function(sGrob, grobToPlot, subPlots, quickSubPlots, quickPlotGrobCounter,
                        isBaseSubPlot, isNewPlot, isReplot, zMat, wipe, xyAxes, legendText,
                        vps, nonPlotArgs) {
    seekViewport(subPlots, recording = FALSE)

    if (is.list(grobToPlot)) {
      # This is for base plot calls... the grobToPlot is a call i.e,. a name
      # Because base plotting is not set up to overplot,
      # must plot a white rectangle
      par(fig = gridFIG())
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
                                         "na.color", "title", "userProvidedPlotFn"))]
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
          if (any(raster::is.factor(grobToPlot))) {
            if (all(na.omit(grobToPlot[]%%1)==0)) {
              sGrob@plotArgs$legendTxt <- raster::levels(grobToPlot)[[1]]
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
                           length = sGrob@plotArgs$length
      ) %>% append(., nonPlotArgs)

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
    if (is.null(sGrob@plotArgs$title)) sGrob@plotArgs$title <- TRUE
    if (!identical(FALSE, sGrob@plotArgs$title) | (isBaseSubPlot & (isNewPlot | isReplot))) {
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
#'                  \code{Plot} is currently being plotted, i.e., a counter of sorts.
#'
#' @include plotting-classes.R
#' @inheritParams .makeQuickPlot
#' @aliases PlotHelpers
#' @keywords internal
#' @name .refreshGrob
#' @rdname Plot-internal
#'
setGeneric(".refreshGrob", function(sGrob, subPlots, legendRange,
                                    grobToPlot, plotArgs, nColumns, nRows, whPlotObj) {
  standardGeneric(".refreshGrob")
})

#' @rdname Plot-internal
#' @aliases PlotHelpers
#' @keywords internal
setMethod(
  ".refreshGrob",
  signature = c(".quickPlotGrob"),
  definition = function(sGrob, subPlots, legendRange,
                        grobToPlot, plotArgs, nColumns, nRows, whPlotObj) {
    seekViewport(paste0("outer", subPlots), recording = FALSE)
    needsNewTitle <- sGrob@plotArgs$new != FALSE
    grid.rect(x = 0, height = unit(1 + needsNewTitle * inherits(grobToPlot, "Raster") * 0.20 / (nRows / 2), "npc"),
              width = unit(1 + inherits(grobToPlot, "Raster") * 0.20 / (nColumns / 2), "npc"),
              gp = gpar(fill = "white", col = "white"), just = "left")
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
#' Because the Plot function can use the global environment as a source of
#' objects to plot, not just the call itself, this function identifies where
#' the data for the grob should come from, the current call or the global
#' environment.
#'
#' @param toPlot The object to plot. Should be a single layer if from a multi-layer
#'               object such as a RasterStack.
#' @param sGrob quickPlot grob object
#' @param takeFromPlotObj Logical. Should the data come from the argument passed
#'                        into Plot (\code{TRUE}), or from the (\code{.quickPlotEnv})
#'                        (\code{FALSE}).
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
        if (!inherits(toPlot, "gg") & !inherits(toPlot, "igraph")) {
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
#' Internal function. Takes a raster .quickPlotGrob, and converts zoomExtent into
#' a zoom, and legendRange into a legend.
#' Then calculates the maxpixels to plot for speed.
#'
#' @param grobToPlot .quickPlotGrob
#'
#' @param zoomExtent an extent object
#'
#' @param legendRange a numeric vector of length >=2 indicating the desired legend range.
#'
#' @param takeFromPlotObj logical. Should the object be found in the Plot call or .GlobalEnv
#'
#' @param arr an \code{.arrangement} object
#'
#' @param speedup numeric, greater than 1 will usually speed up plotting at the
#'                expense of resolution
#'
#' @param newArr logical, whether this is a new arrangement or just adding to a previous one
#'
#' @author Eliot McIntire
#' @keywords internal
#' @importFrom raster ncell
#' @include plotting-classes.R
#' @rdname prepareRaster
# igraph exports %>% from magrittr
.prepareRaster <- function(grobToPlot, zoomExtent, legendRange,
                           takeFromPlotObj, arr, speedup, newArr) {
  if (is.null(zoomExtent)) {
    zoom <- extent(grobToPlot)
    npixels <- ncell(grobToPlot)
  } else {
    zoom <- zoomExtent
    npixels <- ncell(crop(grobToPlot, zoom))
  }
  if (is.null(legendRange)) {
    legendRange <- NA
  }

  if (speedup > 0.1) {
    maxpixels <- min(5e5, 3e4 / (arr@columns * arr@rows) * prod(arr@ds)) %>%
      `/`(., speedup) %>%
      min(., npixels)
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
#' Merges two \code{.quickPlot} objects
#'
#' @param newSP  The "new" \code{.quickPlot} object.
#'               I.e., the new merges and overwrites into current.
#'
#' @param curr   The "current" \code{.quickPlot} object.
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
    })) %>% unlist() # nolint

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
#' @param sPlot A \code{.quickPlot} object.
#' @inheritParams Plot
#'
#' @author Eliot McIntire
#' @export
#' @importFrom grDevices dev.cur dev.new dev.size
#' @importFrom sp bbox
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

      # rewritten for clarity/brevity with pipes below
      whBest <- apply(colByRow, 1, function(x) x[1] / x[2]) %>%
        `-`(., dsDimensionRatio) %>%
        abs() %>%
        which.min()

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
#' \code{speedup} is only used for \code{SpatialPolygons}, \code{SpatialPoints},
#' and \code{SpatialLines} in this function.
#' Attempts have been made to subsample at a good level that optimizes speed of
#' plotting, without losing visible quality. Nevertheless, to force all points to
#' be plotted, use a speedup value less than 0.1.
#' From a speed perspective, there appears to be an optimal subsampling when
#' using \code{thin} from the \pkg{fastshp} package.
#' Presumably, too much thinning requires large distance matrices to be
#' calculated, slowing plotting down.
#' Too little thinning causes an overabundance of points to be plotted, slowing
#' plotting down.
#'
#' The suggested package \code{fastshp} can be installed with:
#' \code{install.packages("fastshp", repos = "https://rforge.net", type = "source")}.
#'
#' NOTE: you may get errors relating to not having installed the software tools
#' required for building R packages on your system.
#' For building on Windows, you'll need to install \code{Rtools} from
#' \url{https://cran.r-project.org/bin/windows/Rtools/}.
#'
#' @param grobToPlot  \code{Raster*}, \code{SpatialLines*},
#'                    \code{SpatialPoints*}, or \code{SpatialPolygons*} object.
#'
#' @param col     Currently only used for the legend of a \code{Raster*} object.
#'
#' @param size    The size of the \code{SpatialPoints}.
#'
#' @param gp      \code{grid} parameters, usually the output of a call to
#'                \code{\link{gpar}}.
#'
#' @param gpText  \code{gpar} object for legend label text.
#'
#' @param legend  Logical indicating whether a legend should be drawn.
#'                Default \code{TRUE}.
#'
#' @param legendText  Vector of values to use for legend value labels.
#'                    Defaults to \code{NULL} which results in a pretty numeric
#'                    representation. If \code{Raster*} has a Raster Attribute
#'                    Table (rat; see \pkg{raster} package), this will be used
#'                    by default. Currently, only a single vector is accepted.
#'
#' @param length  Numeric.
#'
#' @param minv    The minimum value on a \code{Raster*}. Required because not
#'                all Rasters have this defined internally.
#'
#' @param maxv    The maximum value on a \code{Raster*}. Required because not
#'                all Rasters have this defined internally.
#'
#' @param pch     Point character for \code{SpatialPoints}, as \code{par}.
#'
#' @param real    Logical indicating whether the data are \code{real} numbers
#'                (i.e., as opposed to \code{integer} or \code{factor}).
#'
#' @param speedup Numeric. The factor by which the number of vertices in
#'                \code{SpatialPolygons} and \code{SpatialLines*} will be
#'                subsampled. The vertices are already subsampled by default to
#'                make plotting faster.
#'
#' @param vp      whole viewport tree of \code{quickPlotGrob}
#'
#' @param name    Character string of name of object being plotted.
#'
#' @param ...     Additional arguments. None currently implemented.
#'
#' @author Eliot McIntire
#' @export
#' @exportMethod .plotGrob
#' @importFrom data.table ':=' data.table
#' @importFrom grDevices as.raster
#' @importFrom grid gpar gTree gList rasterGrob textGrob grid.draw
#' @importFrom sp proj4string
#' @importFrom raster extent pointDistance xmin xmax ymin ymax
#' @keywords internal
#' @rdname plotGrob
#'
setGeneric(
  ".plotGrob",
  function(grobToPlot, col = NULL, real = FALSE, size = unit(5, "points"), minv, maxv,
           legend = TRUE, legendText = NULL, length = NULL, gp = gpar(), gpText = gpar(),
           pch = 19, speedup = 1, name = character(), vp = list(), ...) {
  standardGeneric(".plotGrob")
})

#' @rdname plotGrob
#' @importFrom grid pointsGrob
setMethod(
  ".plotGrob",
  signature = c("SpatialPoints"),
  definition = function(grobToPlot, col, size,
                        legend, gp = gpar(), pch, speedup, name, vp, ...) {
    speedupScale <- 40
    speedupScale <- if (grepl(proj4string(grobToPlot), pattern = "longlat")) {
      pointDistance(
        p1 = c(xmax(extent(grobToPlot)), ymax(extent(grobToPlot))),
        p2 = c(xmin(extent(grobToPlot)), ymin(extent(grobToPlot))),
        lonlat = TRUE
      ) / (4.8e5 * speedupScale)
    } else {
      max(ymax(extent(grobToPlot)) - ymin(extent(grobToPlot)),
          xmax(extent(grobToPlot)) - xmin(extent(grobToPlot))) /
        speedupScale
    }
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
          message(
            paste(
              "To speed up Polygons plotting using Plot install the fastshp package:\n",
              "install.packages(\"fastshp\", repos=\"https://rforge.net\", type=\"source\")."
            )
          )
          if (Sys.info()[["sysname"]] == "Windows") {
            message(
              paste(
                "You may also need to download and install Rtools from:\n",
                " https://cran.r-project.org/bin/windows/Rtools/"
              )
            )
          }
        }
      }
    }

    pntGrob <- gTree(
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
    grid.draw(pntGrob, recording = FALSE)
    return(invisible(pntGrob))
})

#' @rdname plotGrob
setMethod(
  ".plotGrob",
  signature = c("matrix"),
  definition = function(grobToPlot, col, real, size, minv, maxv,
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
})

#' @rdname plotGrob
#' @importFrom grid polygonGrob
setMethod(
  ".plotGrob",
  signature = c("SpatialPolygons"),
  definition = function(grobToPlot, col, size,
                        legend, gp = gpar(), pch, speedup, name, vp, ...) {

    speedupScale <- if (grepl(proj4string(grobToPlot), pattern = "longlat")) {
      pointDistance(
        p1 = c(xmax(extent(grobToPlot)), ymax(extent(grobToPlot))),
        p2 = c(xmin(extent(grobToPlot)), ymin(extent(grobToPlot))),
        lonlat = TRUE
      ) / 1.2e10
    } else {
      max(ymax(extent(grobToPlot)) - ymin(extent(grobToPlot)),
          xmax(extent(grobToPlot)) - xmin(extent(grobToPlot))) / 2.4e4
    }

    if (is.null(gp$fill)) {
      gp$fill <- rep(RColorBrewer::brewer.pal(8, "Set2"), length.out = length(grobToPlot))
    }

    # For speed of plotting
    xyOrd <- quickPlot::thin(grobToPlot, tolerance = speedupScale * speedup,
                             returnDataFrame = TRUE, minCoordsToThin = 1e5, ...)

    polyGrob <- .createPolygonGrob(gp = gp, xyOrd = xyOrd)
    grid.draw(polyGrob, recording = FALSE)
    return(invisible(polyGrob))
})

#' @rdname plotGrob
#' @importFrom grid polylineGrob arrow
setMethod(
  ".plotGrob",
  signature = c("SpatialLines"),
  definition = function(grobToPlot, col, size,
                        legend, length, gp = gpar(), pch, speedup, name, vp, ...) {
    speedupScale <- if (grepl(proj4string(grobToPlot), pattern = "longlat")) {
      pointDistance(
        p1 = c(xmax(extent(grobToPlot)), ymax(extent(grobToPlot))),
        p2 = c(xmin(extent(grobToPlot)), ymin(extent(grobToPlot))),
        lonlat = TRUE
      ) / 1.2e10
    } else {
      max(ymax(extent(grobToPlot)) - ymin(extent(grobToPlot)),
          xmax(extent(grobToPlot)) - xmin(extent(grobToPlot))) / 2.4e4
    }

    # For speed of plotting
    xy <- lapply(1:length(grobToPlot), function(i) {
      grobToPlot@lines[[i]]@Lines[[1]]@coords
    })
    idLength <- unlist(lapply(xy, length)) / 2
    xy <- do.call(rbind, xy)

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
          message(
            paste(
              "To speed up Lines plotting using Plot, install the fastshp package:\n",
              "install.packages(\"fastshp\", repos=\"https://rforge.net\", type=\"source\")"
            )
          )
          if (Sys.info()[["sysname"]] == "Windows") {
            message(
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
})

#' Make an optimal layout of plots
#'
#' Internal function. Using the size of the current device, and number and
#' dimension ratios of the plots, place them optimally in the plotting region.
#'
#' @param arr an object of class \code{.arrangement}.
#'
#' @param visualSqueeze Numeric. The proportion of the white space to be used
#'                      for plots. Default is 0.75.
#'
#' @param legend Logical indicating whether legend should be included as part of
#'               layout calculation. Default is \code{TRUE}.
#'
#' @param axes Logical indicating whether the axes should be included as part of
#'             layout calculation. Default is \code{TRUE}.
#'
#' @param title Logical indicating whether the names of each plot should be
#'              written above plots and should be included as part of layout
#'               calculation. Default is \code{TRUE}.
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
#' @param sPlot An object of class \code{.quickPlot}.
#'
#' @param newArr  Logical indicating whether this function will create a
#'                completely new viewport. Default \code{FALSE}.
#'
#' @author Eliot McIntire
#' @include plotting-classes.R
#' @importFrom grid viewport vpTree vpList
#' @importFrom raster xmin xmax ymin ymax extent
#' @keywords internal
#' @rdname makeViewports
#'
.makeViewports <- function(sPlot, newArr = FALSE) {
  arr <- sPlot@arr
  sgl <- sPlot@quickPlotGrobList

  extents <- unlist(sapply(sgl, function(x) {
    unname(lapply(x[[1]]@isSpatialObjects, function(z) {
      if (z == TRUE) {
        # for spatial objects
        if (!is.null(x[[1]]@plotArgs$zoomExtent)) {
          x[[1]]@plotArgs$zoomExtent
        } else {
          extent(eval(parse(text = x[[1]]@objName), envir = x[[1]]@envir))
        }
      } else {
        obj <- eval(parse(text = x[[1]]@objName), envir = x[[1]]@envir)
        # if the object has an extent method
        if (hasMethod("extent", is(obj)[1]) & !is.list(obj)) {
          # list has an extent method, but too general
          extent(obj)
        } else {
          # for non spatial objects
          extent(c(xmin = 0, xmax = 2, ymin = 0, ymax = 2))
        }
      }
    }))
  }))

  columns <- arr@columns
  rows <- arr@rows
  gl1 <- grid.layout(
    nrow = rows * 3 + 2, ncol = columns * 3 + 2,
    widths = arr@layout$wdth, heights = arr@layout$ht
  )
  topVp <- viewport(layout = gl1,
                    name = "top"
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
    # makes equal scale
    yrange <- extents[[extentInd]]@ymax - extents[[extentInd]]@ymin
    if (yrange > 0) {
      if (abs((yrange / (extents[[extentInd]]@xmax - extents[[extentInd]]@xmin)) - # nolint
        (biggestDims[1] / biggestDims[2])) > getOption("quickPlot.tolerance")) {
        dimensionRatio <- arr@layout$wdthUnits * arr@ds[1] /
          (arr@layout$htUnits * arr@ds[2])
        plotScaleRatio <- (extents[[extentInd]]@xmin - extents[[extentInd]]@xmax) /
          (extents[[extentInd]]@ymin - extents[[extentInd]]@ymax)

        vS.w <- min(1, plotScaleRatio / dimensionRatio) # nolint
        vS.h <- min(1, dimensionRatio / plotScaleRatio) # nolint

        addX <- abs((extents[[extentInd]]@xmax - extents[[extentInd]]@xmin) * 0.025) # nolint
        addY <- abs((extents[[extentInd]]@ymax - extents[[extentInd]]@ymin) * 0.025) # nolint
        # addY <- abs(extents[[extentInd]]@ymax - extents[[extentInd]]@ymin -
        #               (extents[[extentInd]]@ymax - extents[[extentInd]]@ymin) /
        #               vS.h) / 2
        # addX <- abs(extents[[extentInd]]@xmax - extents[[extentInd]]@xmin -
        #               (extents[[extentInd]]@xmax - extents[[extentInd]]@xmin) /
        #               vS.w) / 2
      } else {
        addY <- addX <- 0
      }
    } else {
      addX <- abs((extents[[extentInd]]@xmax - extents[[extentInd]]@xmin) * 0.025) # nolint
      addY <- abs((extents[[extentInd]]@ymax - extents[[extentInd]]@ymin) * 0.025) # nolint
    }
    # end equal scale
    plotVps[[nam[extentInd]]] <- viewport(
      clip = "on",
      name = nam[extentInd],
      layout.pos.col = lpc,
      layout.pos.row = lpr,
      xscale = c(extents[[extentInd]]@xmin - addX, extents[[extentInd]]@xmax + addX),
      yscale = c(extents[[extentInd]]@ymin - addY, extents[[extentInd]]@ymax + addY)
    )
    plotVps[[paste0("outer", nam[extentInd])]] <- viewport(#clip = "on",
      name = paste0("outer", nam[extentInd]),
      layout.pos.col = lpc,
      layout.pos.row = lpr,
      xscale = c(extents[[extentInd]]@xmin - addX, extents[[extentInd]]@xmax + addX),
      yscale = c(extents[[extentInd]]@ymin - addY, extents[[extentInd]]@ymax + addY)
    )
  }

  wholeVp <- vpTree(topVp, do.call(vpList, plotVps))

  return(list(wholeVp = wholeVp, extents = extents))
}

#' Test whether class has bbox method
#'
#' For internal use only.
#'
#' @rdname hasBbox
#' @param z Logical, whether this object is a SpatialObject
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
    # for spatial objects
    apply(bbox(eval(parse(text = objName), envir = objEnv)), 1, function(y) {
      diff(range(y))
    })
  } else {
    # for non spatial objects
    c(1, 1)
  }
}

#' Convert pairs of coordinates to SpatialLines
#'
#' This will convert 2 objects whose coordinates can be extracted with \code{coordinates}
#' (e.g., \code{sp::SpatialPoints*})
#' to a single SpatialLines
#' object. The first object is treated as the "to" or destination, and the
#' second object the "from" or source. This can be used to represent
#' directional SpatialLines, especially with with arrow heads, as in
#' \code{Plot(sl, length = 0.1)}
#'
#' @export
#' @importFrom sp SpatialLines Lines Line coordinates
#' @param sp1 SpatialPoints* object
#' @param from SpatialPoints* object. Optional. If not provided, then the function
#'             will attempt to find the "previous" coordinates as columns
#'             (\code{prevX}, \code{prevY}) in the \code{sp1} object.
#' @examples
#' caribou <- sp::SpatialPoints(coords = cbind(x = stats::runif(1e1, -50, 50),
#'                                         y = stats::runif(1e1, -50, 50)))
#' caribouFrom <- sp::SpatialPoints(coords = cbind(x = stats::runif(1e1, -50, 50),
#'                                         y = stats::runif(1e1, -50, 50)))
#' caribouLines <- sp2sl(caribou, caribouFrom)
#' Plot(caribouLines, length = 0.1)
sp2sl <- function(sp1, from) {
  l <- vector("list", NROW(sp1))
  beginCoord <- coordinates(sp1)
  if (missing(from)) {
    endCoord <- sp1[, c("prevX", "prevY")]
  } else {
    endCoord <- coordinates(from)
  }

  for (i in seq_along(l)) {
    l[[i]] <- Lines(list(Line(rbind(beginCoord[i, ], endCoord[i, ]))), as.character(i))
  }

  SpatialLines(l)
}

#' Thin a polygon using \code{fastshp::thin}
#'
#' For visualizing, it is sometimes useful to remove points in Spatial* objects.
#' This will change the geometry, so it is not recommended for computation.
#' This is similar to \code{rgeos::gSimplify} and \code{sf::st_simplify},
#' but faster than both (see examples) for large shapefiles, particularly if
#' \code{returnDataFrame} is \code{TRUE}.
#' \emph{\code{thin} will not attempt to preserve topology.}
#' It is strictly for making smaller polygons for the purpose (likely)
#' of visualizing more quickly.
#'
#' @param x A Spatial* object
#' @param tolerance Maximum allowable distance for a point to be removed.
#' @param returnDataFrame If \code{TRUE}, this will return a list of 3 elements,
#'        \code{xyOrd}, \code{hole}, and \code{idLength}.
#'        If \code{FALSE} (default), it will return a \code{SpatialPolygons} object.
#' @param minCoordsToThin If the number of coordinates is smaller than this number,
#'        then thin will just pass through, though it will take the time required to
#'        calculate how many points there are (which is not NROW(coordinates(x)) for
#'        a SpatialPolygon)
#' @param ... Passed to methods (e.g., \code{maxNumPolygons})
#' @param maxNumPolygons For speed, \code{thin} can also simply remove some of the
#'        polygons. This is likely only a reasonable thing to do if there are
#'        a lot of polygons being plotted in a small space. Current default is
#'        taken from \code{options('quickPlot.maxNumPolygons')}, with a message.
#'
#' @export
#' @importFrom data.table as.data.table data.table set
#' @importFrom raster xmax xmin
#' @importFrom sp CRS Polygon Polygons SpatialPolygons SpatialPolygonsDataFrame
#' @rdname thin
#'
#' @examples
#' library(raster)
#'
#' b <- SpatialPoints(cbind(-110, 59, 1000))
#' crs(b) <- sp::CRS("+init=epsg:4326")
#'
#' crsObj <- CRS(paste0("+proj=tmerc +lat_0=0 +lon_0=-115 +k=0.9992 +x_0=500000 +y_0=0 ",
#'                      "+datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"))
#'
#' # make a random polygon -- code adapted from SpaDES.tools::randomPolygon package:
#'   areaM2 <- 1000 * 1e4 * 1.304 # rescale so mean area is close to hectares
#'   b <- spTransform(b, crsObj)
#'
#'   radius <- sqrt(areaM2 / pi)
#'
#'   meanX <- mean(coordinates(b)[, 1]) - radius
#'   meanY <- mean(coordinates(b)[, 2]) - radius
#'
#'   minX <- meanX - radius
#'   maxX <- meanX + radius
#'   minY <- meanY - radius
#'   maxY <- meanY + radius
#'
#' # Add random noise to polygon
#'   xAdd <- round(runif(1, radius * 0.8, radius * 1.2))
#'   yAdd <- round(runif(1, radius * 0.8, radius * 1.2))
#'   nPoints <- 20
#'   betaPar <- 0.6
#'   X <- c(jitter(sort(rbeta(nPoints, betaPar, betaPar) * (maxX - minX) + minX)),
#'         jitter(sort(rbeta(nPoints, betaPar, betaPar) * (maxX - minX) + minX, decreasing = TRUE)))
#'   Y <- c(jitter(sort(rbeta(nPoints / 2, betaPar, betaPar) * (maxY - meanY) + meanY)),
#'          jitter(sort(rbeta(nPoints, betaPar, betaPar) * (maxY - minY) + minY, decreasing = TRUE)),
#'          jitter(sort(rbeta(nPoints / 2, betaPar, betaPar) * (meanY - minY) + minY)))
#'
#'   Sr1 <- Polygon(cbind(X + xAdd, Y + yAdd))
#'   Srs1 <- Polygons(list(Sr1), "s1")
#'   a <- SpatialPolygons(list(Srs1), 1L)
#'   crs(a) <- crsObj
#' # end of making random polygon
#'
#' clearPlot()
#' Plot(a)
#' NROW(a@polygons[[1]]@Polygons[[1]]@coords)
#' if (require(fastshp)) {
#'   aThin <- quickPlot::thin(a, 200)
#'   NROW(aThin@polygons[[1]]@Polygons[[1]]@coords) # fewer
#'   Plot(aThin) # looks similar
#' }
#'
#' # compare -- if you have rgeos
#' # if (require("rgeos")) {
#' #   aSimplify <- gSimplify(a, tol = 200)
#' #   NROW(aSimplify@polygons[[1]]@Polygons[[1]]@coords) # fewer
#' #   Plot(aSimplify)
#' # }
#'
#' # compare -- if you have sf
#' # if (require("sf")) {
#' #   aSF <- st_simplify(st_as_sf(a), dTolerance = 200)
#' #   # convert to Spatial to see how many coordinates
#' #   aSF2 <- as(aSF, "Spatial")
#' #   NROW(aSF2@polygons[[1]]@Polygons[[1]]@coords) # fewer
#' #   Plot(aSF)
#' # }
#'
#' # thin is faster than rgeos::gSimplify and sf::st_simplify on large shapefiles
#' \dontrun{
#'   # this involves downloading a 9 MB file
#'   setwd(tempdir())
#'   albertaEcozoneFiles <- c("Natural_Regions_Subregions_of_Alberta.dbf",
#'                            "Natural_Regions_Subregions_of_Alberta.lyr",
#'                            "Natural_Regions_Subregions_of_Alberta.prj",
#'                            "Natural_Regions_Subregions_of_Alberta.shp.xml",
#'                            "Natural_Regions_Subregions_of_Alberta.shx",
#'                            "natural_regions_subregions_of_alberta.zip",
#'                            "nsr2005_final_letter.jpg", "nsr2005_final_letter.pdf")
#'   albertaEcozoneURL <- paste0("https://www.albertaparks.ca/media/429607/",
#'                               "natural_regions_subregions_of_alberta.zip")
#'   albertaEcozoneFilename <- "Natural_Regions_Subregions_of_Alberta.shp"
#'   zipFilename <- basename(albertaEcozoneURL)
#'   download.file(albertaEcozoneURL, destfile = zipFilename)
#'   unzip(zipFilename, junkpaths = TRUE)
#'   a <- raster::shapefile(albertaEcozoneFilename)
#'
#'   # compare -- if you have rgeos and sf package
#'   # if (require("sf")) {
#'   #   aSF <- st_as_sf(a)
#'   # }
#'   # if (require("rgeos") && require("sf")) {
#'     # thin at 10m
#'     microbenchmark::microbenchmark(times = 20
#'                                    , thin(a, 10),
#'                                    , thin(a, 10, returnDataFrame = TRUE) # much faster
#'    #                               , gSimplify(a, 10),
#'    #                               , st_simplify(aSF, dTolerance = 10))
#'                                   )
#'    # Unit: milliseconds
#'    #                              expr      min   median      max neval cld
#'    # thin(a, 10)                        989.812 1266.393 1479.879     6  a
#'    # gSimplify(a, 10   )               4020.349 4211.414 8881.535     6   b
#'    # st_simplify(aSF, dTolerance = 10) 4087.343 4344.936 4910.299     6   b
#'   #}
#' }
thin <- function(x, tolerance, returnDataFrame, minCoordsToThin, ...) {
  UseMethod("thin")
}

#' @export
#' @rdname thin
thin.SpatialPolygons <- function(x, tolerance = NULL, returnDataFrame = FALSE, minCoordsToThin = 0,
                                 maxNumPolygons = getOption("quickPlot.maxNumPolygons", 3e3), ...) {

  # For speed of plotting
  xyOrd <- .fortify(x, matchFortify = FALSE,
                    simple = returnDataFrame, maxNumPolygons) # a list: out, hole, idLength
  if (is.null(tolerance)) {
    tolerance <- (raster::xmax(x) - raster::xmin(x)) * 0.0001
    message("tolerance set to ", tolerance)
  }
  if (requireNamespace("fastshp", quietly = TRUE)) {
    if (NROW(xyOrd[["out"]]) > minCoordsToThin) {
      message("Some polygons have been simplified")
      thinRes <- fastshp::thin(xyOrd[["out"]]$x, xyOrd[["out"]]$y,
                             tolerance = tolerance, id = xyOrd[["out"]]$groups)

      set(xyOrd[["out"]], , "thinRes", thinRes)
      xyOrd[["out"]][, keepAll := sum(thinRes) < 4, by = groups]

      xyOrd[["out"]] <- xyOrd[["out"]][thinRes | keepAll]

      #xyOrd[["out"]] <- xyOrd[["out"]][thinRes, ]# thin line
      if (returnDataFrame) {
        xyOrd[["idLength"]] <- xyOrd[["out"]][, list(V1 = .N), by = groups]
      } else {
        # clean up a bit
        set(xyOrd[["out"]], , "order", NULL)
        set(xyOrd[["out"]], , "groups", NULL)

        polyList <- split(xyOrd[["out"]], by = c("Polygons", "Polygon"),
                           flatten = FALSE, keep.by = FALSE)
        bb <- lapply(unique(xyOrd$out$Polygons), function(outerI) {
          poly <- lapply(seq(polyList[[outerI]]), function(innerI) {
            #Polygon(as.matrix(polyList[[outerI]][[innerI]][, c("x", "y")]),
            Polygon(cbind(polyList[[outerI]][[innerI]]$x, polyList[[outerI]][[innerI]]$y),
                    hole = unique(as.logical(polyList[[outerI]][[innerI]]$hole)))
          })
          Polygons(poly, ID = outerI)
        })

        names1 <- unique(xyOrd$out$Polygons)
        xyOrd <- SpatialPolygons(bb, proj4string = CRS(proj4string(x)))
        if (is(x, "SpatialPolygonsDataFrame")) {
          if (length(x) > maxNumPolygons) {
            dat <- x@data[as.numeric(names1) + 1,]
            #row.names(dat) <- as.character(seq_len(length(xyOrd)))
          } else {
            dat <- x@data
          }
          xyOrd <- SpatialPolygonsDataFrame(xyOrd, data = dat)
        }

        return(xyOrd)
      }
    }
  } else {
    message(
      paste(
        "To speed up Polygons plotting using Plot install the fastshp package:\n",
        "install.packages(\"fastshp\", repos=\"https://rforge.net\", type=\"source\")."
      )
    )
    if (Sys.info()[["sysname"]] == "Windows") {
      message(
        paste(
          "You may also need to download and install Rtools from:\n",
          " https://cran.r-project.org/bin/windows/Rtools/"
        )
      )
    }
  }
  xyOrd <- list(xyOrd = xyOrd[["out"]], hole = xyOrd[["hole"]],
                idLength = xyOrd[["idLength"]])
}

#' @export
#' @rdname thin
thin.default <- function(x, tolerance, returnDataFrame, minCoordsToThin, ...) {
  message("No method for that class of object exists. See methods('thin') to see current methods")
}

#' Fortify - i.e,. convert an arbitrary object to a data.frame-like object
#'
#' This only deals with SpatialPolygons.
#'
#' @rdname fortify
#' @name fortify
#' @importFrom data.table setDT set
#' @keywords internal
.fortify <- function(x, matchFortify = TRUE, simple = FALSE,
                     maxNumPolygons = getOption("quickPlot.maxNumPolygons", 3e3)) {
  ord <- x@plotOrder
  if (length(ord) > maxNumPolygons) {

    polygonSeq <- .polygonSeq(x, maxNumPolygons) #if (is.numeric(x@data$Shape_Area)) {
    ord <- ord[polygonSeq]
    .showingOnlyMessage(numShowing = maxNumPolygons,
                        totalAvailable = length(x@plotOrder))
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

  numPolygons <- unlist(length(xyOrd.l))
  numPolygon <- unlist(lapply(xyOrd.l, length))

  xyOrd <- do.call(rbind, lapply(xyOrd.l, function(i) {
    do.call(rbind, i)
  }))

  groups <- rep(1:NROW(idLength), idLength$V1)
  if (!simple | matchFortify) {
    # Polygons <- rep(rep(seq(numPolygons), numPolygon), idLength$V1) # sequential numbering
    Polygons <- rep(rep(IDs, numPolygon), idLength$V1) # actual ID labelling
    Polygon <- rep(unlist(lapply(numPolygon, seq)), idLength$V1)
    holes <- rep(hole, idLength$V1)
    orders <- unlist(lapply(idLength$V1, seq))
  }

  if (matchFortify) {
    if (!simple) message("for matchFortify = TRUE, simple is set to FALSE")
    return(data.frame(lat = xyOrd[,1], long = xyOrd[,2], order = orders,
                      hole = holes, id = Polygons, piece = Polygon,
                      #group = paste0(as.character(Polygons), ".", as.character(Polygon)))) # the actual fortify
                      group = groups))
  } else {
    out <- setDT(data.frame(x = xyOrd[,1], y = xyOrd[,2], groups = groups))
    if (!simple) {
      set(out, , "order", orders)
      set(out, , "hole", holes)
      set(out, , "Polygons", Polygons)
      set(out, , "Polygon", Polygon)
    }
    out <- list(out = out, hole = hole, idLength = idLength)

    return(out)
  }
}

.polygonSeq <- function(polygon, maxNumPolygons) {
  if (is.numeric(polygon@data$Shape_Area)) {
    which(polygon@data$Shape_Area>(sort(polygon@data$Shape_Area, decreasing = TRUE)[maxNumPolygons]))
  } else {
    round(seq(1, length(polygon), length.out = maxNumPolygons))
  }

}

.createPolygonGrob <- function(gp, xyOrd) {
  gp$fill[xyOrd[["hole"]]] <- "#FFFFFF00"
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

.showingOnlyMessage <- function(numShowing, totalAvailable) {
  message("Showing only ", numShowing, " of ",
          totalAvailable," polygons in this view. See options('quickPlot.maxNumPolygons')")

}
