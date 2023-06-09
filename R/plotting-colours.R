################################################################################
#' Get and set colours for plotting `Raster*` objects
#'
#' @param object     A `Raster*` object.
#'
#' @return Returns a named list of colours.
#'
#' @aliases getColours
#' @author Alex Chubaty
#' @export
#' @rdname getSetColors
#'
#' @seealso [setColors<-()], `brewer.pal()`, RColorBrewer::ColorBrewer
#'
#' @example inst/examples/example_setColors.R
#'
getColors <- function(object) {
  nams <- names(object)
  if (isRaster(object)) {
    if (isRaster(object)) {
      cols <- lapply(nams, function(x) {
        raster::colortable(object[[x]])
      })
    }
  } else if (isSpat(object) && isGridded(object)) {
    cols <- terra::coltab(object)
    cols <- lapply(cols, colorsRGBtoHex)
    noCols <- vapply(cols, is.null, FUN.VALUE = logical(1))
    if (any(noCols)) {
      # theSeq <- minFn(object):maxFn(object)
      cols <- lapply(cols, function(x) rev(grDevices::terrain.colors(50))) # default in SpatRaster
      # df <- data.frame(value = theSeq, color =  cols)
    }
  } else if (isSpatial(object)) {
    cols <- list(object@data$color)
  } else {
    cols <- NULL
  }
  if (!is.null(nams))
    names(cols) <- nams

  return(cols)
}

#' `setColors` works as a replacement method or a normal function call.
#' This function can accept `RColorBrewer` colours by name. See examples.
#'
#' @param ...   Additional arguments to `colorRampPalette`.
#'
#' @param n     An optional vector of values specifying the number
#'              of levels from which to interpolate the colour palette.
#'
#' @param value  Named list of hex colour codes (e.g., from
#'               `RColorBrewer::brewer.pal`), corresponding to the names
#'               of `RasterLayer`s in `x`.
#'
#' @return Returns a Raster with the `colortable` slot set to `values`.
#'
#' @aliases setColours
#' @export
#' @inheritParams Plot
#' @importFrom grDevices colorRampPalette
#' @rdname getSetColors
#'
#' @seealso `brewer.pal()`, `RColorBrewer::ColorBrewer`,
#'          [`colorRampPalette()`][grDevices::colorRamp].
#'
`setColors<-` <- function(object, ..., n, verbose = getOption("quickPlot.verbose"), value) {
    if (dim(object)[[3]] > 1) { # Multi-layer RasterStack or SpatRaster
      if (!is(value, "list")) {
        value <- lapply(seq(numLayers(object)), function(x) value)
      }
      if (isTRUE(1 == length(n))) {
        n <- rep(n, numLayers(object))
      }
      object <- Map(lay = seq(numLayers(object)), ..., value = value, n = n,
                 f = function(lay, ..., n, value) {
                   # this allows same code for SpatRaster and RasterStack; don't pass object = object
                   `setColors<-`(object[[lay]], ..., n = n, value = value, verbose = verbose)
                 })
      if (isRaster(object[[1]])) {
        object <- raster::stack(object)
      }
      if (is(object[[1]], "SpatRaster")) object <- terra::rast(object)
      return(object)
    }

    if (is(value, "list")) { # value = "list"
      nams <- names(object)
      i <- which(nams %in% names(value))
      for (x in nams[i]) {
        setColors(object[[x]], ..., verbose = verbose) <- value[[x]]
      }
      return(object)
    }

    if (missing(n)) {
      isFac <- if (!terra::is.factor(object)) {
        FALSE
      } else {
        if (all(na.omit(object[]) %% 1 == 0)) {
          ## some factor rasters are actually real number -- makes no sense
          TRUE
        } else {
          FALSE
        }
      }

      if (!isFac) {
        n1 <- round(maxFn(object) - minFn(object)) + 1
      } else {
        n1 <- length(value)
      }

      setColors(object, n = n1, verbose = verbose) <- value
      return(object)
    }

    if (any(is.na(n))) {
      object <- setColors(object = object, value = value)
      return(object)
    }
    if (terra::is.factor(object)) {
      a <- object[];
      a <- a[!is.na(a)]
      isInteger <- !any(a != as.integer(a))
      # isInteger <- all(na.omit(object[])%%1==0)
      if (isInteger) { # some factor rasters are actually real number -- makes no sense
        levs <- terra::levels(object)[[1]]
        nrLevs <- NROW(levs)
        if (n != nrLevs) {
        # if (n != NROW(object@data@attributes[[1]])) {
          messageVerbose("Number of colours not equal number of values: interpolating",
                         verbose = verbose)
          n <- nrLevs
        }
      }
    }
    rcolbrewInfo <- RColorBrewer::brewer.pal.info
    if ((value %in% row.names(rcolbrewInfo))[1]) { # nolint
      if (n > rcolbrewInfo[value, "maxcolors"]) {
        ntmp <- rcolbrewInfo[value, "maxcolors"]
      } else {
        ntmp <- n
      }
      value <- RColorBrewer::brewer.pal(ntmp, value)
    }
    if (terra::is.factor(object)) {
      if (isInteger) { # some factor rasters are actually real number -- makes no sense
        levs <- terra::levels(object)[[1]]
        nrLevs <- NROW(levs)

        pal <- colorRampPalette(value, alpha = TRUE, ...)
        if (n != nrLevs) {
          vals <- pal(n)
        } else {
          vals <- value
        }
        if (isRaster(object))
          object@legend@colortable <- pal(n)
        else
          terra::coltab(object) <- vals

      }
    } else {
      pal <- colorRampPalette(value, alpha = TRUE, ...)
      if (isRaster(object)) {
        raster::colortable(object) <- pal(n)
      } else {
        terra::coltab(object) <- pal(n)
      }

    }
    return(object)
}

# @export
# @rdname getSetColors
# setReplaceMethod(
#  "setColors",
#  signature("RasterLayer", "missing", "character"),
#   function(object, ..., value) {
    # isFac <- if (!raster::is.factor(object)) {
    #   FALSE
    # } else {
    #   if (all(na.omit(object[]) %% 1 == 0)) {
    #     ## some factor rasters are actually real number -- makes no sense
    #     TRUE
    #   } else {
    #     FALSE
    #   }
    # }
    #
    # if (!isFac) {
    #   n <- round(maxFn(object) - minFn(object)) + 1
    # } else {
    #   n <- length(value)
    # }
    #
    # setColors(object, n = n) <- value
    # if (!is.character(object@legend@colortable)) stop("setColors needs colour character values")
    # return(object)
# }#)

# @export
# @rdname getSetColors
# setReplaceMethod(
  # "setColors",
  #  signature("RasterStack", "numeric", "list"),
  #  function(object, ..., n, value) {
#      i <- which(names(object) %in% names(value))
#      if (length(i) == 0) stop("Layer names do not match stack layer names")
#      whValNamed <- names(value)[i] %in% names(n)
#      whNNamed <- names(n) %in% names(value)[i]
#      nFull <- n
#      if (length(n) != length(i)) {
#        # not enough n values
#        if (sum(!nzchar(names(n), keepNA = TRUE)) > 0) {
#          # are there unnamed ones
#          nFull <- rep(n[!whNNamed], length.out = length(i))
#          nFull[whValNamed] <- n[whNNamed]
#          names(nFull)[whValNamed] <- names(n)[whNNamed]
#        } else if (is.null(names(n))) {
#          nFull <- rep(n, length.out = length(i))
#        }
#      }
#
#      for (x in i) {
#        if (x %in% i[whValNamed]) {
#          setColors(object[[names(value)[x]]], ..., n = nFull[[names(value)[x]]]) <-
#            value[[names(value)[x]]]
#        } else {
#          setColors(object[[names(value)[x]]], ..., n = nFull[x]) <- value[[names(value)[x]]]
#        }
#      }
#      return(object)
# }# )

# @export
# @rdname getSetColors
# setReplaceMethod(
#   "setColors",
#    signature("Raster", "missing", "list"),
#    function(object, ..., value) {
     # nams <- names(object)
     # i <- which(nams %in% names(value))
     # for (x in nams[i]) {
     #   setColors(object[[x]], ...) <- value[[x]]
     # }
     # return(object)
# })

# @export
# @rdname getSetColors
# setGeneric("setColors", function(object, value, n) {
#   standardGeneric("setColors")
# })

#' @export
#' @inheritParams Plot
#' @rdname getSetColors
setColors <- function(object, value, n, verbose = getOption("quickPlot.verbose")) {
  if (missing(n)) {
    setColors(object = object, verbose = verbose) <- value
  } else {
    setColors(object = object, n = n, verbose = verbose) <- value
  }
  return(object)
}

# @export
# @rdname getSetColors
# setMethod(
#   "setColors",
#   signature("RasterLayer", "character", "missing"),
#   function(object, value) {
#     setColors(object = object) <- value
#     return(object)
# })

################################################################################
#' Convert Raster to colour matrix usable by raster function for plotting
#'
#' Internal function.
#'
#' @param grobToPlot   A `SpatialObject`.
#'
#' @param zoomExtent   An `Extent` object for zooming to.
#'                     Defaults to whole extent of `grobToPlot`.
#'
#' @param maxpixels    Numeric. Number of cells to subsample the complete
#'                     `grobToPlot`.
#'
#' @param legendRange  Numeric vector giving values that, representing the lower
#'                     and upper bounds of a legend (i.e., `1:10` or
#'                     `c(1,10)` will give same result) that will override
#'                     the data bounds contained within the `grobToPlot`.
#'
#' @param cols         Colours specified in a way that can be understood directly
#'                     or by [colorRampPalette()].
#'
#' @param na.color     Character string indicating the colour for `NA` values.
#'                     Default transparent.
#'
#' @param zero.color   Character string indicating the colour for zero values,
#'                     when zero is the minimum value.
#'                     Otherwise, it is treated as any other colour.
#'                     Default transparent.
#'                     Use `NULL` if zero should be the value given to it
#'                     by the `colortable` associated with the raster.
#'
#' @param skipSample   Logical. If no downsampling is necessary, skip.
#'                     Default `TRUE`.
#'
#' @aliases makeColourMatrix
#' @author Eliot McIntire
#' @importFrom grDevices colorRampPalette terrain.colors
#' @importFrom fpCompare %>>%
#' @importFrom stats na.omit
#' @importFrom utils tail head
#' @include plotting-classes.R
#' @keywords internal
#' @rdname makeColorMatrix
#'
.makeColorMatrix <- function(grobToPlot, zoomExtent, maxpixels, legendRange, cols = NULL,
                             prevMinMax,
            na.color = "#FFFFFF00", zero.color = NULL, skipSample = TRUE) {

  # signature = c("griddedClasses", "Extent", "numeric", "ANY"),
    zoom <- zoomExtent

    isFac <- FALSE
    if (any(terra::is.factor(grobToPlot)))
      if (all(na.omit(grobToPlot[] %% 1) == 0))
        isFac <- TRUE
    # It is 5x faster to access the min and max from the Raster than to
    # calculate it, but it is also often wrong... it is only metadata
    # on the raster, so it is possible that it is incorrect.
    if (!skipSample) {
      colorTable <- getColors(grobToPlot)[[1]]
      if (is.null(prevMinMax$minz)) {
        mn <- minFn(grobToPlot)
      } else {
        mn <- prevMinMax$minz
      }
      if (!inherits(try(mn), "try-error")) {
        minz <- mn
      }
      if (is.null(prevMinMax$maxz)) {
        mx <- maxFn(grobToPlot)
      } else {
        mx <- prevMinMax$maxz
      }
      if (!inherits(try(mx), "try-error")) {
        maxz <- mx
      }

      if (!(is(zoom, "SpatExtent") || is(zoom, "Extent"))) {
        zoom <- unlist(zoom)
        if (isRaster(grobToPlot)) {
          zoom <- raster::extent(zoom)
        } else {
          zoom <- zoom[c("xmin", "xmax", "ymin", "ymax")] # get in correct SpatExtent order
          zoom <- terra::ext(zoom)
        }
      }
      grobToPlot <- terra::crop(grobToPlot, zoom)
      if (terra::ncell(grobToPlot) > maxpixels) {
        if (isRaster(grobToPlot)) {
          grobToPlot <- raster::sampleRegular(grobToPlot, size = maxpixels, asRaster = TRUE)
        } else {
          grobToPlot <- terra::spatSample(grobToPlot, method = "regular", size = maxpixels,
                            as.raster = TRUE)
        }
        # This appears to be modify-in-place on grobToPlot ... that seems bad
      }
      # grobToPlot <- sampleRegular(
      #   x = grobToPlot, size = maxpixels,
      #   ext = zoom, asRaster = TRUE, useGDAL = TRUE
      # )
      if (NROW(colorTable) > 0) {
        cols <- colorTable
      }
    }
    if (isGridded(grobToPlot)) {
      z <- terra::values(grobToPlot)
    } else {
      z <- grobToPlot[]
    }

    # If minFn is defined, then use it, otherwise, calculate them.
    #  This is different than maxz because of the sampleRegular.
    # If the low values in the raster are missed in the sampleRegular,
    #  then the legend will be off by as many as are missing at the bottom;
    #  so, use the metadata version of minFn, but use the max(z) to
    #  accomodate cases where there are too many legend values for the
    # number of raster values.
  #if (!raster::is.factor(grobToPlot)) {
    if (any(is.na(legendRange))) {
      if (!exists("minz")) {
        minz <- suppressWarnings(min(z, na.rm = TRUE))
      }
      if (is.na(minz)) {
        minz <- suppressWarnings(min(z, na.rm = TRUE))
      }
      if (is.infinite(minz)) {
        minz <- 0
      }
      if (!exists("maxz")) {
        maxz <- suppressWarnings(max(z, na.rm = TRUE))
      }
      if (is.na(maxz)) {
        maxz <- suppressWarnings(max(z, na.rm = TRUE))
      }
      if (is.infinite(maxz)) {
        maxz <- 0
      }
    } else {
      minz <- min(legendRange)
      maxz <- max(legendRange)
    }

    real <- any(na.omit(z) %% 1 != 0) # Test for real values or not

    ## Deal with colours - This gets all combinations, real vs. integers,
    ## with zero, with no zero, with NA, with no NA, not enough numbers,
    ## too many numbers
    maxNumCols <- 100

    if (isFac) {
      facLevs <- terra::levels(grobToPlot)[[1]]
      nValues <- NROW(facLevs)
    } else {
      if (any(is.na(legendRange))) {
        nValues <- ifelse(real, maxNumCols + 1, maxz - minz + 1)
      } else {
        nValues <- ifelse(real, maxNumCols + 1,
                          length(seq(legendRange[1], legendRange[length(legendRange)])))
      }
    }

    colTable <- NULL

    # Coming out of SpatRaster, it may be in rgb matrix
    if (is.data.frame(cols))
      cols <- colorsRGBtoHex(cols)
      # cols <- rgb(cols[, "red"]/255, cols[, 'green']/255, cols[, "blue"]/255, cols[, "alpha"]/255)

    if (is.null(cols)) {
      # i.e., contained within raster or nothing
      theCols <- getColors(grobToPlot)[[1]]
      if (NROW(theCols) > 0) {
        colTable <- theCols # This is a vector for RasterLayer, but a data.frame for terra
        lenColTable <- NROW(colTable)

        cols <- if ((nValues > lenColTable) & !isFac) { # nolint
          # not enough colours, use colorRamp
          colTable
          # colorRampPalette(colTable)(nValues)
        } else if ((nValues <= lenColTable) | isFac) { # nolint
          # one more colour than needed:
          #   assume bottom is NA
          if (isFac) {
            factorValues <- terra::levels(grobToPlot)[[1]][["ID"]]
            # factorValues <- grobToPlot@data@attributes[[1]][, 1] %>%
            #   unique() %>%
            #   na.omit() %>%
            #   sort()
            if (length(factorValues) == NROW(colTable)) {
              colTable[seq.int(length(factorValues))]
            } else {
              if ((tail(facLevs$ID, 1) - head(facLevs$ID, 1) + 1) == (NROW(colTable) - 1)) {
                # The case where the IDs are numeric representations
                colTable[factorValues + 1]
              } else {
                colTable[c(1, 1 + factorValues)]
              }

            }
          } else {
            colTable
          }
        } else if (nValues <= (lenColTable - 1)) {
          # one more colour than needed:
          #  assume bottom is NA
          na.color <- colTable[1] # nolint
          colTable[minz:maxz - minz + 2]
        } else if (nValues <= (lenColTable - 2)) {
          # two more colours than needed,
          #  assume bottom is NA, second is white
          na.color <- colTable[1] # nolint
          zero.color <- colTable[2] # nolint
          colTable[minz:maxz - minz + 3]
        } else {
          colTable
        }
      } else {
        # default colour if nothing specified:
        cols <- rev(terrain.colors(nValues))
      }
    } else {
      if (is.character(cols) & (NROW(cols) == 1)) {
        if (cols %in% rownames(RColorBrewer::brewer.pal.info)) {
          suppressWarnings({
            cols <- RColorBrewer::brewer.pal(nValues, cols)
          })
        }
      }
      cols <- if (nValues > NROW(cols)) {
        colorRampPalette(cols)(nValues)
      } else if (nValues < NROW(cols)) {
        if ((minz + nValues - 1)  > NROW(cols)) { # nolint
          # there are enough colours, but they don't start at 1
          cols[minz:maxz - minz + 1 + max(0, 1 - minz)]
        } else {
          cols[minz:maxz + max(0, 1 - minz)]
        }
      } else {
        cols
      }
    }

    # colours are indexed from 1, as with all objects in R, but there
    # are generally zero values on the rasters, so shift according to
    # the minFn value, if it is below 1.
    # Shift it by 2, 1 to make the zeros into two, the other for the
    # NAs to be ones.

    # If object is real numbers, the default above is to discretize.
    # This is particularly bad for numbers below 10.
    # Here, numbers below maxNumCols that are reals will be rescaled
    #  to max = 100.
    # These are, of course, only used for the colour matrix, not the
    #  values on the Raster.

    # Plotting works by making a maxNumCols value raster if
    #  it is a real value original raster. So, we need to keep
    #  the original minz and maxz for legend purposes, but
    #  work with the new rescaled minz and maxz
    minzOrig <- minz
    maxzOrig <- maxz
    whichZero <- numeric()
    whichZeroLegend <- numeric()
    if (!is.null(zero.color)) {
      whichZero <- which(z == 0)
      whichZeroLegend <- which(seq(minz, maxz, length.out = nValues) == 0)
    }

    # Here, rescale so it is between 0 and maxNumCols or nValues
    if (isFac) {
      z <- match(z, facLevs$ID)
    } else {
      if (real) {
        z <- maxNumCols / (maxz - minz) * (z - minz)
        if (length(whichZero)) {
          zeroValue <- maxNumCols / (maxz - minz) * (0 - minz)
        }
      } else {
        # rescale so that the minimum is 1, not <1:
        if (nValues > 1) {
          z <- (nValues - 1) /  (maxz - minz) * (z - minz) + 1
        } else {
          z <- (z - minz) + 1
        }

        if (length(whichZero)) {
          zeroValue <- (nValues - 1) / (maxz - minz) * (0 - minz) + 1
        }

      }
    }
    minz <- suppressWarnings(min(z, na.rm = TRUE))
    maxz <- suppressWarnings(max(z, na.rm = TRUE))
    if (is.infinite(minz)) {
      maxz <- 0
    }
    if (is.infinite(minz)) {
      minz <- 0
    }

    if (any(!is.na(legendRange))) {
      if ((max(legendRange) - min(legendRange) + 1) < NROW(cols)) { # nolint
      } else {
        if (!is.null(colTable)) {
          if (NROW(getColors(grobToPlot)[[1]]) > 0) {
            cols <- colorRampPalette(colTable)(maxzOrig - minzOrig + 1)
          } else {
            # default colour if nothing specified
            cols <- rev(terrain.colors(maxzOrig - minzOrig + 1))
          }
        }
      }
    }

    # Coming out of SpatRaster, it may be in rgb matrix
    if (is.data.frame(cols))
      cols <- colorsRGBtoHex(cols)

    # here, the default colour (transparent) for zero:
    # if it is the minimum value, can be overridden.

    # if range of values is not within the legend range, then give them NA
    if (minz < 0) z[z < 0] <- 0
    if (!isFac) {
      if (real) {
        if (maxz %>>% maxNumCols) z[z %>>% maxNumCols] <- 0
      } else {
        if (maxz %>>% nValues) z[z %>>% nValues] <- 0
      }
    }

    z <- z + 1 # for the NAs
    z[is.na(z)] <- 1

    if (isFac & !is.null(colTable)) {
      # changed from max to length to accommodate zeros or factors not starting at 1
      cols <- rep(na.color, length(factorValues))
      resequence <- seq_along(facLevs$ID) - min(factorValues) + 1
      if (NROW(colTable) == length(resequence))
        cols[resequence] <- colTable
      else
        cols[resequence] <- colTable[resequence]
    }
    if (length(whichZeroLegend)) {
      cols[whichZeroLegend] <- zero.color
    }
    cols <- c(na.color, cols) # make first index of colours be transparent

    minzToMaxz <- seq(minz, maxz)
    lenMinzToMaxz <- length(minzToMaxz)
    if (length(cols) < lenMinzToMaxz)
      cols <- colorRampPalette(cols)(lenMinzToMaxz)

    # Convert numeric z to a matrix of hex colours
    z <- matrix(
      cols[round(z, digits = 0)], nrow = NROW(grobToPlot),
      ncol = ncol(grobToPlot), byrow = TRUE
    )

    list(
      z = z, minz = minzOrig, maxz = maxzOrig,
      cols = cols, real = real
    )
  }


#' Divergent colour palette
#'
#' Creates a palette for the current session for a divergent-colour graphic with
#' a non-symmetric range.
#' Based on ideas from Maureen Kennedy, Nick Povak, and Alina Cansler.
#'
#' @param start.color  Start colour to be passed to `colorRampPalette`.
#'
#' @param end.color    End colour to be passed to `colorRampPalette`.
#'
#' @param min.value    Numeric minimum value corresponding to `start.colour`.
#'                     If attempting to change the colour of a `RasterLayer`,
#'                     this can be set to `minFn(RasterObject)`.
#'
#' @param max.value    Numeric maximum value corresponding to `end.colour`.
#'                     If attempting to change the colour of a `RasterLayer`,
#'                     this can be set to `maxFn(RasterObject)`.
#' @param mid.value    Numeric middle value corresponding to `mid.colour`.
#'                     Default is `0`.
#'
#' @param mid.color    Middle colour to be passed to `colorRampPalette`.
#'                     Defaults to `"white"`.
#'
#' @return A diverging colour palette.
#'
#' @aliases divergentColours
#' @author Eliot McIntire and Alex Chubaty
#' @export
#' @importFrom  grDevices colorRampPalette
#' @seealso [colorRampPalette()]
#'
#' @examples
#' divergentColors("darkred", "darkblue", -10, 10, 0, "white")
#'
setGeneric("divergentColors",
           function(start.color, end.color, min.value, max.value, # nolint
                    mid.value = 0, mid.color = "white") { # nolint
             standardGeneric("divergentColors")
})

#' @rdname divergentColors
#' @aliases divergentColours
setMethod(
  "divergentColors",
  signature = c("character", "character", "numeric", "numeric"),
  definition = function(start.color, end.color, min.value, max.value, # nolint
                        mid.value, mid.color) { # nolint
  ramp1 <- colorRampPalette(c(start.color, mid.color))
  ramp2 <- colorRampPalette(c(mid.color, end.color))

  # now specify the number of values on either side of "mid.value"
  maxBreaks <- floor((max.value - mid.value) + 1) # nolint
  minBreaks <- floor((mid.value - min.value) + 1) # nolint

  lowRamp <- ramp1(minBreaks)
  highRamp <- ramp2(maxBreaks)
  if (minBreaks == 1) lowRamp <- mid.color

  # now create a combined ramp from the higher values of "lowRamp" and
  # the lower values of "highRamp", with the longer one using all values
  # highRamp starts at 2 to avoid duplicating zero

  myColors <- c(lowRamp[1:minBreaks], highRamp[2:maxBreaks])

  return(myColors)
})

#' @importFrom grDevices rgb
colorsRGBtoHex <- function(cols) {
  if ("alpha" %in% colnames(cols))
    rgb(cols[, "red"]/255, cols[, 'green']/255, cols[, "blue"]/255, cols[, "alpha"]/255)
  else
    rgb(cols[, "red"]/255, cols[, 'green']/255, cols[, "blue"]/255)
}

factorValues <- function(r, values, layer = terra::activeCat(r), indices) {
  df <- terra::levels(r)[[1]]
  if (missing(values)) {
    values <- r[][indices]
  }
  df[[layer + 1]][match(values, df[[1]])]

}
