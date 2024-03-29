% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotting-classes.R
\docType{class}
\name{.arrangement-class}
\alias{.arrangement-class}
\alias{.arrangement}
\title{The \code{.arrangement} class}
\description{
This class contains the plotting arrangement information.
}
\details{
These \verb{gp*} parameters will specify plot parameters that are
available with \code{gpar()}. \code{gp} will adjust plot parameters,
\code{gpText} will adjust title and legend text, \code{gpAxis} will
adjust the axes. \code{size} adjusts point size in a \code{SpatialPoints} object.
These will persist with the original \code{Plot} call for each individual object.
Multiple entries can be used, but they must be named list elements
and they must match the \code{...} items to plot.
This is true for a \code{RasterStack} also, i.e., the list of named elements
must be the same length as the number of layers being plotted.
The naming convention used is: \code{RasterStackName$layerName}, i.e, \code{landscape$DEM}.
}
\section{Slots}{

\describe{
\item{\code{rows}}{numeric. Number of rows in the arrangement.}

\item{\code{columns}}{numeric. Number of columns in the arrangement.}

\item{\code{actual.ratio}}{numeric. Ratio of columns to rows}

\item{\code{ds.dimensionRatio}}{numeric. Ratio of the device size to the ratio of the extents.}

\item{\code{ds}}{numeric of length 2. The dimensions of the plotting window in inches.}

\item{\code{objects}}{list of length number of spatial objects. Each list has a character vector
of the layer names in each of those.}

\item{\code{isRaster}}{logical vector, indicating whether each object is a \verb{Raster*} object.}

\item{\code{names}}{character vector, indicating the names of the layers in the plot.}

\item{\code{extents}}{list of class \code{Extent} objects. These are needed to calculate the
\code{ds.dimensionRatio}, which is used to scale the \verb{Spatial*} objects correctly.}

\item{\code{isSpatialObjects}}{logical indicating whether the object(s) are \code{spatialObjects} or not.}

\item{\code{layout}}{list of length 2, with width and height measurements for layout.}

\item{\code{gp}}{a \code{gpar} object or list of named \code{gpar} objects. These names must
match the names of the \code{...} objects. Default is \code{NULL.} See details.}

\item{\code{gpText}}{a \code{gpar} object or a list of named \code{gpar} objects. These names must
match the names of the \code{...} objects. Default is \code{NULL.} See details.}

\item{\code{gpAxis}}{a \code{gpar} object or a list of named \code{gpar} objects. These names must
match the names of the \code{...} objects. Default is \code{NULL.} See details.}

\item{\code{size}}{a numeric or a named list of numerics, used for \code{SpatialPoints} plots.
Default is 5. See details.}
}}

\seealso{
\code{\link[=quickPlotClasses]{quickPlotClasses()}}
}
\author{
Eliot McIntire
}
\keyword{internal}
