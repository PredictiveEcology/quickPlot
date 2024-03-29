% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotting-helpers.R
\name{.objectNames}
\alias{.objectNames}
\title{Extracts the object names}
\usage{
.objectNames(calledFrom = "Plot", argClass = ".quickPlotObjects", argName = "")
}
\arguments{
\item{calledFrom}{character vector of length 1, indicating which function
call is desired. Defaults to \code{Plot}.}

\item{argClass}{character vector of length 1, indicating which class is
being searched for among the arguments.
Defaults to \code{.quickPlotObjects}.}

\item{argName}{character vector of length 1, or \code{NULL}, indicating
if the arguments to select have a name, no name (empty
string), or do not use name (\code{NULL}).}
}
\value{
\code{NULL}. This function is invoked for its side effects.
}
\description{
Internal function primarily used from \code{Plot}.
}
\author{
Eliot McIntire
}
\keyword{internal}
