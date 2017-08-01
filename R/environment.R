#' The quickPlot environment
#'
#' Environment used internally to store internal package objects and methods.
#'
#' @keywords internal
#' @rdname quickPlotEnv
.quickPlotEnv <- new.env(parent = emptyenv())

#' Get objects from the internal \code{quickPlot} environment
#'
#' Internal function. Simple wrapper for \code{\link{get}}.
#'
#' @param x   an object name (given as a character string).
#'
#' @param ... Additional arguments to pass to \code{get}.
#'
#' @author Alex Chubaty
#' @keywords internal
#' @name .getQuickPlot
#' @rdname getQuickPlot
#'
setGeneric(".getQuickPlot", function(x, ...) {
  standardGeneric(".getQuickPlot")
})

#' @rdname getQuickPlot
setMethod(".getQuickPlot",
          signature(x = "ANY"),
          definition = function(x, ...) {
            get(x, envir = .quickPlotEnv, ...)
})

#' Assign to the internal \code{quickPlot} environment.
#'
#' Internal function. Simple wrapper for \code{\link{assign}}.
#'
#' @param x     a variable name, given as a character string.
#'              No coercion is done, and the first element of a character vector
#'              of length greater than one will be used, with a warning.
#'
#' @param value The object to assign. If this is missing, values will be found
#'              with \code{get(x)} in the same environment as the calling
#'              environment.
#'
#' @param ... Additional arguments to pass to \code{assign}.
#'
#' @return Only used for its side effect, namely the object assigned to the \code{.quickPlotEnv}
#'
#' @keywords internal
#' @rdname assignQuickPlot
#'
#' @author Alex Chubaty
setGeneric(".assignQuickPlot", function(x, value, ...) {
  standardGeneric(".assignQuickPlot")
})

#' @rdname assignQuickPlot
setMethod(".assignQuickPlot",
          signature(x = "character", value = "ANY"),
          definition = function(x, value, ...) {
            assign(x, value, envir = .quickPlotEnv, ...)
})

#' @rdname assignQuickPlot
setMethod(".assignQuickPlot",
          signature(x = "character", value = "missing"),
          definition = function(x, value, ...) {
            assign(x, get(x), envir = .quickPlotEnv, ...)
})

#' Is an object defined in the \code{.quickPlotEnv} environment?
#'
#' Internal function. Simple wrapper for \code{\link{exists}}.
#'
#' @param x   An object name, given as a character string.
#'            No coercion is done, and the first element of a character vector
#'            of length greater than one will be used, with a warning.
#'
#' @param ... Additional arguments passed to \code{\link{exists}}
#'
#' @author Alex Chubaty
#' @keywords internal
#' @rdname existsQuickPlot
#'
setGeneric(".existsQuickPlot", function(x, ...) {
  standardGeneric(".existsQuickPlot")
})

#' @rdname existsQuickPlot
setMethod(".existsQuickPlot",
          signature(x = "ANY"),
          definition = function(x, ...) {
            exists(x, envir = .quickPlotEnv, ...)
})
