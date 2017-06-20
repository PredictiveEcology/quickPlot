#' Classes defined in quickPlot
#'
#' These S4 classes are defined within \code{quickPlot}. "dot" classes are not exported and
#' are therefore intended for internal use only.
#' 
#' @section Plotting classes - used within \code{Plot}:
#'
#' \tabular{ll}{
#'   New classes\tab \cr
#'   \code{\link{.quickPlot}} \tab Main class for \code{Plot} - contains \code{.quickGrob} and \code{.arrangement} objects\cr
#'   \code{\link{.quickPlotGrob}} \tab GRaphical OBject used by quickPlot - smallest unit\cr
#'   \code{\link{.arrangement}} \tab The layout or "arrangement" of plot objects\cr
#'   --------------------------- \tab ------------------------------------------------------------------------------------------ \cr
#' }
#'
#' \tabular{ll}{
#'   Unions of existing classes\tab \cr
#'   \code{\link{.quickPlottables}} \tab The union of all object classes Plot can accept\cr
#'   \code{\link{.quickPlotObjects}} \tab The union of spatialObjects and several others\cr
#'   \code{\link{spatialObjects}} \tab The union of several spatial classes\cr
#'   --------------------------- \tab ------------------------------------------------------------------------------------------ \cr
#' }
#'
#' @seealso \code{\link{Plot}}
#' @name quickPlotClasses
#' @rdname quickPlot-classes
#' @author Eliot McIntire and Alex Chubaty
NULL
