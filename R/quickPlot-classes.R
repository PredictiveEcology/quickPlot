#' `quickPlot` classes
#'
#' `quickPlot` uses S4 classes.
#' "Dot" classes are not exported and are therefore intended for internal use only.
#'
#' @section Plotting classes - used within `Plot`:
#'
#' \tabular{ll}{
#'   **New classes**\tab \cr
#'   [.arrangement()] \tab The layout or "arrangement" of plot objects\cr
#'   [.quickPlot()] \tab Main class for `Plot` - contains `.quickGrob`
#'                             and `.arrangement` objects\cr
#'   [.quickPlotGrob()] \tab GRaphical OBject used by `quickPlot` - smallest unit\cr
#' }
#'
#' \tabular{ll}{
#'   **Unions of existing classes:**\tab \cr
#'   [.quickPlottables()] \tab The union of all object classes Plot can accept\cr
#'   [.quickPlotObjects()] \tab The union of `spatialObjects` and several others\cr
#'   [spatialObjects()] \tab The union of several spatial classes\cr
#' }
#'
#' @seealso [Plot()]
#' @name quickPlotClasses
#' @rdname quickPlot-classes
#' @author Eliot McIntire and Alex Chubaty
NULL
