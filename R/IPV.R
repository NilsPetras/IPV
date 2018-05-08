#' IPV: A package to create Item Pool Visualizations
#'
#' The IPV package provides three categories of important functions: input,
#' coord, and plot.
#'
#' @section Input Functions:
#'
#'   The input functions format your data, so the coord functions can use them.
#'   \code{\link{input_excel}}
#'
#' @section Coord Functions:
#'
#'   The coord functions calculate specific coordinates the plot functions draw
#'   from. \code{\link{coord_items}} \code{\link{coord_facets}}
#'   \code{\link{coord_nested}}
#'
#' @section Plot Functions:
#'
#'   The plot functions create the actual charts. \code{\link{plot_items}}
#'   \code{\link{plot_facets}} \code{\link{plot_nested}}
#'
#' @section Workflow:
#'
#'   Creating an IPV from prepared data is a two step process, using a coord_
#'   and a plot_ function. Some customizations are made in the coord_ and some
#'   in the plot_ function. For example use \code{\link{coord_items}} in
#'   conjunction with \code{\link{plot_items}} to produce an item chart.
#'
#' @docType package
#' @name IPV
NULL
