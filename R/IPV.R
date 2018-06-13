#' IPV: A package to create Item Pool Visualizations
#'
#' The IPV package provides four categories of important functions: input,
#' chart, coord, and plot.
#'
#' @section Input Functions:
#'
#'   The input functions help preprocessing your data, so the other functions
#'   can use them. \code{\link{input_excel}}
#'
#' @section Chart Functions:
#'
#'   Chart functions create a ggplot2 object (the chart) and optionally save
#'   this object to a file. There are three types of charts.
#'   \code{\link{item_chart}} \code{\link{facet_chart}}
#'   \code{\link{nested_chart}}
#'
#' @section Workflow:
#'
#'   Prepare your data using input functions. Choose a chart type and use the
#'   proper chart function. Then \enumerate{\item choose parameter values of the
#'   chart function \item check chart appearance \item repeat until satisfied}
#'
#' @docType package
#' @name IPV
NULL
