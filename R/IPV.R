#' IPV: A package to create Item Pool Visualizations
#'
#' The IPV package provides the following functions.
#'
#' @section Estimation function:
#'
#'   \code{\link{ipv_est}} uses raw data to estimate the IPV models and
#'   pre-format their estimates for chart creation. This is the easiest and
#'   recommended Workflow.
#'
#' @section Chart Functions:
#'
#'   Chart functions create a ggplot2 object (the chart). There are three types
#'   of charts. \code{\link{item_chart}} \code{\link{facet_chart}}
#'   \code{\link{nested_chart}}
#'
#' @section Input Functions:
#'
#'   The input functions prepare existing model estimates for the chart
#'   functions. This is not recommended, if the raw data are available. Read in
#'   vectors containing model estimates from within R by using
#'   \code{\link{input_manual_simple}}, \code{\link{input_manual_nested}}) and
#'   \code{\link{input_manual_process}}. Read in model estimates via MS Excel
#'   files and \code{\link{input_excel}}.
#'
#' @section Miscellaneous functions:
#'
#'   The function \code{\link{item_overview}} creates a grid of bar plots
#'   showing the (squared) factor loadings of all items in all models underlying
#'   a nested chart. Use this to inspect the absolute values underlying the
#'   charts.
#'
#'   The function \code{\link{relabel}} enables quick changes of the labels for
#'   variables.
#'
#' @section Basic Workflow:
#'
#'   \enumerate{\item Prepare your raw data. \item Generate the model estimates
#'   using the estimation function. \item Select a chart function and use it
#'   with the estimates, a file name (.pdf), and otherwise default values. \item
#'   Change the default values of the chart function arguments. \item Check the
#'   chart's appearance by opening the created file (do not rely on the display
#'   of plots in R, results may differ). \item Repeat until you are satisfied
#'   with the result.}
#'
#' @docType package
#' @name IPV
NULL
