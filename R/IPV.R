#' IPV: A package to create Item Pool Visualizations
#'
#' The IPV package provides two sets of functions: input functions, and chart
#' functions.
#'
#' @section Input Functions:
#'
#'   The input functions prepare your data for the chart functions. Input data manually using
#'   loose input within R \code{\link{input_manual_simple}} (and possibly
#'   \code{\link{input_manual_nested}}) combined with
#'   \code{\link{input_manual_process}}. Or input data via MS Excel files and
#'   \code{\link{input_excel}} as demonstrated in the examples.
#'
#' @section Chart Functions:
#'
#'   Chart functions create a ggplot2 object (the chart) and save it as a file.
#'   There are three types of charts. All functions have several parameters for
#'   customization. Yet, there are sensible default values for everything but the
#'   data. \code{\link{item_chart}} \code{\link{facet_chart}}
#'   \code{\link{nested_chart}}
#'
#' @section Workflow:
#'
#'   Prepare your data using the input functions. Choose the chart type. Use the
#'   chart function with your data and defaults. Then \enumerate{\item change
#'   the parameter values of the chart function \item check the chart appearance by
#'   opening the created file \item repeat until you are satisfied with the result}
#'
#' @docType package
#' @name IPV
NULL
