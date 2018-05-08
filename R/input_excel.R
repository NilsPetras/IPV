#' Input Excel
#'
#' Reads excel files containing factor loadings and latent correlations for IPV
#' charts.
#'
#' @param global character; name of the excel file containing factor loadings
#'   from the global level and the test level, and latent correlations from the
#'   test level.
#' @param factors character; name(s) of the excel file(s) containing factor
#'   loadings from the test level and the facet level, and latent correlations
#'   from the facet level.
#'
#' @details Note that the excel files need a very specific structure. Use the
#'   example files as templates.
#'
#'   The \code{global} argument defaults to NULL. This allows to only use the
#'   \code{factors} argument, resulting in a simple model with one test and its
#'   facets.
#'
#' @return List containing formatted data including center distances for
#'   \code{\link{coord_items}}, \code{\link{coord_facets}}, and
#'   \code{\link{coord_nested}}.
#'
#' @examples
#' # read data for a simple model by ignoring the "global" parameter of input_excel
#' single_file <- system.file("extdata", "DSSEI.xlsx", package = "IPV", mustWork = TRUE)
#' x <- input_excel(factors = single_file)
#'
#' # read data for a nested model
#' # note that the data needs to be split into several excel files as in the example
#' global <- system.file("extdata", "IPV_global.xlsx", package = "IPV", mustWork = TRUE)
#' factors <- c(system.file("extdata", "IPV_DSSEI.xlsx", package = "IPV", mustWork = TRUE),
#'              system.file("extdata", "IPV_SMTQ.xlsx", package = "IPV", mustWork = TRUE),
#'              system.file("extdata", "IPV_RSES.xlsx", package = "IPV", mustWork = TRUE))
#' x <- input_excel(global = global,factors = factors)
#'
#' @export
input_excel <- function(global=NULL,factors){

  # the helper function 'input_excel_factor' does most of the dirty work and
  # applies to both the global and the nested scale
  if(is.null(global)){
    mydata <- input_excel_factor(factors)
  }else{
    global_input <- input_excel_factor(global)
    # including the factor name in the item name to distinguish between items
    # from different tests but with the same name
    global_input$center_distances$item <- paste(global_input$center_distances$subfactor,
                                                global_input$center_distances$item,
                                                sep = ".")
    factors_input <- lapply(factors,input_excel_factor)
    # naming the lists of the factors, which are list elements of the overall list
    for(i in 1:global_input$parameters$complexity)names(factors_input)[i] <- levels(factors_input[[c(i,1)]]$factor)
    mydata <- list(global=global_input,factors=factors_input)
  }

  return(mydata)
}
