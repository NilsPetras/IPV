#' Input Excel
#'
#' This function reads excel files containing factor loadings and latent correlations for IPV plots.
#'
#' @param global character; name of the excel file containing data on the overall general factor and the factors; see examples.
#' @param factors character; name(s) of the excel file(s) containing data on the factor(s) and their/its facets; see examples.
#'
#' @details Note that the excel files need a very specific structure, see examples.
#' The \code{global} argument defaults to NULL.
#' This allows to read data on a single factor and its facets to the \code{factors} argument.
#' The result is a more simple model that can be displayed by an item plot and a facet plot.
#'
#' @return list containing formatted data including center distances for use with \code{\link{model_items}}, \code{\link{model_facets}}, and \code{\link{model_nested}}.
#' @export
input_excel <- function(global=NULL,factors){
  
  # the helper function 'input_factor_excel' does most of the dirty work and applies to both the global and the nested scale
  if(is.null(global)){
    mydata <- input_factor_excel(factors)
  }else{
    global_input <- input_factor_excel(global)
    # including the factor name in the item name to distinguish between items from different tests but with the same name
    global_input$center_distances$item <- paste(global_input$center_distances$subfactor,global_input$center_distances$item,sep = ".")
    factors_input <- lapply(factors,input_factor_excel)
    # naming the lists of the factors, which are list elements of the overall list
    for(i in 1:global_input$parameters$complexity)names(factors_input)[i] <- levels(factors_input[[c(i,1)]]$factor)
    mydata <- list(global=global_input,factors=factors_input)
  }
  
  return(mydata)
}