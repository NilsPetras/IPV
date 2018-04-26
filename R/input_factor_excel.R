#' Input Factor Excel
#'
#' This function reads factor loadings and latent correlations for factor and its facets from a single excel file.
#'
#' @param file character; filename of the excel file
#'
#' @details Do not use this function, always use \code{\link{input_excel}} instead.
#'
#' @return list containing formatted data including center distances for use with \code{\link{model_items}}, \code{\link{model_facets}}
#' @seealso \code{\link{input_excel}}
input_factor_excel <- function(file){

  # reading in excel sheets 1 and 2 as tibbles
  sheet1 <- readxl::read_excel(file,sheet = 1,col_names = T)
  sheet2 <- readxl::read_excel(file,sheet = 2,col_names = T)

  # checking for factor loadings below 0.1, in such case printing a warning message and setting these loadings to 0.1
  bad <- min(c(sheet1$factor_loading,sheet1$subfactor_loading))
  bad <- bad<0.1
  if(bad)warning("At least one factor loading set to minimum of 0.1")
  sheet1$factor_loading[sheet1$factor_loading<.1] <- .1
  sheet1$subfactor_loading[sheet1$subfactor_loading<.1] <- .1

  # calculating the center distances
  center_distances <- data.frame(factor=sheet1$factor,subfactor=sheet1$subfactor,item=as.factor(sheet1$item),
                                 center_distance=sheet1$subfactor_loading^2/sheet1$factor_loading^2-1,
                                 mean_center_distance=NA)

  # checking for negative center distances, in such case printing a warning message and setting these center distances to 0
  bad <- min(center_distances$center_distance)
  bad <- bad<0
  if(bad)warning("At least one negative center distance adjusted to 0")
  center_distances$center_distance[center_distances$center_distance<0] <- 0

  # calculating mean center distances of facets
  mean_center_distances <- lapply(split(center_distances,center_distances$subfactor),function(x)x$mean_center_distance <- mean(x$center_distance))
  center_distances$mean_center_distance <- as.numeric(mean_center_distances[center_distances$subfactor])

  # number of facets
  parameters <- list(complexity=length(levels(center_distances$subfactor)))

  # subfactor correlation matrix
  subfactor_cors <- apply(as.matrix(sheet2)[,-1],MARGIN = c(1,2),as.numeric)
  row.names(subfactor_cors) <- colnames(subfactor_cors)

  # return
  mydata <- list(center_distances=center_distances,
                 subfactor_cors=subfactor_cors,
                 parameters=parameters)

  return(mydata)
}
