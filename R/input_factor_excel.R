input_factor_excel <- function(file){
  
  # reads data for a single factor from an excel file
  # creates list element for fbrv.input
  
  
  # reading in excel sheets as tibbles
  sheet1 <- read_excel(file,sheet = 1,col_names = T)
  sheet2 <- read_excel(file,sheet = 2,col_names = T)
  
  # checking for low factor loadings
  bad <- min(c(sheet1$factor_loading,sheet1$subfactor_loading))
  bad <- bad<0.1
  if(bad)warning("At least one factor loading set to minimum of 0.1")
  sheet1$factor_loading[sheet1$factor_loading<.1] <- .1
  sheet1$subfactor_loading[sheet1$subfactor_loading<.1] <- .1
  
  # calculating the center distances and putting them into a data frame
  center_distances <- data.frame(factor=sheet1$factor,subfactor=sheet1$subfactor,item=as.factor(sheet1$item),
                                 center_distance=sheet1$subfactor_loading^2/sheet1$factor_loading^2-1,
                                 mean_center_distance=NA)
  
  # checking for negative center distances
  bad <- min(center_distances$center_distance)
  bad <- bad<0
  if(bad)warning("At least one negative center distance adjusted to 0")
  center_distances$center_distance[center_distances$center_distance<0] <- 0
  
  # calculating mean center distances
  mean_center_distances <- lapply(split(center_distances,center_distances$subfactor),function(x)x$mean_center_distance <- mean(x$center_distance))
  center_distances$mean_center_distance <- as.numeric(mean_center_distances[center_distances$subfactor])
  
  # writing down the number of subfactors
  parameters <- list(complexity=length(levels(center_distances$subfactor)))
  
  # creating subfactor correlation matrix
  subfactor_cors <- apply(as.matrix(sheet2)[,-1],MARGIN = c(1,2),as.numeric)
  row.names(subfactor_cors) <- colnames(subfactor_cors)

  # checkout
  mydata <- list(center_distances=center_distances,
                 subfactor_cors=subfactor_cors,
                 parameters=parameters)
  
  return(mydata)
}