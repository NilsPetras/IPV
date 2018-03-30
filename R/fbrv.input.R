fbrv.input <- function(global=NULL,factors){
  
  # reads excel files and calculates center distances from factor loadings
  # creates input for other "fbrv." functions
  
  if(is.null(global)){
    mydata <- fbrv.input.factor(factors)
  }else{
    global_input <- fbrv.input.factor(global)
    global_input$center_distances$item <- paste(global_input$center_distances$subfactor,global_input$center_distances$item,sep = ".")
    factors_input <- lapply(factors,fbrv.input.factor)
    for(i in 1:global_input$parameters$complexity)names(factors_input)[i] <- levels(factors_input[[c(i,1)]]$factor)
    mydata <- list(global=global_input,factors=factors_input)
  }
  
  return(mydata)
}