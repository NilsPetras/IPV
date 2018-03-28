fbrv.input.manual.factor <- function(x){
    
    x <- droplevels(x)
    cplx <- as.numeric(length(levels(x$subfactor)))
    l <- dim(x)[1]
  
    cds <- data.frame(factor=x$factor,
                      subfactor=x$subfactor,
                      item=x$item,
                      center_distance=rep(NA,l),
                      mean_center_distance=rep(NA,l))
    cors <- matrix(rep(NA,cplx^2))
    dim(cors) <- c(cplx,cplx)
    colnames(cors) <- levels(x$subfactor)
    row.names(cors) <- levels(x$subfactor)
    
    mydata <- list(center_distances=cds,subfactor_cors=cors,parameters=list(complexity=cplx))
    
    rm(cds,cors)
  
  return(mydata)
}