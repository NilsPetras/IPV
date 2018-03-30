fbrv.input.manual <- function(structure){
  
  # fbrv.input generates an empty mask to fill with own data
  # structure is a dataframe with the columns "title", "factor", "subfactor" and "item"
  # containing the names of all items and their superordinate factors
  
  cplx <- length(levels(structure$factor))
  
    if(!is.null(structure$title)){
      
      # global level
      cds <- data.frame(factor=rep(levels(structure$title),cplx),
                        subfactor=levels(structure$factor),
                        center_distance=rep(NA,cplx))
      cors <- matrix(rep(NA,cplx^2))
      dim(cors) <- c(cplx,cplx)
      colnames(cors) <- levels(structure$factor)
      row.names(cors) <- levels(structure$factor)
      global <- list(center_distances=cds,subfactor_cors=cors,parameters=list(complexity=cplx))
      rm(cds,cors)
      
      # factor level
      myfactors <- split(structure,f = structure$factor)
      myfactors <- lapply(myfactors,FUN = fbrv.input.factor,mode=mode)
      mydata <- list(global=global,factors=myfactors)
    }
    
    else mydata <- fbrv.input.manual.factor(x=structure)
  
  return(mydata)
}