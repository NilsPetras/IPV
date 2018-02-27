fbrv.calc <- function(data,subrad,rotate=0,subrotate=0,items=FALSE,correlations=TRUE,correlation_spacing=.3,relative_scaling=1.5,extra_arrows=NULL){
  
  # generates the full plotting data using the full input
  # full input consists of all factors as a list and the overarching model where the factors are treated as subfactors of the global factor
  # rotate rotates the global plot whilst not changing the subplots
  # subrotate rotates the subplots (including the item plots), whilst not changing the global plot
  # subrotate can be a fixed value for all subplots or a vector with one value per subplot
  
    ## listwise calculation for single factors
  
    if(length(subrotate==1)){
      factorcoors <- lapply(X = data$factors,FUN = fbrv.model,subrad=subrad,rotate=subrotate)
      if(items==TRUE){
        itemcoors <- lapply(data$factors,fbrv.model.items,rotate=subrotate)
      }
    }
  
  # as loop passing varying parameters to subcircles
  if(length(subrotate)==length(data$factors)){
    factorcoors <- list()
    for(i in 1:length(data$factors)){
      factorcoors[[i]] <- fbrv.model(data$factors[[i]],subrad=subrad,rotate=subrotate[i])
    }
    names(factorcoors) <- names(data$factors)
    if(items==TRUE){
      itemcoors <- list()
      for(i in 1:length(data$factors)){
        itemcoors[[i]] <- fbrv.model.items(data$factors[[i]],rotate=subrotate[i])
      }
    }
  }
  
  
    ## global coors for nested plot
  # some useful variables
  nam <- as.character(data$global$center_distances$subfactor)
  cplx <- data$global$parameters$complexity
  
  # retrieving the size of the subcircles
  getcircsize <- function(x){
    polcircs <- get(x = "pol_circles",envir = as.environment(x))
    polcircs <- polcircs[1,"radius"]
  }
  circsize <- unlist(lapply(factorcoors,getcircsize))
  circsize <- circsize+correlations*correlation_spacing
  
  # pol coors of circles
  polcoor <- as.matrix(rep(0,3*(1+cplx)))
  dim(polcoor) <- c(3,1+cplx)
  polcoor <- data.frame(t(polcoor),row.names = c(levels(data$global$center_distances$factor),nam))
  names(polcoor) <- c("phi","rho","radius")
  polcoor[names(circsize),"radius"] <- circsize
  polcoor$radius[1] <- max(data$global$center_distances[nam==names(circsize),"center_distance"]*relative_scaling+circsize*2)
  polcoor[names(circsize),"rho"] <- c(data$global$center_distances[nam==names(circsize),"center_distance"]*relative_scaling+circsize)
  polcoor$phi <- c(0,2*pi/cplx*c(1:cplx))+rotate
  polcoor$rho[-1] <- polcoor$rho[-1]
  globalcoors <- list(pol_circles = polcoor)
  rm(polcoor)
  
  # cart coors of circles
    # x=cos(phi)*rho; y=sin(phi)*rho
  globalcoors$cart_circles <- globalcoors$pol_circles
  globalcoors$cart_circles[,1] <- cos(globalcoors$pol_circles$phi) * globalcoors$pol_circles$rho
  globalcoors$cart_circles[,2] <- sin(globalcoors$pol_circles$phi) * globalcoors$pol_circles$rho
  names(globalcoors$cart_circles) <- c("x","y","radius")
  row.names(globalcoors$cart_circles)[1] <- ""
  
  # pol coors of inner ring for correlation spacing
  if(correlations==T){
    polcoor <- as.matrix(rep(0,3*(1+cplx)))
    dim(polcoor) <- c(3,1+cplx)
    polcoor <- data.frame(t(polcoor),row.names = c(levels(data$global$center_distances$factor),nam))
    names(polcoor) <- c("phi","rho","radius")
    polcoor[names(circsize),"radius"] <- circsize-correlations*correlation_spacing
    polcoor[names(circsize),"rho"] <- c(data$global$center_distances[nam==names(circsize),"center_distance"]*relative_scaling+circsize)
    polcoor$phi <- c(0,2*pi/cplx*c(1:cplx))+rotate
    polcoor$rho[-1] <- polcoor$rho[-1]
    polcoor <- polcoor[-1,]
    globalcoors$pol_inner_ring <- polcoor
    rm(polcoor)
  }

  # cart coors of inner ring for correlation spacing
  if(correlations==T){
    globalcoors$cart_inner_ring <- globalcoors$pol_inner_ring
    globalcoors$cart_inner_ring[,1] <- cos(globalcoors$pol_inner_ring$phi) * globalcoors$pol_inner_ring$rho
    globalcoors$cart_inner_ring[,2] <- sin(globalcoors$pol_inner_ring$phi) * globalcoors$pol_inner_ring$rho
    names(globalcoors$cart_inner_ring) <- c("x","y","radius")
  }
  
  # pol coor of axes
  globalcoors$pol_axes <- as.matrix(rep(0,5*(cplx)))
  dim(globalcoors$pol_axes) <- c(5,cplx)
  globalcoors$pol_axes <- data.frame(t(globalcoors$pol_axes),row.names = nam)
  names(globalcoors$pol_axes) <- c("rho0","rho1","rho2","rho3","phi") # center, inner and outer intersection of axes and circles, intersection of axes and outer circle
  globalcoors$pol_axes$phi <- tail(globalcoors$pol_circles$phi,cplx)
  globalcoors$pol_axes$rho1 <- tail(globalcoors$pol_circles$rho,cplx)-tail(globalcoors$pol_circles$radius,cplx)
  globalcoors$pol_axes$rho2 <- globalcoors$pol_axes$rho1 + 2 * tail(globalcoors$pol_circles$radius,cplx)
  globalcoors$pol_axes$rho3 <- rep(globalcoors$pol_circles$radius[1])
  
  # cart coor of axes
  globalcoors$cart_axes <- as.matrix(rep(0,8*cplx))
  dim(globalcoors$cart_axes) <- c(8,cplx)
  globalcoors$cart_axes <- data.frame(t(globalcoors$cart_axes),row.names = nam)
  names(globalcoors$cart_axes) <- c("x0","y0","x1","y1","x2","y2","x3","y3")
  globalcoors$cart_axes$x0 <- cos(globalcoors$pol_axes$phi) * globalcoors$pol_axes$rho0
  globalcoors$cart_axes$x1 <- cos(globalcoors$pol_axes$phi) * globalcoors$pol_axes$rho1
  globalcoors$cart_axes$x2 <- cos(globalcoors$pol_axes$phi) * globalcoors$pol_axes$rho2
  globalcoors$cart_axes$x3 <- cos(globalcoors$pol_axes$phi) * globalcoors$pol_axes$rho3
  globalcoors$cart_axes$y0 <- sin(globalcoors$pol_axes$phi) * globalcoors$pol_axes$rho0
  globalcoors$cart_axes$y1 <- sin(globalcoors$pol_axes$phi) * globalcoors$pol_axes$rho1
  globalcoors$cart_axes$y2 <- sin(globalcoors$pol_axes$phi) * globalcoors$pol_axes$rho2
  globalcoors$cart_axes$y3 <- sin(globalcoors$pol_axes$phi) * globalcoors$pol_axes$rho3

  # coor of axis tick label

  # coor of factor name (currently counter-clockwise to smalles circle)
  globalcoors$factor_label <- data.frame(x = NA,y = NA,label = row.names(globalcoors$pol_circles)[1],phi=NA,rho=NA)
  globalcoors$factor_label$phi <- globalcoors$pol_circles[min(globalcoors$pol_circles$radius),"phi"]-pi/cplx
  globalcoors$factor_label$rho <- 2/3*max(globalcoors$pol_circles$radius)
  globalcoors$factor_label$x <- cos(globalcoors$factor_label$phi)*globalcoors$factor_label$rho
  globalcoors$factor_label$y <- sin(globalcoors$factor_label$phi)*globalcoors$factor_label$rho
  if(cplx==2){
    globalcoors$factor_label$x <- globalcoors$factor_label$x*2
    globalcoors$factor_label$y <- globalcoors$factor_label$y*2
  }  
  
  # coor of inner cors as text
  n <- cplx*(cplx-1)
  globalcoors$inner_cors <- data.frame(x=rep(NA,n),y=rep(NA,n),V1=rep(NA,n),V2=rep(NA,n),label=rep(NA,n),xnew=rep(NA,n),ynew=rep(NA,n))
  # subfactor list 1
  a <- row.names(data$global$subfactor_cors)
  a <- c(a,a[1])
  b <- NULL
  # subfactor list 2
  # matching subfactors from list 1 to all other subfactors in the correct order (workaround, do not know how to create a ring in R)
  for(k in 1:cplx) {
    b <- c(b,a[-c(1,cplx+1)])
    a <- a[-1]
    a <- c(a,a[1])
  }
  globalcoors$inner_cors$V1 <- b
  globalcoors$inner_cors$V2 <- unlist(lapply(row.names(data$global$subfactor_cors),FUN=rep,times=cplx-1))
  for(k in 1:n) globalcoors$inner_cors$label[k] <- data$global$subfactor_cors[globalcoors$inner_cors$V1[k],globalcoors$inner_cors$V2[k]]
  globalcoors$inner_cors$label <- as.character(globalcoors$inner_cors$label)
  globalcoors$inner_cors$label[globalcoors$inner_cors$label<1] <- substr(globalcoors$inner_cors$label,2,4)
  globalcoors$inner_cors$x <- globalcoors$cart_circles[globalcoors$inner_cors$V2,"x"]
  globalcoors$inner_cors$y <- globalcoors$cart_circles[globalcoors$inner_cors$V2,"y"]
  # scatter as list and anchor towards the center
  # scatter width resembles the angles of an even n-sided polygon for n subfactors (e.g. 90° = pi/2 for 4 subfactors)
  scatter <- rep(seq(from = (-pi+2*pi/cplx)/2,to = (pi-2*pi/cplx)/2,by = (pi-2*pi/cplx)/(cplx-2)),cplx)
  globalcoors$inner_cors$xnew <- globalcoors$inner_cors$x + cos(globalcoors$pol_circles[globalcoors$inner_cors$V2,"phi"]+pi+scatter)*(globalcoors$pol_circles[globalcoors$inner_cors$V2,"radius"]-correlations*.5*correlation_spacing)
  globalcoors$inner_cors$ynew <- globalcoors$inner_cors$y + sin(globalcoors$pol_circles[globalcoors$inner_cors$V2,"phi"]+pi+scatter)*(globalcoors$pol_circles[globalcoors$inner_cors$V2,"radius"]-correlations*.5*correlation_spacing)
  globalcoors$inner_cors$x <- globalcoors$inner_cors$xnew
  globalcoors$inner_cors$y <- globalcoors$inner_cors$ynew
  globalcoors$inner_cors[6:7] <- list(NULL)
  if(cplx>2)globalcoors$inner_cors$isneighbour <- rep(c(TRUE,rep(FALSE,times = max(0,cplx-3)),TRUE),times = cplx)
  else globalcoors$inner_cors$isneighbour <- TRUE
  
  
    ## subcircles in nested plot
  # shifted coors of subcircles
  for(i in 1:cplx) globalcoors$subcircles[[nam[i]]] <- fbrv.shift(factorcoors[[nam[i]]],globalcoors$cart_circles[nam[i],"x"],globalcoors$cart_circles[nam[i],"y"])
  
  # coor of circles
  for(i in 1:cplx) globalcoors$nested$circles[[nam[i]]] <- globalcoors$subcircles[[c(i,1)]]
  globalcoors$nested$circles <- lapply(globalcoors$nested$circles,tail,n=-1)
  globalcoors$nested$circles <- do.call("rbind",globalcoors$nested$circles)
  globalcoors$nested$circles$labels <- substr(row.names(globalcoors$nested$circles),unlist(gregexpr(pattern = "\\.",row.names(globalcoors$nested$circles)))+1,nchar(row.names(globalcoors$nested$circles)))
  
  # coor of axes
  for(i in 1:cplx) globalcoors$nested$axes[[nam[i]]] <- globalcoors$subcircles[[c(i,2)]]
  globalcoors$nested$axes <- do.call("rbind",globalcoors$nested$axes)
  
  # coor of factor name
  for(i in 1:cplx) globalcoors$nested$factor_label[[nam[i]]] <- globalcoors$subcircles[[c(i,7)]]
  globalcoors$nested$factor_label <- do.call("rbind",globalcoors$nested$factor_label)
  
  # coor of inner correlations
  for(i in 1:cplx) globalcoors$nested$inner_cors[[nam[i]]] <- globalcoors$subcircles[[c(i,8)]]
  globalcoors$nested$inner_cors <- do.call("rbind",globalcoors$nested$inner_cors)
  
  # coor of correlation arrows and labels
  for(i in 1:cplx) globalcoors$nested$subfactor_cor_arrows[[nam[i]]] <- globalcoors$subcircles[[c(i,5)]]
  globalcoors$nested$subfactor_cor_arrows <- do.call("rbind",globalcoors$nested$subfactor_cor_arrows)
  for(i in 1:cplx) globalcoors$nested$subfactor_cor_labels[[nam[i]]] <- globalcoors$subcircles[[c(i,6)]]
  globalcoors$nested$subfactor_cor_labels <- do.call("rbind",globalcoors$nested$subfactor_cor_labels)
  
    ## custom correlation arrows in nested plot
  if(!is.null(extra_arrows)){
    n <- dim(extra_arrows)[1]
    globalcoors$extra_arrows <- data.frame(x1=rep(NA,n),x2=rep(NA,n),y1=rep(NA,n),y2=rep(NA,n),label=rep(NA,n),xlabel=rep(NA,n),ylabel=rep(NA,n))
    globalcoors$extra_arrows$label <- extra_arrows$value
    globalcoors$extra_arrows$x1 <- globalcoors$nested$circles[paste(extra_arrows$V1_factor,extra_arrows$V1_subfactor,sep = "."),"x"]
    globalcoors$extra_arrows$y1 <- globalcoors$nested$circles[paste(extra_arrows$V1_factor,extra_arrows$V1_subfactor,sep = "."),"y"]
    globalcoors$extra_arrows$x2 <- globalcoors$nested$circles[paste(extra_arrows$V2_factor,extra_arrows$V2_subfactor,sep = "."),"x"]
    globalcoors$extra_arrows$y2 <- globalcoors$nested$circles[paste(extra_arrows$V2_factor,extra_arrows$V2_subfactor,sep = "."),"y"]
    globalcoors$extra_arrows$x1new <- globalcoors$extra_arrows$x1+subrad/sqrt((globalcoors$extra_arrows$x2-globalcoors$extra_arrows$x1)^2+(globalcoors$extra_arrows$y2-globalcoors$extra_arrows$y1)^2)*(globalcoors$extra_arrows$x2-globalcoors$extra_arrows$x1)
    globalcoors$extra_arrows$x2new <- globalcoors$extra_arrows$x2+subrad/sqrt((globalcoors$extra_arrows$x2-globalcoors$extra_arrows$x1)^2+(globalcoors$extra_arrows$y2-globalcoors$extra_arrows$y1)^2)*(globalcoors$extra_arrows$x1-globalcoors$extra_arrows$x2)
    globalcoors$extra_arrows$y1new <- globalcoors$extra_arrows$y1+subrad/sqrt((globalcoors$extra_arrows$x2-globalcoors$extra_arrows$x1)^2+(globalcoors$extra_arrows$y2-globalcoors$extra_arrows$y1)^2)*(globalcoors$extra_arrows$y2-globalcoors$extra_arrows$y1)
    globalcoors$extra_arrows$y2new <- globalcoors$extra_arrows$y2+subrad/sqrt((globalcoors$extra_arrows$x2-globalcoors$extra_arrows$x1)^2+(globalcoors$extra_arrows$y2-globalcoors$extra_arrows$y1)^2)*(globalcoors$extra_arrows$y1-globalcoors$extra_arrows$y2)
    globalcoors$extra_arrows$x1 <- globalcoors$extra_arrows$x1new
    globalcoors$extra_arrows$x2 <- globalcoors$extra_arrows$x2new
    globalcoors$extra_arrows$y1 <- globalcoors$extra_arrows$y1new
    globalcoors$extra_arrows$y2 <- globalcoors$extra_arrows$y2new
    globalcoors$extra_arrows$xlabel <- (globalcoors$extra_arrows$x1+globalcoors$extra_arrows$x2)/2
    globalcoors$extra_arrows$ylabel <- (globalcoors$extra_arrows$y1+globalcoors$extra_arrows$y2)/2
    # letting the correlation labels dodge their arrow
    globalcoors$extra_arrows$xlabel <- globalcoors$extra_arrows$xlabel+.1/sqrt((globalcoors$extra_arrows$x2-globalcoors$extra_arrows$x1)^2+(globalcoors$extra_arrows$y2-globalcoors$extra_arrows$y1)^2)*(globalcoors$extra_arrows$y2-globalcoors$extra_arrows$y1)
    globalcoors$extra_arrows$ylabel <- globalcoors$extra_arrows$ylabel+.1/sqrt((globalcoors$extra_arrows$x2-globalcoors$extra_arrows$x1)^2+(globalcoors$extra_arrows$y2-globalcoors$extra_arrows$y1)^2)*(globalcoors$extra_arrows$x1-globalcoors$extra_arrows$x2)
    globalcoors$extra_arrows[8:11] <- list(NULL)
    rm(n)
    }
  
  rm(nam,cplx)
  
  globalcoors$relative_scaling <- relative_scaling
  globalcoors$correlation_spacing <- correlation_spacing
    
  mycoor <- list(factor=factorcoors,global=globalcoors)
  if(items==T)mycoor$items <- itemcoors
  
  return(mycoor)
}