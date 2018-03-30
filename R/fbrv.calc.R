fbrv.calc <- function(data,subradius,
                      rotate=0,subrotate=0,rotate_title=0,
                      items=FALSE,cors=TRUE,cor_spacing=.3,relative_scaling=1.5,extra_arrows=NULL){
  
  # generates the full plotting data using the full input
  # full input consists of all factors as a list and the overarching model where the factors are treated as subfactors of the global factor
  # rotate rotates the global plot whilst not changing the subplots
  # subrotate rotates the subplots (including the item plots), whilst not changing the global plot
  # subrotate can be a fixed value for all subplots or a vector with one value per subplot
  
    ## listwise calculation for single factors
  
    if(length(subrotate)==1){
      factorcoors <- lapply(X = data$factors,FUN = fbrv.model,subradius=subradius,rotate=subrotate)
      names(factorcoors) <- names(data$factors)
    }
    if(length(subrotate)==1 & items==TRUE){
      itemcoors <- lapply(data$factors,fbrv.model.items,rotate=subrotate)
      names(itemcoors) <- names(data$factors)
    }
  # as loop passing varying parameters to subcircles
  if(length(subrotate)==length(data$factors)){
    factorcoors <- list()
    for(i in 1:length(data$factors)){
      factorcoors[[i]] <- fbrv.model(data$factors[[i]],subradius=subradius,rotate=subrotate[i])
    }
    names(factorcoors) <- names(data$factors)
    
  }
  if(length(subrotate)==length(data$factors) & items==TRUE){
    itemcoors <- list()
    for(i in 1:length(data$factors)){
      itemcoors[[i]] <- fbrv.model.items(data$factors[[i]],rotate=subrotate[i])
    }
    names(itemcoors) <- names(data$factors)
  }
  
  
    ## global coors for nested plot
  # some useful variables
  nam <- levels(data$global$center_distances$subfactor)
  cplx <- data$global$parameters$complexity
  
  # retrieving the size of the subcircles
  getcircsize <- function(x){
    polcircs <- get(x = "pol_circles",envir = as.environment(x))
    polcircs <- polcircs[1,"radius"]
  }
  circsize <- unlist(lapply(factorcoors,getcircsize))
  circsize <- circsize+cors*cor_spacing
  
  # matching global subfactors with their mean center distance
  global_center_distances <- data.frame(lapply(split(data$global$center_distances,data$global$center_distances$subfactor),function(x)y <- x$mean_center_distance[1]))
  global_center_distances <- t(global_center_distances)
  global_center_distances <- data.frame(global_center_distances)
  
  # pol coors of circles
  pol_circles <- data.frame(phi=rep(NA,cplx+1),rho=rep(0,cplx+1),radius=rep(NA,cplx+1))
  row.names(pol_circles) <- c(levels(data$global$center_distances$factor),nam)
  pol_circles[names(circsize),"radius"] <- circsize
  pol_circles$radius[1] <- max(global_center_distances[nam,]*relative_scaling+circsize[nam]*2)
  pol_circles[nam,"rho"] <- c(global_center_distances[nam,]*relative_scaling+circsize[nam])
  pol_circles$phi <- c(0,2*pi/cplx*c(1:cplx))+rotate
  pol_circles$rho[-1] <- pol_circles$rho[-1]

  
  # cart coors of circles
    # x=cos(phi)*rho; y=sin(phi)*rho
  cart_circles <- pol_circles
  cart_circles[,1] <- round(cos(pol_circles$phi) * pol_circles$rho, digits = 7)
  cart_circles[,2] <- round(sin(pol_circles$phi) * pol_circles$rho, digits = 7)
  names(cart_circles) <- c("x","y","radius")
  row.names(cart_circles)[1] <- ""
  
  # pol coors of inner ring for cor spacing
  if(cors==T){
    pol_inner_ring <- data.frame(phi=rep(NA,cplx+1),rho=rep(NA,cplx+1),radius=rep(NA,cplx+1))
    row.names(pol_inner_ring) <- c(levels(data$global$center_distances$factor),nam)
    pol_inner_ring[names(circsize),"radius"] <- circsize-cors*cor_spacing
    pol_inner_ring[nam,"rho"] <- c(global_center_distances[nam,]*relative_scaling+circsize[nam])
    pol_inner_ring$phi <- c(0,2*pi/cplx*c(1:cplx))+rotate
    pol_inner_ring$rho[-1] <- pol_inner_ring$rho[-1]
    pol_inner_ring <- pol_inner_ring[-1,]
    pol_inner_ring <- pol_inner_ring
  }
  else pol_inner_ring <- NULL

  # cart coors of inner ring for cor spacing
  if(cors==T){
    cart_inner_ring <- pol_inner_ring
    cart_inner_ring[,1] <- round(cos(pol_inner_ring$phi) * pol_inner_ring$rho, digits = 7)
    cart_inner_ring[,2] <- round(sin(pol_inner_ring$phi) * pol_inner_ring$rho, digits = 7)
    names(cart_inner_ring) <- c("x","y","radius")
  }
  else cart_inner_ring <- NULL
  
  # pol coor of axes
    # center, inner and outer intersection of axes and circles, intersection of axes and outer circle
  pol_axes <- data.frame(rho0=rep(0,cplx),rho1=rep(NA,cplx),rho2=rep(NA,cplx),rho3=rep(NA,cplx),phi=rep(NA,cplx))
  row.names(pol_axes) <- nam
  pol_axes$phi <- tail(pol_circles$phi,cplx)
  pol_axes$rho1 <- tail(pol_circles$rho,cplx)-tail(pol_circles$radius,cplx)
  pol_axes$rho2 <- pol_axes$rho1 + 2 * tail(pol_circles$radius,cplx)
  pol_axes$rho3 <- rep(pol_circles$radius[1])
  
  # cart coor of axes
  cart_axes <- data.frame(x0=rep(NA,cplx),y0=rep(NA,cplx),x1=rep(NA,cplx),y1=rep(NA,cplx),
                          x2=rep(NA,cplx),y2=rep(NA,cplx),x3=rep(NA,cplx),y3=rep(NA,cplx))
  row.names(cart_axes) <- nam
  cart_axes$x0 <- round(cos(pol_axes$phi) * pol_axes$rho0, digits = 7)
  cart_axes$x1 <- round(cos(pol_axes$phi) * pol_axes$rho1, digits = 7)
  cart_axes$x2 <- round(cos(pol_axes$phi) * pol_axes$rho2, digits = 7)
  cart_axes$x3 <- round(cos(pol_axes$phi) * pol_axes$rho3, digits = 7)
  cart_axes$y0 <- round(sin(pol_axes$phi) * pol_axes$rho0, digits = 7)
  cart_axes$y1 <- round(sin(pol_axes$phi) * pol_axes$rho1, digits = 7)
  cart_axes$y2 <- round(sin(pol_axes$phi) * pol_axes$rho2, digits = 7)
  cart_axes$y3 <- round(sin(pol_axes$phi) * pol_axes$rho3, digits = 7)
  
  # coor of axis tick label (actual tick defined in plot function)
  axis_tick <- data.frame(rho = 1, phi = NA, x = NA, y = NA)
  axis_tick$phi <- min(pol_axes$phi %% (2*pi)) + pi / cplx
  axis_tick$x <- round(cos(axis_tick$phi) * axis_tick$rho, digits = 7)
  axis_tick$y <- round(sin(axis_tick$phi) * axis_tick$rho, digits = 7)

  # coor of factor name (currently counter-clockwise to smallest circle)
  factor_label <- data.frame(x = NA,y = NA,label = row.names(pol_circles)[1],phi=NA,rho=NA)
  factor_label$phi <- pol_circles[which.min(pol_circles$radius),"phi"]+pi/cplx+rotate_title
  factor_label$rho <- 2/3*max(pol_circles$radius)
  factor_label$x <- round(cos(factor_label$phi)*factor_label$rho, digits = 7)
  factor_label$y <- round(sin(factor_label$phi)*factor_label$rho, digits = 7)
  
  # coor of inner cors as text
  n <- cplx*(cplx-1)
  inner_cors <- data.frame(x=rep(NA,n),y=rep(NA,n),V1=rep(NA,n),V2=rep(NA,n),
                           label=rep(NA,n),xnew=rep(NA,n),ynew=rep(NA,n))
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
  inner_cors$V1 <- b
  inner_cors$V2 <- unlist(lapply(row.names(data$global$subfactor_cors),FUN=rep,times=cplx-1))
  for(k in 1:n) inner_cors$label[k] <- data$global$subfactor_cors[inner_cors$V1[k],inner_cors$V2[k]]
  inner_cors$label <- as.character(inner_cors$label)
  inner_cors$label[inner_cors$label<1] <- substr(inner_cors$label,2,4)
  inner_cors$x <- cart_circles[inner_cors$V2,"x"]
  inner_cors$y <- cart_circles[inner_cors$V2,"y"]
  # scatter as list and anchor towards the center
  # scatter width resembles the angles of an even n-sided polygon for n subfactors (e.g. 90° = pi/2 for 4 subfactors)
  scatter <- rep(seq(from = (-pi+2*pi/cplx)/2,to = (pi-2*pi/cplx)/2,by = (pi-2*pi/cplx)/(cplx-2)),cplx)
  inner_cors$xnew <- inner_cors$x + round(cos(pol_circles[inner_cors$V2,"phi"]+pi+scatter), digits = 7)*(pol_circles[inner_cors$V2,"radius"]-cors*.5*cor_spacing)
  inner_cors$ynew <- inner_cors$y + round(sin(pol_circles[inner_cors$V2,"phi"]+pi+scatter), digits = 7)*(pol_circles[inner_cors$V2,"radius"]-cors*.5*cor_spacing)
  inner_cors$x <- inner_cors$xnew
  inner_cors$y <- inner_cors$ynew
  inner_cors[6:7] <- list(NULL)
  
  
    ## subcircles in nested plot
  # shifted coors of subcircles
  subcircles <- list()
  for(i in 1:cplx) subcircles[[nam[i]]] <- fbrv.shift(factorcoors[[nam[i]]],cart_circles[nam[i],"x"],cart_circles[nam[i],"y"])
  
  # coor of circles
  nested <- list(circles=NULL,axes=NULL,factor_label=NULL,inner_cors=NULL)
  for(i in 1:cplx) nested$circles[[nam[i]]] <- subcircles[[c(i,1)]]
  nested$circles <- lapply(nested$circles,tail,n=-1)
  nested$circles <- do.call("rbind",nested$circles)
  nested$circles$label <- substr(row.names(nested$circles),unlist(gregexpr(pattern = "\\.",row.names(nested$circles)))+1,nchar(row.names(nested$circles)))
  
  # coor of axes
  for(i in 1:cplx) nested$axes[[nam[i]]] <- subcircles[[c(i,2)]]
  nested$axes <- do.call("rbind",nested$axes)
  
  # coor of factor name
  for(i in 1:cplx) nested$factor_label[[nam[i]]] <- subcircles[[c(i,3)]]
  nested$factor_label <- do.call("rbind",nested$factor_label)
  
  # coor of inner cors
  for(i in 1:cplx) nested$inner_cors[[nam[i]]] <- subcircles[[c(i,4)]]
  nested$inner_cors <- do.call("rbind",nested$inner_cors)
  
    ## custom cor arrows in nested plot
  if(!is.null(extra_arrows)){
    n <- dim(extra_arrows)[1]
    arrows <- data.frame(x1=rep(NA,n),x2=rep(NA,n),y1=rep(NA,n),y2=rep(NA,n),label=rep(NA,n),xlabel=rep(NA,n),ylabel=rep(NA,n))
    arrows$label <- extra_arrows$value
    arrows$x1 <- nested$circles[paste(extra_arrows$V1_factor,extra_arrows$V1_subfactor,sep = "."),"x"]
    arrows$y1 <- nested$circles[paste(extra_arrows$V1_factor,extra_arrows$V1_subfactor,sep = "."),"y"]
    arrows$x2 <- nested$circles[paste(extra_arrows$V2_factor,extra_arrows$V2_subfactor,sep = "."),"x"]
    arrows$y2 <- nested$circles[paste(extra_arrows$V2_factor,extra_arrows$V2_subfactor,sep = "."),"y"]
    arrows$x1new <- arrows$x1+subradius/sqrt((arrows$x2-arrows$x1)^2+(arrows$y2-arrows$y1)^2)*(arrows$x2-arrows$x1)
    arrows$x2new <- arrows$x2+subradius/sqrt((arrows$x2-arrows$x1)^2+(arrows$y2-arrows$y1)^2)*(arrows$x1-arrows$x2)
    arrows$y1new <- arrows$y1+subradius/sqrt((arrows$x2-arrows$x1)^2+(arrows$y2-arrows$y1)^2)*(arrows$y2-arrows$y1)
    arrows$y2new <- arrows$y2+subradius/sqrt((arrows$x2-arrows$x1)^2+(arrows$y2-arrows$y1)^2)*(arrows$y1-arrows$y2)
    arrows$x1 <- arrows$x1new
    arrows$x2 <- arrows$x2new
    arrows$y1 <- arrows$y1new
    arrows$y2 <- arrows$y2new
    arrows$xlabel <- (arrows$x1+arrows$x2)/2
    arrows$ylabel <- (arrows$y1+arrows$y2)/2
    # letting the cor labels dodge their arrow
    arrows$xlabel <- arrows$xlabel+.1/sqrt((arrows$x2-arrows$x1)^2+(arrows$y2-arrows$y1)^2)*(arrows$y2-arrows$y1)
    arrows$ylabel <- arrows$ylabel+.1/sqrt((arrows$x2-arrows$x1)^2+(arrows$y2-arrows$y1)^2)*(arrows$x1-arrows$x2)
    arrows[8:11] <- list(NULL)
    rm(n)
  }
  else arrows <- NULL

  rm(nam,cplx)
  
  global <- list(pol_circles = pol_circles,
                      cart_circles = cart_circles,
                      pol_inner_ring = pol_inner_ring,
                      cart_inner_ring = cart_inner_ring,
                      pol_axes = pol_axes,
                      cart_axes = cart_axes,
                      axis_tick = axis_tick,
                      factor_label = factor_label,
                      inner_cors = inner_cors,
                      nested = nested,
                      relative_scaling = relative_scaling,
                      cor_spacing = cor_spacing,
                      arrows = arrows)
    
  coor <- list(factor=factorcoors,global=global)
  if(items==T)coor$items <- itemcoors
  
  return(coor)
}