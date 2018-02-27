fbrv.model <- function(data,subrad,rotate=0) {
  
  ## modelling function building coor
  # data is a list with a specific structure
  # subrad is the radius of the inner circles
  # rotate is an angle to rotate the whole graphic by

  # some useful variables
  cplx <- data$parameters$complexity
  
  # pol coor of circles (center of main circle is (0,0), coor are centers of circles, phi refers to the angle, rho to the distance)
  pol_circles <- as.matrix(rep(0,3*(1+cplx)))
  dim(pol_circles) <- c(3,1+cplx)
  pol_circles <- data.frame(t(pol_circles),row.names = c(levels(data$center_distances$factor),levels(data$center_distances$subfactor)))
  names(pol_circles) <- c("phi","rho","radius")
  pol_circles$radius[1] <- max(data$center_distances$mean_center_distance)+2*subrad
  pol_circles$radius[2:length(pol_circles$radius)] <- subrad
  pol_circles$rho <- c(0,tapply(data$center_distances$center_distance,data$center_distances$subfactor,mean)+subrad)
  pol_circles$phi <- c(0,2*pi/cplx*c(1:cplx))+rotate
  
  # cart coor of circles
    # x=cos(phi)*rho; y=sin(phi)*rho
  cart_circles <- pol_circles
  cart_circles[,1] <- cos(pol_circles$phi) * pol_circles$rho
  cart_circles[,2] <- sin(pol_circles$phi) * pol_circles$rho
  names(cart_circles) <- c("x","y","radius")
  row.names(cart_circles)[1] <- ""
  
  # pol coor of axes
  pol_axes <- as.matrix(rep(0,5*(cplx)))
  dim(pol_axes) <- c(5,cplx)
  pol_axes <- data.frame(t(pol_axes),row.names = c(levels(data$center_distances$subfactor)))
  names(pol_axes) <- c("rho0","rho1","rho2","rho3","phi") # center, inner and outer intersection of axes and circles, intersection of axes and outer circle
  pol_axes$phi <- tail(pol_circles$phi,cplx)
  pol_axes$rho1 <- tail(pol_circles$rho,cplx)-subrad
  pol_axes$rho2 <- pol_axes$rho1 + 2 * subrad
  pol_axes$rho3 <- rep(max(pol_circles$radius))
  
  # cart coor of axes
  cart_axes <- as.matrix(rep(0,8*cplx))
  dim(cart_axes) <- c(8,cplx)
  cart_axes <- data.frame(t(cart_axes),row.names = c(levels(data$center_distances$subfactor)))
  names(cart_axes) <- c("x0","y0","x1","y1","x2","y2","x3","y3")
  cart_axes$x0 <- cos(pol_axes$phi) * pol_axes$rho0
  cart_axes$x1 <- cos(pol_axes$phi) * pol_axes$rho1
  cart_axes$x2 <- cos(pol_axes$phi) * pol_axes$rho2
  cart_axes$x3 <- cos(pol_axes$phi) * pol_axes$rho3
  cart_axes$y0 <- sin(pol_axes$phi) * pol_axes$rho0
  cart_axes$y1 <- sin(pol_axes$phi) * pol_axes$rho1
  cart_axes$y2 <- sin(pol_axes$phi) * pol_axes$rho2
  cart_axes$y3 <- sin(pol_axes$phi) * pol_axes$rho3
  
  # coor of factor name
  factor_label <- data.frame(x = NA,y = NA,label = row.names(pol_circles)[1],phi=NA,rho=NA)
  factor_label$phi <- pol_circles[min(pol_circles$radius),"phi"]-pi/cplx
  factor_label$rho <- 2/3*max(pol_circles$radius)
  factor_label$x <- cos(factor_label$phi)*factor_label$rho
  factor_label$y <- sin(factor_label$phi)*factor_label$rho
  if(cplx==2){
    factor_label$x <- factor_label$x*2
    factor_label$y <- factor_label$y*2
  }
  
  # coor of inner cors as text
  n <- cplx*(cplx-1)
  inner_cors <- data.frame(x=rep(NA,n),y=rep(NA,n),V1=rep(NA,n),V2=rep(NA,n),label=rep(NA,n),xnew=rep(NA,n),ynew=rep(NA,n))
  # subfactor list 1
  a <- row.names(data$subfactor_cors)
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
  inner_cors$V2 <- unlist(lapply(row.names(data$subfactor_cors),FUN=rep,times=cplx-1))
  for(k in 1:n) inner_cors$label[k] <- data$subfactor_cors[inner_cors$V1[k],inner_cors$V2[k]]
  inner_cors$label <- as.character(inner_cors$label)
  inner_cors$label[inner_cors$label<1] <- substr(inner_cors$label,2,4)
  inner_cors$x <- cart_circles[inner_cors$V2,"x"]
  inner_cors$y <- cart_circles[inner_cors$V2,"y"]
  # scatter as list and anchor towards the center
  # scatter width resembles the angles of an even n-sided polygon for n subfactors (e.g. 90° = pi/2 for 4 subfactors)
  scatter <- rep(seq(from = (-pi+2*pi/cplx)/2,to = (pi-2*pi/cplx)/2,by = (pi-2*pi/cplx)/(cplx-2)),cplx)
  inner_cors$xnew <- inner_cors$x + cos(pol_circles[inner_cors$V2,"phi"]+pi+scatter)*pol_circles[inner_cors$V2,"radius"]*.75
  inner_cors$ynew <- inner_cors$y + sin(pol_circles[inner_cors$V2,"phi"]+pi+scatter)*pol_circles[inner_cors$V2,"radius"]*.75
  inner_cors$x <- inner_cors$xnew
  inner_cors$y <- inner_cors$ynew
  inner_cors[6:7] <- list(NULL)
  if(cplx>2)inner_cors$isneighbour <- rep(c(TRUE,rep(FALSE,times = max(0,cplx-3)),TRUE),times = cplx)
  else inner_cors$isneighbour <- TRUE
  
  coor <- list(pol_circles = pol_circles,
               cart_circles = cart_circles,
               pol_axes = pol_axes,
               cart_axes = cart_axes,
               factor_label = factor_label,
               inner_cors = inner_cors)
    
  return(coor)
}