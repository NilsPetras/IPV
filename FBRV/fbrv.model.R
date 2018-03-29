fbrv.model <- function(data,subradius,rotate=0,rotate_title=0) {
  
  ## modelling function building coor
  # data is a list with a specific structure
  # subradius is the radius of the inner circles
  # rotate is an angle to rotate the whole graphic by

  # some useful variables
  cplx <- data$parameters$complexity
  
  # pol coor of circles (center of main circle is (0,0), coor are centers of circles, phi refers to the angle, rho to the distance)
  pol_circles <- data.frame(phi=rep(NA,cplx+1),rho=rep(NA,cplx+1),radius=rep(NA,cplx+1))
  row.names(pol_circles) <- c(levels(data$center_distances$factor),levels(data$center_distances$subfactor))
  pol_circles$radius[1] <- max(data$center_distances$mean_center_distance)+2*subradius
  pol_circles$radius[2:length(pol_circles$radius)] <- subradius
  pol_circles$rho <- c(0,tapply(data$center_distances$center_distance,data$center_distances$subfactor,mean)+subradius)
  pol_circles$phi <- c(0,2*pi/cplx*c(1:cplx))+rotate
  
  # cart coor of circles
    # x=cos(phi)*rho; y=sin(phi)*rho
  cart_circles <- pol_circles
  cart_circles[,1] <- round(cos(pol_circles$phi) * pol_circles$rho, digits = 7)
  cart_circles[,2] <- round(sin(pol_circles$phi) * pol_circles$rho, digits = 7)
  names(cart_circles) <- c("x","y","radius")
  row.names(cart_circles)[1] <- ""
  
  # pol coor of axes
    # center, inner and outer intersection of axes and circles, intersection of axes and outer circle
  pol_axes <- data.frame(rho0=rep(0,cplx),rho1=rep(NA,cplx),rho2=rep(NA,cplx),rho3=rep(NA,cplx),phi=rep(NA,cplx))
  row.names(pol_axes) <- c(levels(data$center_distances$subfactor))
  pol_axes$phi <- tail(pol_circles$phi,cplx)
  pol_axes$rho1 <- tail(pol_circles$rho,cplx)-subradius
  pol_axes$rho2 <- pol_axes$rho1 + 2 * subradius
  pol_axes$rho3 <- rep(max(pol_circles$radius))
  
  # cart coor of axes
  cart_axes <- data.frame(x0=rep(NA,cplx),y0=rep(NA,cplx),x1=rep(NA,cplx),y1=rep(NA,cplx),
                          x2=rep(NA,cplx),y2=rep(NA,cplx),x3=rep(NA,cplx),y3=rep(NA,cplx))
  row.names(cart_axes) <- c(levels(data$center_distances$subfactor))
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
  axis_tick$phi <- min(pol_axes$phi) - pi / cplx
  axis_tick$x <- round(cos(axis_tick$phi) * axis_tick$rho, digits = 7)
  axis_tick$y <- round(sin(axis_tick$phi) * axis_tick$rho, digits = 7)
  
  # coor of factor name (default counter-clockwise to smallest circle)
  factor_label <- data.frame(x = NA,y = NA,label = row.names(pol_circles)[1],phi=NA,rho=NA)
  factor_label$phi <- pol_circles[which.min(pol_circles$rho),"phi"]+pi/cplx+rotate_title
  factor_label$rho <- 2/3*max(pol_circles$radius)
  factor_label$x <- round(cos(factor_label$phi)*factor_label$rho, digits = 7)
  factor_label$y <- round(sin(factor_label$phi)*factor_label$rho, digits = 7)
  
  # coor of inner cors as text
  n <- cplx*(cplx-1)
  inner_cors <- data.frame(x=rep(NA,n),y=rep(NA,n),V1=rep(NA,n),V2=rep(NA,n),
                           label=rep(NA,n),xnew=rep(NA,n),ynew=rep(NA,n))
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
  inner_cors$xnew <- inner_cors$x + round(cos(pol_circles[inner_cors$V2,"phi"]+pi+scatter), digits = 7)*pol_circles[inner_cors$V2,"radius"]*.75
  inner_cors$ynew <- inner_cors$y + round(sin(pol_circles[inner_cors$V2,"phi"]+pi+scatter), digits = 7)*pol_circles[inner_cors$V2,"radius"]*.75
  inner_cors$x <- inner_cors$xnew
  inner_cors$y <- inner_cors$ynew
  inner_cors[6:7] <- list(NULL)
  
  coor <- list(pol_circles = pol_circles,
               cart_circles = cart_circles,
               pol_axes = pol_axes,
               cart_axes = cart_axes,
               axis_tick = axis_tick,
               factor_label = factor_label,
               inner_cors = inner_cors)
    
  return(coor)
}