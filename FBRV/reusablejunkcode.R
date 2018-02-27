# junk code for possible reuse of ideas

    # coordinates in fbrv.model by direction (abandoned idea)
if(correlations=="inner"){
  if(cplx>3){
    n <- cplx*(cplx-3)/2
    coor$inner_cors <- data.frame(x1=rep(NA,n),y1=rep(NA,n),x2=rep(NA,n),y2=rep(NA,n),V1=rep(NA,n),V2=rep(NA,n),label=rep(NA,n))
    i <- c((cplx-3),(cplx-3):1)
    coor$inner_cors$V1 <- rep(row.names(data$subfactor_cors)[1:length(i)],i)
    j <- as.list(rep(0,length(i)))
    for(k in 1:length(i)) j[[k]] <- c(i[k]:1)
    j <- unlist(j)+1+rep(1:length(i),i)
    coor$inner_cors$V2 <- row.names(data$subfactor_cors[j,])
    for(k in 1:n) coor$inner_cors$label[k] <- data$subfactor_cors[coor$inner_cors$V1[k],coor$inner_cors$V2[k]]
    coor$inner_cors$label <- as.character(coor$inner_cors$label)
    coor$inner_cors$label[coor$inner_cors$label<1] <- substr(coor$inner_cors$label,2,4)
    coor$inner_cors$x1 <- coor$cart_circles[coor$inner_cors$V1,"x"]
    coor$inner_cors$x2 <- coor$cart_circles[coor$inner_cors$V2,"x"]
    coor$inner_cors$y1 <- coor$cart_circles[coor$inner_cors$V1,"y"]
    coor$inner_cors$y2 <- coor$cart_circles[coor$inner_cors$V2,"y"]
    coor$inner_cors$x1new <- coor$inner_cors$x1 + (0.03+subrad)/sqrt(((coor$inner_cors$x2-coor$inner_cors$x1)^2+(coor$inner_cors$y2-coor$inner_cors$y1)^2))*(coor$inner_cors$x2-coor$inner_cors$x1)
    coor$inner_cors$x2new <- coor$inner_cors$x2 + (0.03+subrad)/sqrt(((coor$inner_cors$x2-coor$inner_cors$x1)^2+(coor$inner_cors$y2-coor$inner_cors$y1)^2))*(coor$inner_cors$x1-coor$inner_cors$x2)
    coor$inner_cors$y1new <- coor$inner_cors$y1 + (0.03+subrad)/sqrt(((coor$inner_cors$x2-coor$inner_cors$x1)^2+(coor$inner_cors$y2-coor$inner_cors$y1)^2))*(coor$inner_cors$y2-coor$inner_cors$y1)
    coor$inner_cors$y2new <- coor$inner_cors$y2 + (0.03+subrad)/sqrt(((coor$inner_cors$x2-coor$inner_cors$x1)^2+(coor$inner_cors$y2-coor$inner_cors$y1)^2))*(coor$inner_cors$y1-coor$inner_cors$y2)
    coor$inner_cors$x1 <- coor$inner_cors$x1new
    coor$inner_cors$x2 <- coor$inner_cors$x2new
    coor$inner_cors$y1 <- coor$inner_cors$y1new
    coor$inner_cors$y2 <- coor$inner_cors$y2new
    coor$inner_cors[8:11] <- list(NULL)
  }
}
if(correlations=="all"){
  n <- cplx*(cplx-1)
  coor$inner_cors <- data.frame(x1=rep(NA,n),y1=rep(NA,n),x2=rep(NA,n),y2=rep(NA,n),V1=rep(NA,n),V2=rep(NA,n),label=rep(NA,n))
  a <- rep(row.names(data$subfactor_cors),cplx)
  for(k in cplx:1) a <- a[-(cplx*(k-1)+k)] 
  coor$inner_cors$V1 <- a
  coor$inner_cors$V2 <- unlist(lapply(row.names(data$subfactor_cors),FUN=rep,times=cplx-1))
  for(k in 1:n) coor$inner_cors$label[k] <- data$subfactor_cors[coor$inner_cors$V1[k],coor$inner_cors$V2[k]]
  coor$inner_cors$label <- as.character(coor$inner_cors$label)
  coor$inner_cors$label[coor$inner_cors$label<1] <- substr(coor$inner_cors$label,2,4)
  coor$inner_cors$x1 <- coor$cart_circles[coor$inner_cors$V1,"x"]
  coor$inner_cors$x2 <- coor$cart_circles[coor$inner_cors$V2,"x"]
  coor$inner_cors$y1 <- coor$cart_circles[coor$inner_cors$V1,"y"]
  coor$inner_cors$y2 <- coor$cart_circles[coor$inner_cors$V2,"y"]
  coor$inner_cors$x1new <- coor$inner_cors$x1 + (-0.03+subrad)/sqrt(((coor$inner_cors$x2-coor$inner_cors$x1)^2+(coor$inner_cors$y2-coor$inner_cors$y1)^2))*(coor$inner_cors$x2-coor$inner_cors$x1)
  coor$inner_cors$x2new <- coor$inner_cors$x2 + (-0.03+subrad)/sqrt(((coor$inner_cors$x2-coor$inner_cors$x1)^2+(coor$inner_cors$y2-coor$inner_cors$y1)^2))*(coor$inner_cors$x1-coor$inner_cors$x2)
  coor$inner_cors$y1new <- coor$inner_cors$y1 + (-0.03+subrad)/sqrt(((coor$inner_cors$x2-coor$inner_cors$x1)^2+(coor$inner_cors$y2-coor$inner_cors$y1)^2))*(coor$inner_cors$y2-coor$inner_cors$y1)
  coor$inner_cors$y2new <- coor$inner_cors$y2 + (-0.03+subrad)/sqrt(((coor$inner_cors$x2-coor$inner_cors$x1)^2+(coor$inner_cors$y2-coor$inner_cors$y1)^2))*(coor$inner_cors$y1-coor$inner_cors$y2)
  coor$inner_cors$x1 <- coor$inner_cors$x1new
  coor$inner_cors$x2 <- coor$inner_cors$x2new
  coor$inner_cors$y1 <- coor$inner_cors$y1new
  coor$inner_cors$y2 <- coor$inner_cors$y2new
  coor$inner_cors[8:11] <- list(NULL)
}

    # adding initial of correlation partner to list
coor$inner_cors$label <- paste(substr(coor$inner_cors$V1,1,1),coor$inner_cors$label,sep = " ")

    # axis tick as line in first quadrant instead of dashed circle
# coor of axis tick
coor$axis_tick <- data.frame(t(as.matrix(c(.1,rep(0,7)))))
names(coor$axis_tick) <- c("rho","phi","x","y","x1","y1","x2","y2")
coor$axis_tick$phi <- min(coor$pol_axes$phi[coor$pol_axes$rho1>.1] %% (2 * pi))
coor$axis_tick$x <- cos(coor$axis_tick$phi) * coor$axis_tick$rho
coor$axis_tick$y <- sin(coor$axis_tick$phi) * coor$axis_tick$rho
coor$axis_tick$x1 <- coor$axis_tick$x - .3 * coor$axis_tick$y
coor$axis_tick$y1 <- coor$axis_tick$y + .3 * coor$axis_tick$x
coor$axis_tick$x2 <- coor$axis_tick$x + .3 * coor$axis_tick$y
coor$axis_tick$y2 <- coor$axis_tick$y - .3 * coor$axis_tick$x

# coor of axis tick label
coor$axis_tick_label <- data.frame(t(as.matrix(c(0,0))))
names(coor$axis_tick_label) <- c("x","y")
coor$axis_tick_label$x <- coor$axis_tick$x2 + .06
coor$axis_tick_label$y <- coor$axis_tick$y2 

# coor of subfactor cor arrows as tangents
subfactor_cor_arrows <- data.frame(cart_circles$x)
names(subfactor_cor_arrows) <- "x1"
subfactor_cor_arrows$y1 <- cart_circles$y
subfactor_cor_arrows <- subfactor_cor_arrows[-1,]
subfactor_cor_arrows$x2 <- subfactor_cor_arrows$x1[(1:(cplx))%%cplx+1]
subfactor_cor_arrows$y2 <- subfactor_cor_arrows$y1[(1:(cplx))%%cplx+1]
subfactor_cor_arrows$dx <- subfactor_cor_arrows$x2-subfactor_cor_arrows$x1
subfactor_cor_arrows$dy <- subfactor_cor_arrows$y2-subfactor_cor_arrows$y1
subfactor_cor_arrows$k <- subfactor_cor_arrows$dx/subfactor_cor_arrows$dy
subfactor_cor_arrows$addx <- abs(subrad/sqrt(subfactor_cor_arrows$k^2+1))
subfactor_cor_arrows$addy <- abs(subfactor_cor_arrows$addx*subfactor_cor_arrows$k)
# the shift direction is based on the fact, that all arrows are determined counter-clockwise and based on
# the sign of the relative position of the circles on the respective other dimension (-dx for addy, dy for addx)
subfactor_cor_arrows$y1 <- subfactor_cor_arrows$y1+subfactor_cor_arrows$addy*-1*sign(subfactor_cor_arrows$dx)
subfactor_cor_arrows$y2 <- subfactor_cor_arrows$y2+subfactor_cor_arrows$addy*-1*sign(subfactor_cor_arrows$dx)
subfactor_cor_arrows$x1 <- subfactor_cor_arrows$x1+subfactor_cor_arrows$addx*sign(subfactor_cor_arrows$dy)
subfactor_cor_arrows$x2 <- subfactor_cor_arrows$x2+subfactor_cor_arrows$addx*sign(subfactor_cor_arrows$dy)

# arrows as partial tangents with shorter ends to avoid overlap with circles
subfactor_cor_arrows$x1new <- subfactor_cor_arrows$x1+0.075/sqrt(((subfactor_cor_arrows$x2-subfactor_cor_arrows$x1)^2+(subfactor_cor_arrows$y2-subfactor_cor_arrows$y1)^2))*(subfactor_cor_arrows$x2-subfactor_cor_arrows$x1)
subfactor_cor_arrows$x2new <- subfactor_cor_arrows$x2+0.075/sqrt(((subfactor_cor_arrows$x2-subfactor_cor_arrows$x1)^2+(subfactor_cor_arrows$y2-subfactor_cor_arrows$y1)^2))*(subfactor_cor_arrows$x1-subfactor_cor_arrows$x2)
subfactor_cor_arrows$y1new <- subfactor_cor_arrows$y1+0.075/sqrt(((subfactor_cor_arrows$x2-subfactor_cor_arrows$x1)^2+(subfactor_cor_arrows$y2-subfactor_cor_arrows$y1)^2))*(subfactor_cor_arrows$y2-subfactor_cor_arrows$y1)
subfactor_cor_arrows$y2new <- subfactor_cor_arrows$y2+0.075/sqrt(((subfactor_cor_arrows$x2-subfactor_cor_arrows$x1)^2+(subfactor_cor_arrows$y2-subfactor_cor_arrows$y1)^2))*(subfactor_cor_arrows$y1-subfactor_cor_arrows$y2)
subfactor_cor_arrows$x1 <- subfactor_cor_arrows$x1new
subfactor_cor_arrows$x2 <- subfactor_cor_arrows$x2new
subfactor_cor_arrows$y1 <- subfactor_cor_arrows$y1new
subfactor_cor_arrows$y2 <- subfactor_cor_arrows$y2new
subfactor_cor_arrows[10:13] <- list(NULL)

# coor of subfactor cor labels
subfactor_cor_labels <- as.matrix(rep(0,5*cplx))
dim(subfactor_cor_labels) <- c(5,cplx)
subfactor_cor_labels <- data.frame(t(subfactor_cor_labels))
names(subfactor_cor_labels) <- c("x","y","cor","V1","V2")
subfactor_cor_labels$x <- .50*(subfactor_cor_arrows$x1+subfactor_cor_arrows$x2)
subfactor_cor_labels$y <- .50*(subfactor_cor_arrows$y1+subfactor_cor_arrows$y2)
subfactor_cor_labels$x <- subfactor_cor_labels$x + 0.06/sqrt(((subfactor_cor_arrows$x2-subfactor_cor_arrows$x1)^2+(subfactor_cor_arrows$y2-subfactor_cor_arrows$y1)^2))*(subfactor_cor_arrows$y2-subfactor_cor_arrows$y1)
subfactor_cor_labels$y <- subfactor_cor_labels$y + 0.06/sqrt(((subfactor_cor_arrows$x2-subfactor_cor_arrows$x1)^2+(subfactor_cor_arrows$y2-subfactor_cor_arrows$y1)^2))*(subfactor_cor_arrows$x1-subfactor_cor_arrows$x2) 
subfactor_cor_labels$V1 <- levels(data$center_distances$subfactor)
subfactor_cor_labels$V2 <- subfactor_cor_labels$V1[(1:(cplx))%%cplx+1]
for(i in 1:cplx)  subfactor_cor_labels$cor[i] <- data$subfactor_cors[subfactor_cor_labels$V1[i],subfactor_cor_labels$V2[i]]
subfactor_cor_labels$cor <- as.character(subfactor_cor_labels$cor)
subfactor_cor_labels$cor[subfactor_cor_labels$cor<1] <- substr(subfactor_cor_labels$cor,2,4)

