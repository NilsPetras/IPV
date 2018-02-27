fbrv.model.items <- function(data, rotate = 0){
  
  ## modelling function building coor
  # data is a list containing the data to be drawn
  # rotate is an angle to rotate the whole graphic by
  
  #some useful variables
  cplx <- data$parameters$complexity
  maxcd <- max(data$center_distances$center_distance)
  
  #polcoor of axes and axes labels
  pol_axes <- data.frame(phi = rep(NA, cplx), rho = rep(NA, cplx), rholabel = rep(NA, cplx))
  row.names(pol_axes) <- levels(data$center_distances$subfactor)
  pol_axes$rho <- maxcd * 1.2
  pol_axes$phi <- c(2 * pi / cplx * c(1:cplx)) + rotate
  pol_axes$rholabel <- pol_axes$rho * 1.1
  
  #cartcoor of axes and axes labels
  # x = cos(phi) * rho;y = sin(phi) * rho
  cart_axes <- pol_axes
  cart_axes[, 1] <- cos(pol_axes$phi) * pol_axes$rho
  cart_axes[, 2] <- sin(pol_axes$phi) * pol_axes$rho
  cart_axes[, 3] <- cos(pol_axes$phi) * pol_axes$rholabel
  cart_axes[, 4] <- sin(pol_axes$phi) * pol_axes$rholabel
  names(cart_axes) <- c("x", "y", "xlabel", "ylabel")
  
  # coor of items
  n <- length(data$center_distances$item)
  items <- data.frame(rho = rep(NA, n), phi = rep(NA, n), x = rep(NA, n), y = rep(NA, n), x1 = rep(NA, n), y1 = rep(NA, n), x2 = rep(NA, n), y2 = rep(NA, n), width = rep(NA, n))
  row.names(items) <- data$center_distances$item
  items$phi <- pol_axes$phi[data$center_distances$subfactor]
  items$rho <- data$center_distances$center_distance + .01
  items <- items[order(items$phi, items$rho), ]
  items$x <- cos(items$phi) * items$rho
  items$y <- sin(items$phi) * items$rho
  items$width <- .9 * maxcd
  items$width[seq(from = 1, by = 2, to = length(items$width))] <- 1.1 * maxcd
  items$x1 <- items$x-items$y / items$rho * .03 * items$width
  items$y1 <- items$y + items$x / items$rho * .03 * items$width
  items$x2 <- items$x + items$y / items$rho * .03 * items$width
  items$y2 <- items$y-items$x / items$rho * .03 * items$width
  
  #coor of main grid
  n <- trunc(maxcd * 10)
  maingrid <- data.frame(x = rep(0, n), y = rep(0, n), r = rep(NA, n), alpha = rep(NA, n))
  maingrid$r <- seq(from = .1, by = .1, to = round(maxcd, digits = 2))
  maingrid$alpha <- .5
  maingrid$alpha[seq(from = 5, by = 5, to = length(maingrid$alpha))] <- 1
  
  # coor of axis tick label
  axis_tick <- data.frame(rho = .54, phi = NA, x = NA, y = NA)
  axis_tick$phi <- min(pol_axes$phi[pol_axes$rho>.1] %% (2 * pi)) + pi / 16
  axis_tick$x <- cos(axis_tick$phi) * axis_tick$rho
  axis_tick$y <- sin(axis_tick$phi) * axis_tick$rho
  
  #coor of factor name
  factor_label <- data.frame(phi = mean(pol_axes$phi[1:2]), 
                             rho = .5 * maxcd, 
                             label = data$center_distances$factor[1], 
                             x = NA, y = NA)
  factor_label$x <- cos(factor_label$phi) * factor_label$rho
  factor_label$y <- sin(factor_label$phi) * factor_label$rho
  
  coor <- list(pol_axes = pol_axes, 
               cart_axes = cart_axes, 
               items = items, 
               maingrid = maingrid, 
               axis_tick = axis_tick, 
               factor_label = factor_label)
  
  return(coor)
}