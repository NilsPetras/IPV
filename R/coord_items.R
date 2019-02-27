#' Coord Items
#'
#' Generates the coordinates for an item chart.
#'
#' @param data data in the appropriate format, given by the input functions.
#' @param rotate_radians integer; radian angle to rotate the chart
#'   counter-clockwise by; use fractions of pi (e.g. pi/2 = 90 degrees).
#' @param rotate_degrees integer; angle in degrees to rotate the chart
#'   counter-clockwise by.
#' @param rotate_test_label_radians integer; radian angle to rotate the test label
#'   counter-clockwise by; use fractions of pi (e.g. pi/2 = 90 degrees).
#' @param rotate_test_label_degrees integer; angle in degrees to rotate the test
#'   label counter-clockwise by.
#' @param width_items integer; item bar width relative to default.
#' @param length_items integer; item bar length relative to default.
#' @param length_ratio_items integer; relative item bar length; defaults to 1.5.
#' @param dodge integer; horizontal outward dodge of facet labels relative to
#'   default.
#'
#' @details Use \code{\link{item_chart}} to create item charts.
#'
#' @return List containing coordinates of chart objects.
#'
#' @seealso \code{\link{plot_items}} \code{\link{coord_nested}}
#'   \code{\link{item_chart}}
coord_items <- function (
  data,
  rotate_radians = 0,
  rotate_degrees = 0,
  rotate_test_label_radians = 0,
  rotate_test_label_degrees = 0,
  width_items = 1,
  length_items = 1,
  length_ratio_items = 1.5,
  dodge = 1) {


  # helper variables -----------------------------------------------------------

  # number of facets (=subfactors)
  cplx <- length(colnames(data$cors))

  # total rotation value in radians
  rotate <- rotate_radians + rotate_degrees * pi / 180

  # total test label rotation value in radians
  rotate_test_label <- rotate_test_label_radians + rotate_test_label_degrees * pi / 180

  # maximum center distance (determining the chart size)
  maxcd <- max(data$cds$cd)


  # chart objects --------------------------------------------------------------

  ## axes ---------------------------

  # polar coordinates of axis line ends and axis labels
  # axis ends at 120 % of the maximum center distance
  # axis labels at 110 % of the axis ends
  p_axes <- data.frame(phi = rep(NA, cplx),
                       rho = NA,
                       rholabel = NA)
  row.names(p_axes) <- levels(data$cds$subfactor)
  p_axes$rho <- maxcd * 1.2
  p_axes$phi <- c(2 * pi / cplx * c(1:cplx)) + rotate
  p_axes$phi[p_axes$phi > 2 * pi] <-
    p_axes$phi[p_axes$phi > 2 * pi] - 2 * pi
  p_axes$rholabel <- p_axes$rho * 1.1

  # cartesian coordinates
  # x = cos(phi) * rho
  # y = sin(phi) * rho
  c_axes <- p_axes
  # rounded values to decrease display length in console
  c_axes[ ,1] <- round(cos(p_axes$phi) * p_axes$rho, digits = 7)
  c_axes[ ,2] <- round(sin(p_axes$phi) * p_axes$rho, digits = 7)
  c_axes[ ,3] <- round(cos(p_axes$phi) * p_axes$rholabel, digits = 7) +
    cos(p_axes$phi) * .1 * dodge * p_axes$rho
  c_axes[ ,4] <- round(sin(p_axes$phi) * p_axes$rholabel, digits = 7)
  names(c_axes) <- c("x", "y", "xlabel", "ylabel")


  ## items --------------------------

  # coordinates of item bars
  # the length of item bars alternates between long and short to better
  # distinguish similar items
  # the length of item bars is relative to the overall size of the chart
  # center distances = from the origin to the inner edge of the item bar
  # n = number of items
  n <- length(data$cds$item)
  items <- data.frame(
    rho = rep(NA, n), phi = NA,
    x   = NA, y   = NA,
    x1  = NA, y1  = NA,
    x2  = NA, y2  = NA,
    length = NA)
  row.names(items) <- data$cds$item
  items$phi <- p_axes$phi[data$cds$subfactor]
  items$rho <- data$cds$cd + maxcd * .00625 * width_items
  items <- items[order(items$phi, items$rho), ]
  items$x <- round(cos(items$phi) * items$rho, digits = 7)
  items$y <- round(sin(items$phi) * items$rho, digits = 7)

  # items are split by facets so each facet gets its own sequence of item
  # lengths (alternating, beginning with long, short, long, ...)
  # the anonymous function uses placeholder values to limit its environment
  items$length <-  unlist(lapply(split(items, data$cds$subfactor),
                                 function (x) {
    x$length <- 1
    x$length[seq(from = 1, by = 2, to = length(x$length))] <- 2
  return(x$length)}))

  items$length[items$length == 1] <- 1.2 * length_items * maxcd
  items$length[items$length == 2] <-
    1.2 * length_items * length_ratio_items * maxcd

  items$x1 <- items$x - items$y / items$rho * .03 * items$length
  items$y1 <- items$y + items$x / items$rho * .03 * items$length
  items$x2 <- items$x + items$y / items$rho * .03 * items$length
  items$y2 <- items$y - items$x / items$rho * .03 * items$length


  ## grid ---------------------------

  # coordinates of grid lines
  # m = magnitude, d = digits, u = unit
  m <- floor(log10(maxcd))
  d <- maxcd / 10 ^ m
  u <- 10 ^ (m - 1)

  if (floor(d) < 3) {
    grid <- data.frame(x = rep(0, floor(10 * d)),
                       y = 0,
                       r = NA,
                       alpha = .5)
    grid$r <- seq(from = u,
                  by = u,
                  to = floor(10 * d) * u)
    grid$alpha[seq(10, length(grid$alpha), 10)] <- 1
  }  else if (floor(d) < 5) {
    grid <- data.frame(x = rep(0, floor(5 * d)),
                       y = 0,
                       r = NA,
                       alpha = .5)
    grid$r <- seq(from = 2 * u,
                  by = 2 * u,
                  to = floor(5 * d) * 2 * u)
    grid$alpha[seq(5, length(grid$alpha), 5)] <- 1
  } else {
    grid <- data.frame(x = rep(0, floor(2 * d)),
                       y = 0,
                       r = NA,
                       alpha = .5)
    grid$r <- seq(from = 5 * u,
                  by = 5 * u,
                  to = floor(2 * d) * 5 * u)
    grid$alpha[seq(2, length(grid$alpha), 2)] <- 1
  }

  # axis tick label
  # the first major grid line is marked
  # the axis tick label automatically shows within the first quadrant
  axis_tick <- data.frame(
    rho = grid[grid$alpha == 1, "r"] + u / 2,
    label = as.character(grid[grid$alpha == 1, "r"]),
    phi = NA,
    x = NA,
    y = NA)
  axis_tick$phi <- min(p_axes[p_axes$rho > 0, "phi"]) -
    pi / cplx

  axis_tick$x <- round(cos(axis_tick$phi) * axis_tick$rho, digits = 7)
  axis_tick$y <- round(sin(axis_tick$phi) * axis_tick$rho, digits = 7)


  ## test label ---------------------

  # coordinates of factor name
  # the factor label automatically shows between the first two facets
  # the factor label shows at half the maximum center distance from the origin
  test_label <- data.frame(phi = mean(p_axes$phi[1] + pi / cplx) + rotate_test_label,
                      rho = .5 * maxcd,
                      label = data$cds$factor[1],
                      x = NA, y = NA)
  test_label$x <- round(cos(test_label$phi) * test_label$rho, digits = 7)
  test_label$y <- round(sin(test_label$phi) * test_label$rho, digits = 7)


  # return ---------------------------------------------------------------------

  # list of all dataframes containing the coordinates of the chart objects
  coord <- list(p_axes    = p_axes,
                c_axes    = c_axes,
                items     = items,
                grid      = grid,
                axis_tick = axis_tick,
                test_label     = test_label)

  return(coord)
}
