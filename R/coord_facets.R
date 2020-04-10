#' Coord Facets
#'
#' Generates the coordinates for a facet chart.
#'
#' @param data SEM estimates in the appropriate format, given by the input
#'   functions.
#' @param facet_order character; vector of facet names in desired order
#'   (counter-clockwise); defaults to NULL, in which case the order is based on
#'   the correlation matrix columns in 'data'.
#' @param subradius integer; same unit as center distances; radius of the facet
#'   circles; defaults to 0, in which case an appropriate value is estimated.
#' @param tick numeric; axis tick position; defaults to 0, in which case an
#'   appropriate value is estimated.
#' @param rotate_tick_label numeric; number of positions to move the tick label
#'   (counter-clockwise); defaults to 0.
#' @param rotate_radians integer; radian angle to rotate the chart
#'   counter-clockwise by; use fractions of pi (e.g. pi/2 = 90 degrees).
#' @param rotate_degrees integer; angle in degrees to rotate the chart
#'   counter-clockwise by.
#' @param dist_test_label integer; position of the test label relative to the
#'   surrounding circle; defaults to 2/3, in which case the test label is
#'   displayed 2/3 of the way from the center to the surrounding circle.
#' @param rotate_test_label_radians integer; radian angle to rotate the test
#'   label counter-clockwise by; use fractions of pi (e.g. pi/2 = 90 degrees).
#' @param rotate_test_label_degrees integer; angle in degrees to rotate the
#'   global label counter-clockwise by.
#'
#' @details Use \code{\link{facet_chart}} to create facet charts.
#'
#' @return List containing coordinates of chart objects.
#'
#' @seealso \code{\link{plot_facets}} \code{\link{facet_chart}}
coord_facets <- function (
  data,
  facet_order = NULL,
  subradius = 0,
  tick = 0,
  rotate_tick_label = 0,
  rotate_radians = 0,
  rotate_degrees = 0,
  dist_test_label = 2 / 3,
  rotate_test_label_radians = 0,
  rotate_test_label_degrees = 0) {




  # test without facets --------------------------------------------------------

  # tests without facets are indicated by data = NA
  # in this case, only the test label (centered) and the outer circle
  # (radius = subradius) should be drawn.

  if (anyNA(data)) {
    p_circs <- data.frame(phi = 0,
                          rho = 0,
                          radius = subradius)
    row.names(p_circs) <- "temp"
    c_circs <- p_circs
    names(c_circs) <- c("x","y","radius")
    p_axes <- data.frame(rho0 = NA,
                         rho1 = NA,
                         rho2 = NA,
                         rho3 = NA,
                         phi = NA)
    c_axes <- data.frame(x0 = NA, y0 = NA,
                         x1 = NA, y1 = NA,
                         x2 = NA, y2 = NA,
                         x3 = NA, y3 = NA)
    axis_tick <- data.frame(rho = NA, phi = NA, x = NA, y = NA)
    test_label <- data.frame(x = 0,
                             y = 0,
                             label = "temp",
                             phi = 0,
                             rho = 0)
    cors <- data.frame(x = NA,
                       y = NA,
                       V1 = NA,
                       V2 = NA,
                       label = NA)

    coord <- list(p_circs    = p_circs,
                  c_circs    = c_circs,
                  p_axes     = p_axes,
                  c_axes     = c_axes,
                  axis_tick  = axis_tick,
                  test_label = test_label,
                  cors       = cors)
    return (coord)
  }

  # helper variables -----------------------------------------------------------

  cplx <- length(colnames(data$cors))
  rotate <- rotate_radians + rotate_degrees * pi / 180
  rotate_test_label <- rotate_test_label_radians +
    rotate_test_label_degrees * pi / 180
  mcd <- data$cds$mean_cd
  if (is.null(facet_order)) {
    facet_order <- colnames(data$cors)
  }
  # ignore additional names for calls by coord_nested()
  facet_order <- facet_order[facet_order %in% colnames(data$cors)]

  # default axis tick and subradius need to scale based on the data, to avoid
  # messy results
  if (tick == 0){
    tick <- signif(max(.15 * max(mcd), .3 * min(mcd)), 1)
    sc <- rep(c(1, 2, 5), 5) * 10 ^ rep(-3:1, each = 3)
    tick <- sc[which.min(abs(tick - sc))]
  }
  if (subradius == 0) {
    subradius <- max(mean(mcd), .25 * max(mcd)) *
    (5 / (3 + cplx)) *
    (.25 + .25 * (min(max(mean(mcd), .25 * max(mcd)) / stats::sd(mcd), 3)))
    message(paste("Facet circle radius set to ",
                  signif(subradius, digits = 3),
                  " based on the data.",
                  sep = ""))
  }

  # chart objects --------------------------------------------------------------



  ## circles ------------------------

  # polar (p_) for easier calculations, carthesian (c_) for application
  # note: the surrounding circle is the first in the data frame
  p_circs <- data.frame(phi = rep(NA, cplx + 1),
                        rho = 0,
                        radius = NA)
  row.names(p_circs) <- c(levels(data$cds$factor),
                          facet_order)
  p_circs$radius[1] <- max(mcd) + 2 * subradius
  p_circs$radius[2:length(p_circs$radius)] <- subradius
  mean_cds <- tapply(data$cds$cd, data$cds$subfactor, mean)
  p_circs[facet_order, "rho"] <- mean_cds[facet_order] + subradius
  rm(mean_cds)
  p_circs$phi <- c(0, 2 * pi / cplx * c(1:cplx)) + rotate
  p_circs$phi[p_circs$phi > 2 * pi] <-
    p_circs$phi[p_circs$phi > 2 * pi] - 2 * pi

  # note: polar to carthesian works like this:
  # x = cos(phi) * rho
  # y = sin(phi) * rho
  c_circs <- p_circs
  # rounded values to decrease display length
  c_circs[ ,1] <- round(cos(p_circs$phi) * p_circs$rho, digits = 7)
  c_circs[ ,2] <- round(sin(p_circs$phi) * p_circs$rho, digits = 7)
  names(c_circs) <- c("x","y","radius")
  row.names(c_circs)[1] <- ""


  ## axes ---------------------------

  p_axes <- data.frame(rho0 = rep(0, cplx),
                       rho1 = NA,
                       rho2 = NA,
                       rho3 = NA,
                       phi = NA)
  row.names(p_axes) <- c(facet_order)
  p_axes$phi <- utils::tail(p_circs$phi, cplx)
  p_axes$rho1 <- utils::tail(p_circs$rho, cplx) - subradius
  p_axes$rho2 <- p_axes$rho1 + 2 * subradius
  p_axes$rho3 <- rep(max(p_circs$radius))

  c_axes <- data.frame(x0 = rep(NA, cplx), y0 = NA,
                       x1 = NA, y1 = NA,
                       x2 = NA, y2 = NA,
                       x3 = NA, y3 = NA)
  row.names(c_axes) <- c(colnames(data$cors))
  c_axes$x0 <- round(cos(p_axes$phi) * p_axes$rho0, digits = 7)
  c_axes$x1 <- round(cos(p_axes$phi) * p_axes$rho1, digits = 7)
  c_axes$x2 <- round(cos(p_axes$phi) * p_axes$rho2, digits = 7)
  c_axes$x3 <- round(cos(p_axes$phi) * p_axes$rho3, digits = 7)
  c_axes$y0 <- round(sin(p_axes$phi) * p_axes$rho0, digits = 7)
  c_axes$y1 <- round(sin(p_axes$phi) * p_axes$rho1, digits = 7)
  c_axes$y2 <- round(sin(p_axes$phi) * p_axes$rho2, digits = 7)
  c_axes$y3 <- round(sin(p_axes$phi) * p_axes$rho3, digits = 7)

  axis_tick <- data.frame(rho = tick, phi = NA, x = NA, y = NA)
  axis_tick$phi <- min(p_circs$phi) + (pi + 2 * pi * rotate_tick_label) / cplx
  axis_tick$x <- round(cos(axis_tick$phi) * axis_tick$rho, digits = 7)
  axis_tick$y <- round(sin(axis_tick$phi) * axis_tick$rho, digits = 7)


  ## test label ---------------------

  # default test label guesses where free space is
  # (next to lowest center distance)
  test_label <- data.frame(x = NA,
                      y = NA,
                      label = row.names(p_circs)[1],
                      phi = NA,
                      rho = NA)
  cs <- p_circs[-1, ]
  test_label$phi <- cs[which.min(cs$rho), "phi"] - pi / cplx + rotate_test_label
  rm(cs)
  test_label$rho <- dist_test_label * max(p_circs$radius)
  test_label$x <- round(cos(test_label$phi) * test_label$rho, digits = 7)
  test_label$y <- round(sin(test_label$phi) * test_label$rho, digits = 7)


  ## correlations -------------------

  n <- cplx * (cplx - 1)
  cors <- data.frame(x = rep(NA, n),
                     y = NA,
                     V1 = NA,
                     V2 = NA,
                     label = NA,
                     xnew = NA,
                     ynew = NA)

  a <- facet_order
  a <- c(a, a[1])
  b <- NULL
  for (k in 1:cplx) {
    b <- c(b, a[-c(1, cplx + 1)])
    a <- a[-1]
    a <- c(a, a[1])
  }
  cors$V1 <- b
  cors$V2 <- unlist(lapply(facet_order, FUN = rep, times = cplx - 1))

  for (k in 1:n) {
    cors$label[k] <- data$cors[cors$V1[k], cors$V2[k]]
  }
  cors$label <- as.character(cors$label)
  # exclude leading 0's for aesthetic reasons
  cors$label[cors$label != 1 & cors$label != 0] <- substr(cors$label, 2, 4)

  cors$x <- c_circs[cors$V2, "x"]
  cors$y <- c_circs[cors$V2, "y"]

  # scatter labels for readability, position indicates partner variable
  scatter <- rep(seq(from = (-pi + 2 * pi / cplx) / 2,
                     to   = (pi  - 2 * pi / cplx) / 2,
                     by   = (pi  - 2 * pi / cplx) / (cplx - 2)),
                 cplx)
  rho <- p_circs[cors$V2, "radius"] * .75
  phi <- p_circs[cors$V2, "phi"]
  cors$xnew <- cors$x + round(cos(phi + pi + scatter), digits = 7) * rho
  cors$ynew <- cors$y + round(sin(phi + pi + scatter), digits = 7) * rho
  cors$x <- cors$xnew
  cors$y <- cors$ynew
  cors[6:7] <- list(NULL)


  # return ---------------------------------------------------------------------

  coord <- list(p_circs    = p_circs,
                c_circs    = c_circs,
                p_axes     = p_axes,
                c_axes     = c_axes,
                axis_tick  = axis_tick,
                test_label = test_label,
                cors       = cors)

  return(coord)
}
