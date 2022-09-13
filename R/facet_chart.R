#' Facet Chart
#'
#' Creates a facet chart, showing the facets of a test.
#'
#' @param data Object of class IPV as created by the function 'ipv_est'
#' @param test character; name of the test to plot; defaults to the first in the
#'   list.
#' @param cd_method character; method to summarize center distances, either
#'   "mean" or "aggregate", see details; defaults to "aggregate".
#' @param facet_order character; vector of facet names in desired order
#'   (counter-clockwise); defaults to NULL, in which case the order is based on
#'   the correlation matrix columns in 'data'.
#' @param subradius integer; same unit as center distances; radius of the facet
#'   circles; defaults to 0, in which case an appropriate value is estimated.
#' @param file_name character; name of the file to save. Supported formats are:
#'   "pdf" (highest quality and smallest file size), "png", "jpeg"; defaults to
#'   "none".
#' @param size integer; changes the size of most chart objects simultaneously.
#' @param font character; text font, use extrafonts to access additional fonts;
#'   defaults to "sans", which is "Helvetica".
#' @param rotate_radians integer; radian angle to rotate the chart
#'   counter-clockwise by; use fractions of pi (e.g. pi/2 = 90 degrees).
#' @param rotate_degrees integer; angle in degrees to rotate the chart
#'   counter-clockwise by.
#' @param zoom_x integer; vector with two values, the edges of the zoomed
#'   section on the x-axis; defaults to NULL.
#' @param zoom_y integer; vector with two values, the edges of the zoomed
#'   section on the y-axis; defaults to NULL.
#' @param file_width integer; file width in inches; defaults to 10.
#' @param file_height integer; file height in inches; defaults to 10.
#' @param dpi integer; resolution in dots per inch for "png" and "jpeg" files;
#'   defaults to 500.
#' @param color accent color; defaults to blue ("#007AD6").
#' @param fade integer; brightness of the gray tones between 0 = "black" and 100
#'   = "white" in steps of 1; defaults to 85.
#' @param tick numeric; axis tick position; defaults to 0, in which case an
#'   appropriate value is estimated.
#' @param rotate_tick_label numeric; number of positions to move the tick label
#'   (counter-clockwise); defaults to 0.
#' @param cor_labels logical; if \code{TRUE}, shows latent correlations between
#'   facets; defaults to \code{TRUE}.
#' @param dist_test_label integer; position of the test label relative to the
#'   surrounding circle; defaults to 2/3, in which case the test label is
#'   displayed 2/3 of the way from the center to the surrounding circle.
#' @param rotate_test_label_radians integer; radian angle to rotate the test
#'   label counter-clockwise by; use fractions of pi (e.g. pi/2 = 90 degrees).
#' @param rotate_test_label_degrees integer; angle in degrees to rotate the
#'   global label counter-clockwise by.
#' @param title character; overall chart title; defaults to NULL.
#' @param size_title integer; title font size relative to default.
#' @param size_test_label integer; test font size relative to default.
#' @param size_facet_labels integer; facet font size relative to default.
#' @param width_axes integer; radial axis width relative to default.
#' @param width_circles integer; facet circle outline width relative to default.
#' @param width_tick integer; axis tick line width relative to default.
#' @param size_tick_label integer; axis tick font size relative to default.
#' @param size_cor_labels integer; correlation font size relative to default.
#' @param size_marker integer; size (in inches) of the value marker  at the
#'   circle border that indicates the center distance, a value of 0 omits the
#'   marker; defaults to .1
#'
#' @details To summarize center distances (\code{cd_method}), the "mean" method
#'   computes the average center distance (compute cds first, summarize across
#'   items second), while the "aggregate" method computes a center distance
#'   based on the sum of the squared loadings (summarize across items first,
#'   compute cds second). "Aggregate" (default) is recommended, because it is
#'   more meaningful in cases with heterogeneous factor loadings, while "mean"
#'   is the originally proposed method.
#'
#'   Pdf files will be vector based and can be scaled arbitrarily. For other
#'   formats use \code{file_width}, \code{file_height}, and \code{dpi} to avoid
#'   later rescaling and loss of quality.
#'
#'   Instead of using screenshots to crop the chart, it is highly recommendable
#'   to use \code{zoom_x} and \code{zoom_y}. This allows for vector-based
#'   graphics quality when showing sections of the chart. With this cropping
#'   method, use \code{file_width} to set the overall size of the file output,
#'   \code{file_height} will automatically adjust to retain the correct aspect
#'   ratio, if both \code{zoom_x} and \code{zoom_y} are provided.
#'
#'   Consider adding title and caption in your typesetting software (LaTeX, MS
#'   Word, ...), not here. The option to add a title is only a quick and dirty
#'   shurtcut. It reduces chart size and is inflexible. Adding the title
#'   manually will provide additional options, but requires you to save to a
#'   file manually. To manually add a title or caption use
#'   \code{\link[ggplot2]{labs}}.
#'
#' @return Object of the class "ggplot".
#'
#' @seealso \code{\link{item_chart}} \code{\link{nested_chart}}
#'
#' @examples
#' # as simple as that:
#' facet_chart(self_confidence, test = "SMTQ")
#'
#' @export
facet_chart <- function(
  data,
  test = NULL,
  cd_method = "aggregate",
  facet_order = NULL,
  subradius = 0,
  file_name = "none",
  size = 1,
  font = "sans",
  rotate_radians = 0,
  rotate_degrees = 0,
  file_width = 10,
  file_height = 10,
  zoom_x = NULL,
  zoom_y = NULL,
  dpi = 500,
  color = "#007AD6",
  fade = 85,
  tick = 0,
  rotate_tick_label = 0,
  cor_labels = TRUE,
  dist_test_label = 2 / 3,
  rotate_test_label_radians = 0,
  rotate_test_label_degrees = 0,
  title = NULL,
  size_title = 1,
  size_cor_labels = 1,
  size_test_label = 1,
  size_facet_labels = 1,
  width_axes = 1,
  width_circles = 1,
  width_tick = 1,
  size_tick_label = 1,
  size_marker = .1){

  if (is.null(test)) {
    test <- names(data$est$tests)[1]
  }
  if (!test %in% names(data$est$tests)) {
    stop(paste("There is no test called ", test, sep = ""))
  }
  data <- data$est$tests[[test]]

  coord <- coord_facets(
    data = data,
    cd_method = cd_method,
    facet_order = facet_order,
    subradius = subradius,
    tick = tick,
    rotate_tick_label,
    rotate_radians = rotate_radians,
    rotate_degrees = rotate_degrees,
    dist_test_label = dist_test_label,
    rotate_test_label_radians =rotate_test_label_radians,
    rotate_test_label_degrees = rotate_test_label_degrees)

  myipv <- plot_facets(
    coord = coord,
    title = title,
    size = size,
    file_name = file_name,
    file_width = file_width,
    file_height = file_height,
    zoom_x = zoom_x,
    zoom_y = zoom_y,
    dpi = dpi,
    color = color,
    fade = fade,
    font = font,
    cor_labels = cor_labels,
    size_cor_labels = size_cor_labels,
    size_title = size_title,
    size_test_label = size_test_label,
    size_facet_labels = size_facet_labels,
    width_axes = width_axes,
    width_circles = width_circles,
    width_tick = width_tick,
    size_tick_label = size_tick_label,
    size_marker = size_marker)

  return(myipv)
}



#' Plot Facets
#'
#' Generates a facet chart from coordinates.
#'
#' @param coord list generated by \code{\link{coord_facets}} or
#'   \code{\link{coord_nested}}.
#' @param title character; overall chart title; defaults to NULL.
#' @param size integer; changes the size of most chart objects simultaneously.
#' @param file_name character; name of the file to save. Supported formats are:
#'   "pdf" (highest quality and smallest file size), "png", "jpeg"; defaults to
#'   "none".
#' @param zoom_x integer; vector with two values, the edges of the zoomed
#'   section on the x-axis; defaults to NULL.
#' @param zoom_y integer; vector with two values, the edges of the zoomed
#'   section on the y-axis; defaults to NULL.
#' @param file_width integer; file width in inches; defaults to 10.
#' @param file_height integer; file height in inches; defaults to 10.
#' @param dpi integer; resolution in dots per inch for "png" and "jpeg" files;
#'   defaults to 500.
#' @param color accent color; defaults to "black".
#' @param fade integer; brightness of the gray tones between 0 = "black" and 100
#'   = "white" in steps of 1; defaults to 85.
#' @param cor_labels logical; if \code{TRUE}, shows latent correlations between
#'   facets; defaults to \code{TRUE}.
#' @param font character; text font, use extrafonts to access additional fonts;
#'   defaults to "sans", which is "Helvetica".
#' @param size_title integer; title font size relative to default.
#' @param size_test_label integer; test font size relative to default.
#' @param size_facet_labels integer; facet font size relative to default.
#' @param width_axes integer; radial axis width relative to default.
#' @param width_circles integer; facet circle outline width relative to default.
#' @param width_tick integer; axis tick line width relative to default.
#' @param size_tick_label integer; axis tick font size relative to default.
#' @param size_cor_labels integer; correlation font size relative to default.
#' @param size_marker integer; size (in inches) of the value marker  at the
#'   circle border that indicates the center distance, a value of 0 omits the
#'   marker; defaults to .1
#'
#' @details Use \code{\link{facet_chart}} to create facet charts.
#'
#' @return Object of the class "ggplot".
#'
#' @seealso \code{\link{coord_facets}} \code{\link{facet_chart}}
plot_facets <- function(
  coord,
  title = NULL,
  size = 1,
  file_name = "none",
  file_width = 10,
  file_height = 10,
  zoom_x = NULL,
  zoom_y = NULL,
  dpi = 500,
  color = "black",
  fade = 85,
  font = "sans",
  cor_labels = TRUE,
  size_title = 1,
  size_cor_labels = 1,
  size_test_label = 1,
  size_facet_labels = 1,
  width_axes = 1,
  width_circles = 1,
  width_tick = 1,
  size_tick_label = 1,
  size_marker = .1){


  # preparation ----------------------------------------------------------------

  if (cor_labels == TRUE) {
    cors <- coord$cors
  } else cors <- NULL

  # some calculations are not possible within aes_string(), so aesthetics are
  # prepared here
  facet_labels <- row.names(coord$c_circs)

  tick <- signif(sqrt((coord$axis_tick$x ^ 2) + (coord$axis_tick$y ^ 2)), 1)
  tick_label_label <- as.character(formatC(tick, format = "fg"))
  tick_label_x <- coord$axis_tick$x +
    0.03 * size * size_tick_label *
    cos(coord$axis_tick$phi) * coord$p_circs[1, "radius"]
  tick_label_y <- coord$axis_tick$y +
    0.03 * size * size_tick_label *
    sin(coord$axis_tick$phi) * coord$p_circs[1, "radius"]

  # aspect ratio correction (to manage zoomed cases)
  if(!is.null(zoom_x) & !is.null(zoom_y)) {
    asp <- diff(zoom_y) / diff(zoom_x)
    file_height <- asp * file_width
  }


  # chart ----------------------------------------------------------------------

  myipv <- ggplot2::ggplot(coord$c_circs) +


    ## initializing -----------------

  ggplot2::coord_fixed() +
    ggplot2::theme(
      axis.line        = ggplot2::element_blank(),
      axis.text.x      = ggplot2::element_blank(),
      axis.text.y      = ggplot2::element_blank(),
      axis.ticks       = ggplot2::element_blank(),
      axis.title.x     = ggplot2::element_blank(),
      axis.title.y     = ggplot2::element_blank(),
      legend.position  = "none",
      panel.background = ggplot2::element_blank(),
      panel.border     = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      plot.background  = ggplot2::element_blank(),
      text             = ggplot2::element_text(size = 16, family = font),
      plot.margin      = ggplot2::margin(0, 0, 0, 0, "in"),
      plot.title       = ggplot2::element_text(
        hjust = .5,
        vjust = -3,
        size = 16 * size * size_title)) +
    ggplot2::aes() +


    ## layers -----------------------

  # ordered from bottom to top, for correct overlap

  # center dot
  ggplot2::geom_point(
    ggplot2::aes(x = 0, y = 0),
    size = 3 * size * width_axes) +

    # axis tick
    ggforce::geom_circle(
      ggplot2::aes(x0 = 0, y0 = 0, r = tick),
      linetype = "dotted",
      size = .5 * min(size, 1) * width_tick) +

    # facet circle background
    ggforce::geom_circle(
      data = coord$c_circs[-1, ],
      ggplot2::aes_string(x0 = "x", y0 = "y", r = "radius"),
      size = .5 * size * width_circles,
      color = color,
      fill = "white") +

    # outer axis segments
    ggplot2::geom_segment(
      data = coord$c_axes,
      ggplot2::aes_string(x = "x2", y = "y2", xend = "x3", yend = "y3"),
      size = .5 * size * width_axes,
      color = paste("gray", fade, sep = "")) +

    # tick label
    ggplot2::geom_text(
      data = coord$axis_tick,
      ggplot2::aes(x = tick_label_x,
                   y = tick_label_y,
                   label = tick_label_label),
      angle = (coord$axis_tick$phi - pi / 48 - pi / 2) * 180 / pi,
      family = font,
      size = 4 * size * size_tick_label) +

    # test circle
    ggforce::geom_circle(
      data = coord$c_circs[1, ],
      ggplot2::aes_string(x0 = "x", y0 = "y", r = "radius"),
      size = .5 * size * width_axes,
      color = paste("gray", fade, sep = "")) +

    # facet circles
    ggforce::geom_circle(
      data = coord$c_circs[-1, ],
      ggplot2::aes_string(x0 = "x", y0 = "y", r = "radius"),
      size = .6 * size * width_circles,
      color = color) +

    # facet labels
    ggplot2::geom_text(
      ggplot2::aes_string(x = "x", y = "y", label = "facet_labels"),
      family = font,
      size = 6 * size * size_facet_labels) +

    # inner axis segments
    ggplot2::geom_segment(
      data = coord$c_axes,
      ggplot2::aes_string(x = "x0", y = "y0", xend = "x1", yend = "y1"),
      size = 1.5 * size * width_axes,
      arrow = ggplot2::arrow(
        angle = 90,
        ends = "last",
        length = ggplot2::unit(size_marker, "inches")),
      color = "black") +

    # test label
    ggplot2::geom_text(
      data = coord$test_label,
      ggplot2::aes_string(x = "x", y = "y", label = "label"),
      family = font,
      size = 8 * size * size_test_label,
      fontface = "bold",
      color = "black")


  ## optional layers ----------------

  # correlations
  if (!is.null(cors)) {
    myipv <- myipv +
      ggplot2::geom_text(
        data = cors,
        ggplot2::aes_string(x = "x", y = "y", label = "label"),
        family = font,
        size = 4 * size * size_cor_labels)
  }

  # title
  if (!is.null(title)) {
    myipv <- myipv +
      ggplot2::ggtitle(label = title)
  }

  # section
  if (!is.null(c(zoom_x, zoom_y))) {
    myipv <- myipv +
      ggplot2::coord_cartesian(xlim = zoom_x, ylim = zoom_y, expand = FALSE)
    if(!is.null(zoom_x) & !is.null(zoom_y) & file_name != "none") {
      message(paste(
        "file_height was set to ",
        signif(asp, 4),
        " times the file_width, to retain the aspect ratio.",
        sep = ""))
    }

  }


  # optional file save ---------------------------------------------------------

  ## .pdf ---------------------------

  if (substring(file_name, nchar(file_name)-3+1) == "pdf") {
    ggplot2::ggsave(file_name,
                    myipv,
                    width = file_width,
                    height = file_height,
                    units = "in",
                    dpi = dpi)
  }


  ## .png ---------------------------

  if (substring(file_name, nchar(file_name)-3+1) == "png") {
    ggplot2::ggsave(file_name,
                    myipv,
                    width = file_width,
                    height = file_height,
                    units = "in",
                    dpi = dpi)
  }


  ## .jpeg --------------------------

  if (substring(file_name, nchar(file_name)-3+1) == "peg") {
    ggplot2::ggsave(file_name,
                    myipv,
                    width = file_width,
                    height = file_height,
                    units = "in",
                    dpi = dpi)
  }


  # return ---------------------------------------------------------------------

  return(myipv)
}
