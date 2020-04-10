#' Item Chart
#'
#' Creates an item chart, showing the items of a test arranged by facets.
#'
#' @param data SEM estimates in the appropriate format, given by the input
#'   functions.
#' @param facet_order character; vector of facet names in desired order
#'   (counter-clockwise); defaults to NULL, in which case the order is based on
#'   the correlation matrix columns in 'data'.
#' @param file_name character; name of the file to save. Supported formats are:
#'   "pdf" (highest quality and smallest file size), "png", "jpeg"; defaults to "none".
#' @param size integer; changes the size of most chart objects simultaneously.
#' @param font character; text font, use extrafonts to access additional fonts;
#'   defaults to "sans", which is "Helvetica".
#' @param rotate_radians integer; radian angle to rotate the chart
#'   counter-clockwise by; use fractions of pi (e.g. pi/2 = 90 degrees).
#' @param rotate_degrees integer; angle in degrees to rotate the chart
#'   counter-clockwise by.
#' @param grid_limit integer; upper limit to which the grid lines should be
#'   drawn; defaults to 0, in which case an appropriate value is estimated.
#' @param file_width integer; file width in inches; defaults to 12.
#' @param file_height integer; file height in inches; defaults to 10.
#' @param dpi integer; resolution in dots per inch for "png" and "jpeg" files;
#'   defaults to 500.
#' @param color first accent color; defaults to "black".
#' @param color2 second accent color; defaults to "black".
#' @param fade_axes integer; brightness of the gray tone of the axes between 0 =
#'   "black" and 100 = "white" in steps of 1; defaults to 50.
#' @param fade_grid_major integer; brightness of the gray tone of the major grid
#'   lines between 0 = "black" and 100 = "white" in steps of 1; defaults to 15.
#' @param fade_grid_minor integer; brightness of the gray tone of the minor grid
#'   lines between 0 = "black" and 100 = "white" in steps of 1; defaults to 65.
#' @param dodge integer; horizontal outward dodge of facet labels relative to
#'   default.
#' @param dist_test_label integer; position of the test label relative to the
#'   surrounding circle; defaults to .5, in which case the test label is
#'   displayed halfway from the center to the surrounding circle.
#' @param rotate_test_label_radians integer; radian angle to rotate the test
#'   label counter-clockwise by; use fractions of pi (e.g. pi/2 = 90 degrees).
#' @param rotate_test_label_degrees integer; angle in degrees to rotate the test
#'   label counter-clockwise by.
#' @param width_items integer; item bar width relative to default.
#' @param length_items integer; item bar length relative to default.
#' @param length_ratio_items integer; relative item bar length; defaults to 1.5.
#' @param title character; overall chart title; defaults to NULL.
#' @param size_title integer; title font size relative to default.
#' @param size_test_label integer; test label font size relative to default.
#' @param size_facet_labels integer; facet label font size relative to default.
#' @param width_axes integer; radial axis width relative to default.
#' @param size_arrow_heads integer; arrow head size relative to default.
#' @param width_grid integer; grid line width relative to default.
#' @param size_tick_label integer; axis tick label font size relative to
#'   default.
#'
#' @return Object of the class "ggplot" and, by default, the same object saved
#'   as a file.
#'
#' @details When changing the size of objects, consider the \code{size}
#'   parameter first and make specific adjustments with the other \code{size_}
#'   and \code{width_} parameters after.
#'
#'   To better display overlapping item values, change the width of the item
#'   bars, or set the accent colors to different values, or change the ratio of
#'   item lengths.
#'
#'   Pdf files will be vector based and can be scaled arbitrarily. For other
#'   formats use \code{file_width}, \code{file_height}, and \code{dpi} to avoid
#'   later rescaling and loss of quality.
#'
#'   Consider adding title and caption in your typesetting software (LaTeX, MS
#'   Word, ...), not here. The option to add a title is only a quick and dirty
#'   shurtcut. It reduces chart size and is inflexible. Adding the title
#'   manually will provide additional options, but requires you to save to a
#'   file manually. To manually add a title or caption use
#'   \code{\link[ggplot2]{labs}}.
#'
#' @seealso \code{\link{facet_chart}} \code{\link{nested_chart}}
#'
#' @examples
#' # as simple as that
#' item_chart(SMTQ)
#'
#' @export
item_chart <- function(
  data,
  facet_order = NULL,
  file_name = "none",
  size = 1,
  font = "sans",
  rotate_radians = 0,
  rotate_degrees = 0,
  grid_limit = 0,
  file_width = 12,
  file_height = 10,
  dpi = 500,
  color = "black",
  color2 = "black",
  fade_axes = 50,
  fade_grid_major = 15,
  fade_grid_minor = 65,
  dodge = 1,
  dist_test_label = .5,
  rotate_test_label_radians = 0,
  rotate_test_label_degrees = 0,
  width_items = 1,
  length_items = 1,
  length_ratio_items = 1.5,
  title = NULL,
  size_title = 1,
  size_tick_label = 1,
  size_test_label = 1,
  size_facet_labels = 1,
  width_axes = 1,
  size_arrow_heads = 1,
  width_grid = 1){

  coord <- coord_items(
    data = data,
    facet_order = facet_order,
    rotate_radians = rotate_radians,
    rotate_degrees = rotate_degrees,
    grid_limit = grid_limit,
    dodge = dodge,
    width_items = width_items,
    length_items = length_items,
    length_ratio_items = length_ratio_items,
    dist_test_label = dist_test_label,
    rotate_test_label_radians =rotate_test_label_radians,
    rotate_test_label_degrees = rotate_test_label_degrees)

  myipv <- plot_items(
    coord = coord,
    size = size,
    file_name = file_name,
    file_width = file_width,
    file_height = file_height,
    dpi = dpi,
    color = color,
    color2 = color2,
    fade_axes = fade_axes,
    fade_grid_major = fade_grid_major,
    fade_grid_minor = fade_grid_minor,
    font = font,
    title = title,
    size_title = size_title,
    size_tick_label = size_tick_label,
    size_test_label = size_test_label,
    size_facet_labels = size_facet_labels,
    width_axes = width_axes,
    size_arrow_heads = size_arrow_heads,
    width_items = width_items,
    width_grid = width_grid)

  return(myipv)
}



#' Plot Items
#'
#' Generates an item chart from coordinates.
#'
#' @param coord list generated by \code{\link{coord_items}} or
#'   \code{\link{coord_nested}}.
#' @param size integer; changes the size of most chart objects simultaneously.
#' @param file_name character; name of the file to save. Supported formats are:
#'   "pdf" (highest quality and smallest file size), "png", "jpeg"; defaults to "none".
#' @param file_width integer; file width in inches; defaults to 12.
#' @param file_height integer; file height in inches; defaults to 10.
#' @param dpi integer; resolution in dots per inch for "png" and "jpeg" files;
#'   defaults to 500.
#' @param font character; text font, use extrafonts to access additional fonts;
#'   defaults to "sans", which is "Helvetica".
#' @param color first accent color; defaults to "black".
#' @param color2 second accent color; defaults to "black".
#' @param fade_axes integer; brightness of the gray tone of the axes between 0 =
#'   "black" and 100 = "white" in steps of 1; defaults to 50.
#' @param fade_grid_major integer; brightness of the gray tone of the major grid
#'   lines between 0 = "black" and 100 = "white" in steps of 1; defaults to 15.
#' @param fade_grid_minor integer; brightness of the gray tone of the minor grid
#'   lines between 0 = "black" and 100 = "white" in steps of 1; defaults to 65.
#' @param title character; overall chart title; defaults to NULL.
#' @param size_title integer; title font size relative to default.
#' @param size_test_label integer; test font size relative to default.
#' @param size_facet_labels integer; facet font size relative to default.
#' @param width_axes integer; radial axis width relative to default.
#' @param size_arrow_heads integer; arrow head size relative to default.
#' @param width_items integer; item bar width relative to default.
#' @param width_grid integer; grid line width relative to default.
#' @param size_tick_label integer; axis tick label font size relative to
#'   default.
#'
#' @details Use \code{\link{item_chart}} to create item charts.
#'
#' @return Object of the class "ggplot".
#'
#' @seealso \code{\link{coord_items}} \code{\link{item_chart}}
plot_items <- function (
  coord,
  size = 1,
  file_name = "none",
  file_width = 12,
  file_height = 10,
  dpi = 500,
  color = "black",
  color2 = "black",
  fade_axes = 50,
  fade_grid_major = 15,
  fade_grid_minor = 65,
  font = "sans",
  title = NULL,
  size_title = 1,
  size_tick_label = 1,
  size_test_label = 1,
  size_facet_labels = 1,
  width_axes = 1,
  size_arrow_heads = 1,
  width_items = 1,
  width_grid = 1) {


  # preparation ----------------------------------------------------------------

  # some calculations are not possible within aes_string(), so aesthetics are
  # prepared here
  axis_labels <- row.names(coord$c_axes)


  # chart ----------------------------------------------------------------------

  myipv <- ggplot2::ggplot(coord$c_axes) + # x has placeholder value


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

  # ordered from bottom to top for correct overlap

  # center dot
  ggplot2::geom_point(
    ggplot2::aes(x = 0, y = 0),
    color = paste("gray", fade_axes, sep = ""),
    size = .8 * size * width_axes) +

    # minor grid
    ggforce::geom_circle(
      data = coord$grid[coord$grid$alpha == .5, ],
      ggplot2::aes_string(x0 = "x", y0 = "y", r = "r"),
      color = paste("gray", fade_grid_minor, sep = ""),
      linetype = "dotted",
      size = size * min(size, .5) * width_grid) +

    # major grid
    ggforce::geom_circle(
      data = coord$grid[coord$grid$alpha == 1, ],
      ggplot2::aes_string(x0 = "x", y0 = "y", r = "r"),
      color = paste("gray", fade_grid_major, sep = ""),
      linetype = "dotted",
      size = size * min(size, .5) * width_grid) +

    # axes
    ggplot2::geom_segment(
      data = coord$c_axes,
      ggplot2::aes_string(x = 0, y = 0, xend = "x", yend = "y"),
      color = paste("gray", fade_axes, sep = ""),
      arrow = ggplot2::arrow(
        ends = "last",
        length = ggplot2::unit(.01 * size * size_arrow_heads, "native"),
        type = "closed"),
      size = .5 * size * width_axes) +

    # items 2
    ggplot2::geom_segment(
      data = coord$items[coord$items$length == max(coord$items$length), ],
      ggplot2::aes_string(x = "x1", y = "y1", xend = "x2", yend = "y2"),
      size = 1.5 * width_items,
      color = color2) +

    # items 1
    ggplot2::geom_segment(
      data = coord$items[coord$items$length == min(coord$items$length), ],
      ggplot2::aes_string(x = "x1", y = "y1", xend = "x2", yend = "y2"),
      size = 1.5 * width_items,
      color = color) +

    # test label
    ggplot2::geom_text(
      data = coord$test_label,
      ggplot2::aes_string(x = "x", y = "y", label = "label"),
      family = font,
      size = 8 * size * size_test_label,
      color = color,
      fontface = "bold") +

    # axis labels
    ggplot2::geom_text(
      ggplot2::aes_string(x = "xlabel", y = "ylabel", label = "axis_labels"),
      family = font,
      size = 6 * size * size_facet_labels,
      hjust = "inward") +

    # tick label
    ggplot2::geom_text(
      data = coord$axis_tick,
      ggplot2::aes_string(x = "x", y = "y", label = "label"),
      angle = (coord$axis_tick$phi - pi / 48 - pi / 2) * 180 / pi,
      family = font,
      size = 4 * size * size_tick_label,
      color = "gray20")


  ## optional layers ----------------

  # title
  if (!is.null(title)) {
    myipv <- myipv +
      ggplot2::ggtitle(label = title)
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
