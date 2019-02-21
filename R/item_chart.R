#' Item Chart
#'
#' Creates an item chart, showing the items of a test arranged by facets.
#'
#' @param data data in the appropriate format, given by the input functions.
#' @param file_name character; name of the file to save. Supported formats are:
#'   "pdf" (highest quality and smallest file size), "png", "jpeg". Use "none"
#'   to suppress file output; defaults to "myipv.pdf".
#' @param size integer; changes the size of most chart objects simultaneously.
#' @param font character; text font, use extrafonts to access additional fonts;
#'   defaults to "sans", which is "Helvetica".
#' @param rotate_radians integer; radian angle to rotate the chart
#'   counter-clockwise by; use fractions of pi (e.g. pi/2 = 90 degrees).
#' @param rotate_degrees integer; angle in degrees to rotate the chart
#'   counter-clockwise by.
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
#' @param rotate_title_radians integer; radian angle to rotate the global label
#'   counter-clockwise by; use fractions of pi (e.g. pi/2 = 90 degrees).
#' @param rotate_title_degrees integer; angle in degrees to rotate the global
#'   label counter-clockwise by.
#' @param width_items integer; item bar width relative to default.
#' @param length_items integer; item bar length relative to default.
#' @param length_ratio_items integer; relative item bar length; defaults to 1.5.
#' @param size_test_label integer; test font size relative to default.
#' @param size_facet_labels integer; facet font size relative to default.
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
#' @seealso \code{\link{facet_chart}} \code{\link{nested_chart}}
#'
#' @examples
#' # as simple as that
#' item_chart(SMTQ)
#'
#' @export
item_chart <- function(
  data,
  file_name = "myipv.pdf",
  size = 1,
  font = "sans",
  rotate_radians = 0,
  rotate_degrees = 0,
  file_width = 12,
  file_height = 10,
  dpi = 500,
  color = "black",
  color2 = "black",
  fade_axes = 50,
  fade_grid_major = 15,
  fade_grid_minor = 65,
  dodge = 1,
  rotate_title_radians = 0,
  rotate_title_degrees = 0,
  width_items = 1,
  length_items = 1,
  length_ratio_items = 1.5,
  size_tick_label = 1,
  size_test_label = 1,
  size_facet_labels = 1,
  width_axes = 1,
  size_arrow_heads = 1,
  width_grid = 1){

  coord <- coord_items(
    data = data,
    rotate_radians = rotate_radians,
    rotate_degrees = rotate_degrees,
    dodge = dodge,
    width_items = width_items,
    length_items = length_items,
    length_ratio_items = length_ratio_items,
    rotate_title_radians =rotate_title_radians,
    rotate_title_degrees = rotate_title_degrees)

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
    size_tick_label = size_tick_label,
    size_test_label = size_test_label,
    size_facet_labels = size_facet_labels,
    width_axes = width_axes,
    size_arrow_heads = size_arrow_heads,
    width_items = width_items,
    width_grid = width_grid)

  return(myipv)
}
