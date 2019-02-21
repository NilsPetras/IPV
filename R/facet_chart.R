#' Facet Chart
#'
#' Creates a facet chart, showing the facets of a test.
#'
#' @param data data in the appropriate format, given by the input functions.
#' @param subradius integer; same unit as center distances; radius of the facet
#'   circles; defaults to 0, in which case an appropriate value is estimated.
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
#' @param file_width integer; file width in inches; defaults to 10.
#' @param file_height integer; file height in inches; defaults to 10.
#' @param dpi integer; resolution in dots per inch for "png" and "jpeg" files;
#'   defaults to 500.
#' @param color accent color; defaults to "black".
#' @param fade integer; brightness of the gray tones between 0 = "black" and 100
#'   = "white" in steps of 1; defaults to 85.
#' @param tick numeric; axis tick position; defaults to 0, in which case an
#'   appropriate value is estimated.
#' @param cor_labels logical; if \code{TRUE}, shows latent correlations between
#'   facets; defaults to \code{TRUE}.
#' @param rotate_test_label_radians integer; radian angle to rotate the test label
#'   counter-clockwise by; use fractions of pi (e.g. pi/2 = 90 degrees).
#' @param rotate_test_label_degrees integer; angle in degrees to rotate the global
#'   label counter-clockwise by.
#' @param size_test_label integer; test font size relative to default.
#' @param size_facet_labels integer; facet font size relative to default.
#' @param width_axes integer; radial axis width relative to default.
#' @param width_circles integer; facet circle outline width relative to default.
#' @param width_tick integer; axis tick line width relative to default.
#' @param size_tick_label integer; axis tick font size relative to default.
#' @param size_cor_labels integer; correlation font size relative to default.
#'
#' @return Object of the class "ggplot" and, by default, the same object saved
#'   as a file.
#'
#' @seealso \code{\link{item_chart}} \code{\link{nested_chart}}
#'
#' @examples
#' # as simple as that:
#' facet_chart(SMTQ)
#'
#' @export
facet_chart <- function(
  data,
  subradius = 0,
  file_name = "myipv.pdf",
  size = 1,
  font = "sans",
  rotate_radians = 0,
  rotate_degrees = 0,
  file_width = 10,
  file_height = 10,
  dpi = 500,
  color = "black",
  fade = 85,
  tick = 0,
  cor_labels = TRUE,
  rotate_test_label_radians = 0,
  rotate_test_label_degrees = 0,
  size_cor_labels = 1,
  size_test_label = 1,
  size_facet_labels = 1,
  width_axes = 1,
  width_circles = 1,
  width_tick = 1,
  size_tick_label = 1){

  coord <- coord_facets(
    data = data,
    subradius = subradius,
    tick = tick,
    rotate_radians = rotate_radians,
    rotate_degrees = rotate_degrees,
    rotate_test_label_radians =rotate_test_label_radians,
    rotate_test_label_degrees = rotate_test_label_degrees)

  myipv <- plot_facets(
    coord = coord,
    size = size,
    file_name = file_name,
    file_width = file_width,
    file_height = file_height,
    dpi = dpi,
    color = color,
    fade = fade,
    font = font,
    cor_labels = cor_labels,
    size_cor_labels = size_cor_labels,
    size_test_label = size_test_label,
    size_facet_labels = size_facet_labels,
    width_axes = width_axes,
    width_circles = width_circles,
    width_tick = width_tick,
    size_tick_label = size_tick_label)

  return(myipv)
}
