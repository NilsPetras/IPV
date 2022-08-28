#' Is IPV
#'
#' @param x object
#'
#' @return logical, checks if the class of the object is "IPV"
#' @export
is.IPV <- function(x) {
  inherits(x, "IPV")
}
