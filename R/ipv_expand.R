#' IPV expand
#'
#' Helper function to expand estimates of simple IPV to full sized object of
#' class "IPV"
#'
#' @param est list; estimates including center distances
#' @param est_raw list; raw estimates
#'
#' @return object of class "IPV"
ipv_expand <- function(est, est_raw) {

  # expand to object of class "IPV"
  y <- list(
    est = list(
      global = est,
      tests = list(est)),
    est_raw = list(
      global = est_raw,
      tests = list(est_raw)),
    xarrow = NA)
  names(y$est$tests) <- names(y$est_raw$tests) <-
    y$est$global$cds$factor[1]
  class(y) <- c("IPV", "list")

  # clean up the placeholder on the non-existing global level
  y$est$global$cds[ ,c("cd", "mean_cd", "aggregate_cd")] <- 0
  y$est_raw$global$fls$subfactor_loading <-
    y$est_raw$global$fls$factor_loading
  y$est$global$cors <- matrix(1)
  row.names(y$est$global$cors) <- colnames(y$est$global$cors) <-
    y$est$global$cds$factor[1]
  y$est_raw$global$cors <- y$est$global$cors
  y$est$global$cds$subfactor <- y$est_raw$global$fls$subfactor <-
    y$est$global$cds$factor
  y$est$global$cds$item <- y$est_raw$global$fls$item <-
    paste(
      y$est$global$cds$factor,
      y$est$global$cds$item,
      sep = ".")

  return(y)
}
