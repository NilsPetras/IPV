#' New IPV
#'
#' Create empty structure of class IPV
#' @param xarrow logical; should the element xarrow be included?; defaults to TRUE
#'
#' @return IPV; empty IPV structure
#' @export
new_IPV <- function(xarrow = TRUE) {
  x <- list(
    est = list(
      global = list(
        cds = data.frame(
          factor = factor(NA),
          subfactor = factor(NA),
          item = factor(NA),
          cd = numeric(1),
          mean_cd = numeric(1),
          aggregate_cd = numeric(1)),
        cors = diag(3)),
      tests = list(
        mytest = list(
          cds = data.frame(
            factor = factor(NA),
            subfactor = factor(NA),
            item = factor(NA),
            cd = numeric(1),
            mean_cd = numeric(1),
            aggregate_cd = numeric(1)),
          cors = diag(3)))),
  est_raw = list(
    global = list(
      fls = data.frame(
        factor = factor(NA),
        subfactor = factor(NA),
        item = factor(NA),
        factor_loading = numeric(1),
        subfactor_loading = numeric(1)),
      cors = diag(3)),
    tests = list(
      mytest = list(
        fls = data.frame(
          factor = factor(NA),
          subfactor = factor(NA),
          item = factor(NA),
          factor_loading = numeric(1),
          subfactor_loading = numeric(1)),
        cors = diag(3)))))


  if(!is.null(xarrow)) {
    x$xarrow <- data.frame(
      test1 = character(1),
      facet1 = character(1),
      test2 = character(1),
      facet2 = character(1),
      value = character(1))
  } else {
    x$xarrow <- NA
  }

  class(x) <- c("IPV", "list")

  return(x)
}

