#' Input Excel Factor
#'
#' Reads factor loadings and latent correlations from an excel file.
#'
#' @param file character; filename of the excel file
#'
#' @details Helper function of \code{\link{input_excel}}.
#'
#' @return list containing formatted data including center distances for
#'   \code{\link{item_chart}}, \code{\link{facet_chart}}
#' @seealso \code{\link{input_excel}}
input_excel_factor <- function (file) {


  # file reading ---------------------------------------------------------------

  # excel sheets 1 and 2 as tibbles
  sheet1 <- readxl::read_excel(file, sheet = 1, col_names = T)
  sheet2 <- readxl::read_excel(file, sheet = 2, col_names = T)


  # checking and recalculating -------------------------------------------------

  ## factor loadings ----------------

  # checking for factor loadings below .1
  bad <- min(c(sheet1$factor_loading, sheet1$subfactor_loading))
  bad <- bad < 0.1
  if (bad) warning ("At least one factor loading set to minimum of 0.1")
  sheet1$factor_loading[sheet1$factor_loading < .1] <- .1
  sheet1$subfactor_loading[sheet1$subfactor_loading < .1] <- .1


  ## center distances ---------------

  # calculating the center distances
  cds <- data.frame(
    factor = sheet1$factor,
    subfactor = sheet1$subfactor,
    item = as.factor(sheet1$item),
    cd = sheet1$subfactor_loading ^ 2 / sheet1$factor_loading ^ 2 - 1,
    mean_cd = NA)

  # checking for negative center distances
  bad <- min(cds$cd)
  bad <- bad < 0
  if (bad) warning ("At least one negative center distance adjusted to 0")
  cds$cd[cds$cd < 0] <- 0

  # calculating mean center distances of facets
  mean_cds <- lapply(split(cds, cds$subfactor),
                     function (x) x$mean_cd <- mean(x$cd))
  cds$mean_cd <- as.numeric(mean_cds[cds$subfactor])


  ## model parameters ---------------

  # number of facets
  pars <- list(cplx = length(levels(cds$subfactor)))


  ## correlations -------------------

  # subfactor correlation matrix
  cors <- apply(as.matrix(sheet2)[ ,-1], c(1,2), as.numeric)
  row.names(cors) <- colnames(cors)


  # return ---------------------------------------------------------------------

  mydata <- list(cds = cds,
                 cors = cors,
                 pars = pars)

  return(mydata)
}
