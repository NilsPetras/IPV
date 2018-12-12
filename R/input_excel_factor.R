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

  ## missing values -----------------

  # checking for wrong number of columns
  if (length(sheet1) != 5) stop ("Wrong number of columns")

  # checking for missing values
  sheet1 <- na.fail(sheet1)


  ## factor loadings ----------------

  # factor loadings < 0
  bad <- min(c(sheet1$factor_loading, sheet1$subfactor_loading))
  bad <- bad < 0
  if (bad) stop ("Negative factor loading")

  # factor loadings < .1
  bad <- min(c(sheet1$factor_loading, sheet1$subfactor_loading))
  bad <- bad < 0.1
  if (bad) warning ("At least one factor loading set to minimum of 0.1")
  sheet1$factor_loading[sheet1$factor_loading < .1] <- .1
  sheet1$subfactor_loading[sheet1$subfactor_loading < .1] <- .1

  # factor loadings > 1
  bad <- max(c(sheet1$factor_loading, sheet1$subfactor_loading))
  bad <- bad > 1
  if (bad) warning ("At least one factor loading > 1, check for correctness")


  ## names

  # checking for multiple factor names
  if (length(levels(as.factor(sheet1$factor))) > 1) stop (
    "The column \"factor\" contains more than one factor")

  # checking for doubled item names within subfactor
  a <- split(sheet1, f = sheet1$subfactor)
  lapply(a, function(x) if (length(levels(as.factor(x$item))) <
                            length(x$item)
                            ) stop ("Item name reoccuring within subfactor"))


  # subfactor with only one item
  a <- table(as.factor(sheet1$subfactor))
  bad <- names(which(a == min(a)))

  if (min(table(as.factor(sheet1$subfactor))) < 2) warning (
    c("These subfactors only refer to a single item: ", bad))

  # checking for wrong column names
  if (!isTRUE(all.equal(names(sheet1), c("factor",
                                        "subfactor",
                                        "item",
                                        "factor_loading",
                                        "subfactor_loading")))
      ) stop ("Wrong or missing column names")


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


  ## correlations -------------------

  # checking for wrong row or column names or order
  if (!isTRUE(all.equal(names(sheet2)[-1], sheet2[[1]]))) stop (
    "Wrong names or order in correlation matrix"
  )

  # checking for unknown or missing variable names
  if (!isTRUE(all.equal(levels(as.factor(sheet1$subfactor)),
                        levels(as.factor(sheet2[[1]]))))
      ) stop ("Variables in correlation matrix do not match subfactors")

  # checking for missing values
  sheet2 <- na.fail(sheet2)

  # subfactor correlation matrix
  cors <- apply(as.matrix(sheet2)[ ,-1], c(1,2), as.numeric)
  row.names(cors) <- colnames(cors)

  # checking if any correlation value out of bounds
  if (any(cors > 1)) stop ("Correlation > 1")
  if (any(cors < -1)) stop ("Correlation < -1")

  # checking if values in matrix are symmetrical (Hermitian)
  if (!isSymmetric(cors)) stop ("Correlation matrix asymmetrical")

  # checking if main diagonal values are 1
  if (any(diag(cors)!=1)) warning (
    "Main diagonal in correlation matrix set to 1")
  diag(cors) <- 1


  # return ---------------------------------------------------------------------

  mydata <- list(cds = cds,
                 cors = cors)

  return(mydata)
}
