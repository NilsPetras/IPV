#' Input Excel
#'
#' Reads excel files containing factor loadings and latent correlations for IPV
#' charts.
#'
#' @param global character; name of the excel file containing factor loadings
#'   from the global level and the test level, and latent correlations from the
#'   test level.
#' @param tests character; name(s) of the excel file(s) containing factor
#'   loadings from the test level and the facet level, and latent correlations
#'   from the facet level.
#'
#' @details Note that the excel files need a very specific structure. Use the
#'   example files as templates.
#'
#'   The \code{global} argument defaults to NULL. This allows to only use the
#'   \code{tests} argument, resulting in a simple model with one test and its
#'   facets.
#'
#' @return List containing formatted data including center distances for
#'   \code{\link{item_chart}}, \code{\link{facet_chart}}, and
#'   \code{\link{nested_chart}}.
#'
#' @examples
#' # read data for a simple model by ignoring the "global" parameter of input_excel
#' single_file <- system.file("extdata", "DSSEI.xlsx", package = "IPV", mustWork = TRUE)
#' x <- input_excel(tests = single_file)
#'
#' # read data for a nested model
#' # note that the data needs to be split into several excel files as in the example
#' global <- system.file("extdata", "IPV_global.xlsx", package = "IPV", mustWork = TRUE)
#' tests <- c(system.file("extdata", "IPV_DSSEI.xlsx", package = "IPV", mustWork = TRUE),
#'              system.file("extdata", "IPV_SMTQ.xlsx", package = "IPV", mustWork = TRUE),
#'              system.file("extdata", "IPV_RSES.xlsx", package = "IPV", mustWork = TRUE))
#' x <- input_excel(global = global, tests = tests)
#'
#' @export
input_excel <- function(global = NULL, tests){


  # the helper function 'input_excel_factor' below is applied to both the global
  # and the nested scale

  if (is.null(global)) { # simple case: just run the helper function
    mydata <- input_excel_factor(tests)
  } else { # nested case

    global_input <- input_excel_factor(global)


    # check if all files are given
    if (length(levels(global_input$cds$subfactor)) != length(tests)
        ) stop ("Missing file")

    tests_input <- lapply(tests, input_excel_factor)

    # check matches between global and test level
      # factor names matching
    x <- lapply(tests_input, `[[`, 1)
    x <- sort(as.character(unlist(lapply(x, `[[`, "factor"))))
    if (!isTRUE(all.equal(sort(as.character(global_input$cds$subfactor)), x))
        ) stop ("Factor name or item per factor count mismatch between global
                and tests")

      # item names and number of items matching
    x <- lapply(tests_input, `[[`, 1)
    x <- sort(as.character(unlist(lapply(x, `[[`, "item"))))
    if (!isTRUE(all.equal(sort(as.character(global_input$cds$item)), x))
        ) stop ("Number of items or item name mismatch between global and
                tests")

    # including the factor name in the item name to distinguish between items
    # from different tests but with the same name
    global_input$cds$item <- paste(global_input$cds$subfactor,
                                   global_input$cds$item,
                                   sep = ".")

    for (i in 1:length(colnames(global_input$cors))) {
      names(tests_input)[i] <- levels(tests_input[[c(i, 1)]]$factor)
    }

    mydata <- list(global = global_input, tests = tests_input)
  }

  return(mydata)
}



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

  # excel sheet 1 contains the center distances, 2 contains the correlations
  sheet1 <- readxl::read_excel(file, sheet = 1, col_names = T)
  sheet2 <- readxl::read_excel(file, sheet = 2, col_names = T)


  # checking and recalculating -------------------------------------------------

  # running a ton of checks to avoid identifiable typos and other mistakes

  ## missing values -----------------

  # checking for wrong number of columns
  if (length(sheet1) != 5) stop ("Wrong number of columns")

  # checking for missing values
  sheet1 <- stats::na.fail(sheet1)


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


  ## names --------------------------

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

  cds <- data.frame(
    factor = sheet1$factor,
    subfactor = sheet1$subfactor,
    item = as.factor(sheet1$item),
    cd = sheet1$subfactor_loading ^ 2 / sheet1$factor_loading ^ 2 - 1,
    mean_cd = NA)

  # negative center distances are adjusted to zero for chart clarity
  bad <- min(cds$cd)
  bad <- bad < 0
  if (bad) message ("Negative center distance adjusted to 0")
  cds$cd[cds$cd < 0] <- 0

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
  sheet2 <- stats::na.fail(sheet2)

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

