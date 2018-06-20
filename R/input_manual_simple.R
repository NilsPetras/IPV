#'Input Manual Simple
#'
#'Generates manual data input for a simple model with one test.
#'
#'@param test_name character; the name of the test.
#'@param facet_names character; the names of the facets in correct order.
#'@param items_per_facet integer; number of items per facet in correct order
#'  (determined by facet_names), if all facets have the same number of items a
#'  single number can be used.
#'@param item_names character or integer; the names of the items in correct
#'  order (determined by facet_names); defaults to NULL, in this case the items
#'  will be ennumerated.
#'@param general_loadings integer; vector of the items' loadings on the general
#'  factor (test) in correct order (determined by item_names); defaults to NULL,
#'  in this case complete by hand.
#'@param correlated_loadings integer; vector of the items' loadings on their
#'  correlated factor (facet) in correct order (determined by item_names);
#'  defaults to NULL, in this case complete by hand.
#'@param correlation_matrix matrix containing the latent correlations between
#'  facets, pay attention to the order of rows and columns, which is determined
#'  by facet_names; defaults to NULL, in this case complete by hand.
#'
#'@details Pay attention to the order of facets and items, it has to be coherent
#'  throughout the whole data. facet_names and items_per_facet determine which
#'  facet is listed first and how many items there are listed for that facet.
#'  item_names, general_loadings and correlated_loadings have to match that
#'  order. The correlation matrix uses the order in facet_names for rows and
#'  columns.
#'
#'  Check the output for correctness and complete missing data before continuing
#'  with \code{\link{input_manual_process}}!
#'
#'@return list containing raw data, that needs to be preprocessed using
#'  \code{\link{input_manual_process}}.
#'
#'@seealso \code{\link{input_manual_nested}} \code{\link{input_manual_process}}
#'
#'@examples
#'# these RSES data can also be seen in self_confidence, the example data of
#'# this package
#'mydata <- input_manual_simple(
#'test_name = "RSES",
#'facet_names = c("Ns", "Ps"),
#'items_per_facet = 5,
#'item_names = c(2, 5, 6, 8, 9,
#'               1, 3, 4, 7, 10),
#'general_loadings = c(.5806, .5907, .6179, .5899, .6559,
#'                     .6005, .4932, .4476, .5033, .6431),
#'correlated_loadings = c(.6484, .6011, .6988, .6426, .6914,
#'                        .6422, .5835, .536, .5836, .6791),
#'correlation_matrix = matrix(data = c(1, .69,
#'                                     .69, 1),
#'                            nrow = 2,
#'                            ncol = 2))
#'mydata
#'input_manual_process(mydata)
#'
#'@export
input_manual_simple <- function(
  test_name,
  facet_names,
  items_per_facet,
  item_names = NULL,
  general_loadings = NULL,
  correlated_loadings = NULL,
  correlation_matrix = NULL) {


  # helper variables -----------------------------------------------------------

  cplx <- length(facet_names)


  # initializing object --------------------------------------------------------

  if (length(items_per_facet) == 1) {
    items_per_facet <- rep(items_per_facet, cplx)
  }

  mydata <- list(
    fls = data.frame(factor = as.factor(rep(test_name, sum(items_per_facet))),
                     subfactor = NA,
                     item = NA,
                     factor_loading = NA,
                     subfactor_loading = NA),
    cors = matrix(nrow = cplx, ncol = cplx),
    pars = list(cplx = cplx))


  # factor loadings data frame--------------------------------------------------

  # subfactor names
  subfactor <- NULL
  for (i in 1:cplx) {
    subfactor <- c(subfactor, rep(facet_names[i], items_per_facet[i]))
  }
  mydata$fls$subfactor <- as.factor(subfactor)

  # item names
  if (is.null(item_names)) {
    mydata$fls$item <- as.factor(1:sum(items_per_facet))
  } else {
    mydata$fls$item <- as.factor(item_names)
  }

  # loadings
  if (!is.null(general_loadings)) {
    mydata$fls$factor_loading <- general_loadings
  }
  if (!is.null(correlated_loadings)) {
    mydata$fls$subfactor_loading <- correlated_loadings
  }

  # correlation matrix ---------------------------------------------------------

  if (!is.null(correlation_matrix)) {mydata$cors <- correlation_matrix}
  colnames(mydata$cors) <- facet_names
  rownames(mydata$cors) <- facet_names
  diag(mydata$cors) <- 1





  # return ---------------------------------------------------------------------

  return(mydata)
}
