#'Input Manual Simple
#'
#'Generates manual data input for a simple model with one test.
#'
#'@param test_name character; the name of the test.
#'@param facet_names character; the names of the facets in correct order.
#'@param items_per_facet integer; number of items per facet in correct order
#'  (determined by facet_names), if all facets have the same number of items a
#'  single number can be used, e.g. 5 instead of c(5, 5, 5, 5).
#'@param item_names character or integer; the names of the items in correct
#'  order (determined by facet_names).
#'@param test_loadings integer; vector of the factor loadings from the single
#'  factor model of the test or a group factor model of multiple tests in
#'  correct order (determined by item_names).
#'@param facet_loadings integer; vector of the factor loadings on the facet
#'  factors from the group factor model in correct order (determined by
#'  item_names).
#'@param correlation_matrix matrix containing the latent correlations between
#'  facets, pay attention to the order of rows and columns, which is determined
#'  by facet_names.
#'
#'@details Pay attention to the order of facets and items, it has to be coherent
#'  throughout the whole data. facet_names and items_per_facet determine which
#'  facet is listed first and how many items there are listed for that facet.
#'  item_names, test_loadings and facet_loadings have to match that order.
#'  The correlation matrix uses the order in facet_names for rows and columns.
#'
#'  Visually inspect the returned object before continuing with
#'  \code{\link{input_manual_process}}!
#'
#'@return list containing "raw" data, that needs to be preprocessed using
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
#'test_loadings = c(.5806, .5907, .6179, .5899, .6559,
#'                     .6005, .4932, .4476, .5033, .6431),
#'facet_loadings = c(.6484, .6011, .6988, .6426, .6914,
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
  item_names,
  test_loadings,
  facet_loadings,
  correlation_matrix) {


  cplx <- length(facet_names)
  if (length(items_per_facet) == 1) {
    items_per_facet <- rep(items_per_facet, cplx)
  }
  nitems <- sum(items_per_facet)


  # running a ton of checks to avoid identifiable typos and other mistakes

  # missing or superfluous values
  if (any(c(length(item_names) != nitems,
            length(test_loadings) != nitems,
            length(facet_loadings) != nitems))
      ) stop ("Missing or superfluous value")

  # correlation matrix wrong size
  if (any(dim(correlation_matrix) != c(cplx, cplx))
      ) stop ("Correlation matrix dimensions do not match number of subfactors")

  # values in matrix asymmetrical (Hermitian)
  if (!isSymmetric(correlation_matrix)) stop ("Correlation matrix asymmetrical")

  # correlation matrix values > 1 or < -1
  if (any(correlation_matrix > 1)) stop ("Correlation > 1")
  if (any(correlation_matrix < -1)) stop ("Correlation < -1")

  # factor loadings < 0
  bad <- min(c(test_loadings, facet_loadings))
  bad <- bad < 0
  if (bad) stop ("Data contains negative factor loading")

  # factor loadings < .1
  bad <- min(c(test_loadings, facet_loadings))
  bad <- bad < .1
  if (bad) warning ("At least one factor loading set to minimum of 0.1")
  test_loadings[test_loadings < .1] <- .1
  facet_loadings[facet_loadings < .1] <- .1

  # factor loadings > 1
  bad <- max(c(test_loadings, facet_loadings))
  bad <- bad > 1
  if (bad) warning ("At least one factor loading > 1, check for correctness")

  # subfactor with only one item
  if (any(items_per_facet == 1)) warning ("Single item subfactor")


  mydata <- list(
    fls = data.frame(factor = as.factor(rep(test_name, nitems)),
                     subfactor = NA,
                     item = as.factor(item_names),
                     factor_loading = test_loadings,
                     subfactor_loading = facet_loadings),
    cors = correlation_matrix)

  subfactor <- NULL
  for (i in 1:cplx) {
    subfactor <- c(subfactor, rep(facet_names[i], items_per_facet[i]))
  }
  mydata$fls$subfactor <- as.factor(subfactor)

  # check if any item name occurs more than once within a facet
  a <- split(mydata$fls, f = mydata$fls$subfactor)
  lapply(a, function(x) if (length(levels(droplevels(x$item))) <
                            length(x$item)
  ) stop ("Item name reoccuring within subfactor"))

  colnames(mydata$cors) <- facet_names
  rownames(mydata$cors) <- facet_names
  if (any(diag(mydata$cors)!=1)) warning (
    "Main diagonal in correlation matrix set to 1")
  diag(mydata$cors) <- 1

  return(mydata)
}
