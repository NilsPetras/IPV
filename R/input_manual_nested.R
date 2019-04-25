#'Input Manual Nested
#'
#'Generates manual data input for a nested model with several tests.
#'
#'@param construct_name character; the name of the overall construct.
#'@param test_names character; the names of the tests in correct order.
#'@param items_per_test integer; number of items per test in correct order
#'  (determined by test_names), if all tests have the same number of items a
#'  single number can be used, e.g. 10 instead of c(10, 10, 10).
#'@param item_names character or integer; the names of the items in correct
#'  order (determined by test_names).
#'@param construct_loadings integer; vector of the factor loadings from the
#'  single factor model of the construct in correct order (determined by
#'  item_names).
#'@param test_loadings integer; vector of the factor loadings on the test
#'  factors from the group factor model in correct order (determined by
#'  item_names).
#'@param correlation_matrix matrix containing the latent correlations between
#'  tests, pay attention to the order of rows and columns, which is determined
#'  by test_names.
#'
#'@details Pay attention to the order of tests and items, it has to be coherent
#'  throughout the whole data. test_names and items_per_test determine which
#'  test is listed first and how many items are listed for that test.
#'  item_names, construct_loadings and test_loadings have to match that
#'  order. The correlation matrix uses the order in test_names for rows and
#'  columns.
#'
#'  This function only lists the name of the tests in output$tests. For each of
#'  those tests, the data on the facets needs to be added using
#'  \code{\link{input_manual_simple}}.
#'
#'  Visually inspect the returned object before continuing with
#'  \code{\link{input_manual_process}}!
#'
#'@return list containing "raw" data. The data on the facets of the tests needs
#'  to be added using \code{\link{input_manual_simple}}. Afterwards, the whole
#'  data needs to be preprocessed using \code{\link{input_manual_process}}.
#'
#'@seealso \code{\link{input_manual_simple}} \code{\link{input_manual_process}}
#'
#' @examples
#'# these data can also be seen in self_confidence, the example data of
#'# this package
#' mydata <- input_manual_nested(
#'construct_name = "Self-Confidence",
#'test_names = c("DSSEI", "SMTQ", "RSES"),
#'items_per_test = c(20, 14, 10),
#'item_names = c(
#'  1,  5,  9, 13, 17, # DSSEI
#'  3,  7, 11, 15, 19, # DSSEI
#'  16,  4, 12,  8, 20, # DSSEI
#'  2,  6, 10, 14, 18, # DSSEI
#'  11, 13, 14,  1,  5,  6, # SMTQ
#'  3, 10, 12,  8, # SMTQ
#'  7,  2,  4,  9, # SMTQ
#'  1,  3,  4,  7, 10, # RSES
#'  2,  5,  6,  8,  9), # RSES
#'construct_loadings = c(
#'  .5189, .6055, .618,  .4074, .4442,
#'  .5203, .2479, .529,  .554,  .5144,
#'  .3958, .5671, .5559, .4591, .4927,
#'  .3713, .5941, .4903, .5998, .6616,
#'  .4182, .2504, .4094, .3977, .5177, .4603,
#'  .3271,  .261, .3614, .4226,
#'  .2076, .3375, .5509, .3495,
#'  .5482, .4627, .4185, .4185, .5319,
#'  .4548, .4773, .4604, .4657, .4986),
#'test_loadings = c(
#'  .5694, .6794, .6615, .4142, .4584, # DSSEI
#'  .5554, .2165, .5675, .5649, .4752, # DSSEI
#'  .443 , .6517, .6421, .545 , .5266, # DSSEI
#'  .302 , .6067, .5178, .5878, .6572, # DSSEI
#'  .4486, .3282, .4738, .4567, .5986, .5416, # SMTQ
#'  .3602, .2955, .3648, .4814, # SMTQ
#'  .2593, .4053, .61  , .4121, # SMTQ
#'  .6005, .4932, .4476, .5033, .6431, # RSES
#'  .5806, .5907, .6179, .5899, .6559), # RSES
#'correlation_matrix = matrix(data = c(  1, .73, .62,
#'                                       .73,   1, .75,
#'                                       .62, .75,   1),
#'                            nrow = 3,
#'                            ncol = 3))
#'mydata
#'
#'@export
input_manual_nested <- function(
  construct_name,
  test_names,
  items_per_test,
  item_names,
  construct_loadings,
  test_loadings,
  correlation_matrix) {


  cplx <- length(test_names)
  if (length(items_per_test) == 1) {
    items_per_test <- rep(items_per_test, cplx)
  }

  # here the construct + tests are treated as a test + facets, therefore the
  # mismatch of parameter names
  mydata <- list(
    global = input_manual_simple(
      test_name = construct_name,
      facet_names = test_names,
      items_per_facet = items_per_test,
      item_names = item_names,
      test_loadings = construct_loadings,
      facet_loadings = test_loadings,
      correlation_matrix = correlation_matrix),
    tests = as.list(rep(NA, cplx)))

    names(mydata$tests) <- test_names

  # avoid problems with similar naming patterns by adding the subfactor to the
  # item names
  mydata$global$fls$item <- as.character(paste(mydata$global$fls$subfactor,
                                            sep = ".",
                                            mydata$global$fls$item))

  return(mydata)
}
