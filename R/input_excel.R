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


  # the helper function 'input_excel_factor' does most of the dirty work and
  # applies to both the global and the nested scale

  if (is.null(global)) { # simple case
    mydata <- input_excel_factor(tests)
  } else { # nested case

    # global level
    global_input <- input_excel_factor(global)


    # check if all files are given
    if (length(levels(global_input$cds$subfactor)) != length(tests)
        ) stop ("Missing file")

    # test level
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

    # including test names
    for (i in 1:length(colnames(global_input$cors))) {
      names(tests_input)[i] <- levels(tests_input[[c(i, 1)]]$factor)
    }

    mydata <- list(global = global_input, tests = tests_input)
  }

  return(mydata)
}
