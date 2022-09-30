#' IPV estimation
#'
#' @param dat data frame; raw data (see details)
#' @param name character; name of the overall construct or test that comprises
#'   all items used
#' @param include_raw logical; should raw estimates of factor loadings be
#'   included in the output?; defaults to TRUE
#' @param include_lav logical; should lavaan objects of the fitted models be
#'   included in the output?; defaults to TRUE
#' @param include_xarrow logical; should an object for the drawing of arrows in
#'   nested plots be returned?; defaults to TRUE
#' @param id character; name of the case identifying variable in long format;
#'   defaults to "id"
#' @param value.var character; name of the variable in long format that contains
#'   measurement values; defaults to "value"
#' @param ... further arguments passed to lavaan::cfa (or one step further to
#'   lavaan::lavOptions).
#'
#'
#' @details the data given to \code{dat} can be either in long or in wide
#'   format.
#'
#'   If they are in wide format, they have to conform to the following rules: *
#'   no additional variables / columns * variables are named according to the
#'   following pattern: \code{"test_facet_item"}. * If there is only one test in
#'   the data, the pattern is "facet_item". For tests without facets in a larger
#'   dataset also comprising tests with items, the pattern is "test_item". *
#'   Variable names have to be unique. Item names have to be unique at the level
#'   of the test (not only at the level of the facet) See example
#'
#'   If they are in long format, they have to include the columns "test",
#'   "facet", and "item", as well as a case identifying variable (\code{id}) and
#'   the measurement variable (\code{value.var}).
#'
#' @return list; \code{$est} includes the center distances and all necessary
#'   input for the IPV chart functions, \code{$est_raw} includes the factor
#'   loadings and latent correlations, \code{$lav} includes the fitted models
#'   (class: \code{lavaan}), \code{$xarrow} includes a data frame for arrows
#'   between facets in nested charts, that can be passed on directly to
#'   \code{nested_chart}; by default, all three of these elements are provided.
#'
#'   \code{$xarrow} includes only those cases, where the estimate of the latent
#'   correlation between facets exceeds the estimate of the latent correlation
#'   between their respective tests, as recommended by the original authors.
#'
#' @export
#'
#' @examples
#' # an IPV that comprises the honesty/humility and the agreeableness factor of
#' # the HEXACO
#' res <- ipv_est(
#'   HEXACO[ ,grep("^H|^A", names(HEXACO))],
#'   "HA")
#' nested_chart(res)
#'
#' # Customize call to lavaan::cfa via ellipsis to treat missing data and use long format
#'
#' HEXACO_long <- reshape2::melt(
#'   cbind(id = row.names(HEXACO)[1:1000],
#'   HEXACO[1:1000,1:240]),
#'  id.vars = "id")
#' HEXACO_long$test <- substr(HEXACO_long$variable, 1, 1)
#' HEXACO_long$facet <- substr(HEXACO_long$variable, 3, 6)
#' HEXACO_long$item <- substr(HEXACO_long$variable, 8, 13)
#' HEXACO_long$variable <- NULL
#' head(HEXACO_long)
#' res <- ipv_est(
#'   HEXACO_long[HEXACO_long$test %in% c("H", "A"), ],
#'   name = "HA", missing = "fiml")
#'
#'
ipv_est <- function(
  dat,
  name,
  include_raw = TRUE,
  include_lav = TRUE,
  include_xarrow = TRUE,
  id = "id",
  value.var = "value",
  ...) {

  # convert from long to wide format if needed
  if(any(c("facet", "item") %in% names(dat))) {
    dat <- ipv_long_to_wide(
      x = dat,
      id = id,
      value.var = value.var)
    }

  # helper variables
  nam <- get_names(dat)
  tests <- unique(nam$test)

  # model estimation
  mods <- write_IPV_syntax(dat, name)
  if (length(mods) == 2) { # xarrows don't exist in simple models
    include_xarrow <-  FALSE
  }

  fits <- lapply(mods, function(x) {
    lavaan::cfa(x, dat, ...)
  })

  loads <- lapply(fits, floads)
  cors <- lapply(fits, cormat)

  # data formatting
  if (length(mods) == 2) { # simple case
    est_raw <- list(
      fls = data.frame(
        factor = as.factor(rep(name)),
        subfactor = as.factor(nam$facet),
        item = as.factor(nam$item),
        factor_loading = loads[[1]],
        subfactor_loading = loads[[2]]),
      cors = cors[[2]])
  } else if (length(mods) == 3) { # nested case
    est_raw <- list(
      global = list(
        fls = data.frame(
          factor = as.factor(rep(name)),
          subfactor = as.factor(nam$test),
          item = as.factor(paste(nam$test, nam$item, sep = ".")),
          factor_loading = loads[[1]],
          subfactor_loading = loads[[2]]),
        cors = cors[[2]]),
      tests = as.list(rep(NA, length(tests)))
    )

    names(est_raw$tests) <- tests

    for(i in unique(nam[nam$facet != "","test"])) {
      facets <- unique(nam[nam$test == i, "facet"])
      est_raw$tests[[i]] <- list(
        fls = data.frame(
          factor = as.factor(rep(i)),
          subfactor = as.factor(nam[nam$test == i, "facet"]),
          item = as.factor(nam[nam$test == i, "item"]),
          factor_loading = floads(fits[[2]], i),
          subfactor_loading = floads(fits[[3]], facets)),
        cors = cors[[3]][rownames(cors[[3]]) %in% facets,
                         colnames(cors[[3]]) %in% facets])
    }

    if (include_xarrow) {
      xarrow <- get_xarrows(cors = cors, design = unique(nam[ ,1:2]))
      if(is.null(xarrow)) xarrow <- NA
    }
  }


  # calculation of center distances
  est <- input_manual_process(est_raw)$est


  y <- list(est = est)
  if (include_raw) {
    y[["est_raw"]] <- est_raw
  }
  if (include_lav) {
    y[["lav"]] <- fits
  }
  if (include_xarrow) {
    y[["xarrow"]] <- xarrow
  }

  class(y) <- c("IPV", "list")

  return(y)
}





#' Write IPV syntax
#'
#' Write lavaan model syntax of IPV models on the given dataset
#'
#' @param dat data frame; correctly formatted raw data
#' @param name character; name of the overall construct or test
#'
#' @details Variable names in dat have to conform to the pattern
#'   "test_facet_item". If there is only one test in the data, the pattern is
#'   "facet_item". For tests without facets in a larger dataset also comprising
#'   tests with items, the pattern is "test_item". Variable names have to be
#'   unique at the level of the test AND the facet. Item names have to be unique
#'   at the level of the test (not only at the level of the facet).
#'
#' @return list of character; lavaan model syntax
write_IPV_syntax <- function(dat, name) {

  # helper variables

  nam <- get_names(dat)
  if (is.null(nam$test)) {
    simple <- TRUE
  } else {
    simple <- FALSE
  }

  if (!simple) {
    tests <- unique(nam$test)
  }
  facets <- unique(nam$facet)[unique(nam$facet) != ""]

  # checks
  # duplicated facet label across tests
  if (!simple) {
    temp <- unlist(tapply(nam$facet, nam$test, unique))
    temp <- temp[temp != ""]
    if(any(duplicated(temp))) {
      stop(
        "The data contain a duplicate facet name across tests, please disambiguate.")
    }
    rm(temp)
  }


  # duplicated item label
  if (any(duplicated(names(dat)))) {
    stop(
      "The data contain a duplicate indicator variable name, please disambiguate.")
  }

  # construct model (nested case) / test model (simple case)
  mod1 <- paste(
    name, "=~",
    paste(names(dat), collapse = " + "))

  if (!simple) { # nested case
    # test model
    mod2 <- ind_lav(tests, names(dat))

    # facet model
    if (all(nam$facet == "")) {
      mod3 <- NULL
    } else {
      mod3 <- ind_lav(
        facets,
        names(dat))
    }
  } else {
    mod2 <- ind_lav(
      facets,
      names(dat))
  }

  y <- list(
    mod1 = mod1,
    mod2 = mod2)
  if (exists("mod3")) {
    y[["mod3"]] <- mod3
  }

  return(y)
}





#' ind lav
#'
#' create a lavaan model syntax based on a set of variable names and indicator
#' names that comprise these variable names
#'
#' @param vars character; variable names
#' @param indicators character; indicator names, may include unused indicators
#'
#' @details Indicator names have to include the variable names like this:
#'   "...variable_...". Variable names have to be unique and cannot be contained
#'   in one another like this: "variable_" and "ariable_"
#'
#' @return character; lavaan model syntax
ind_lav <- function(vars, indicators) {

  temp <- list()
  for(i in vars) {
    temp[i] <- paste(
      i, "=~",
      paste(
        grep(paste(i, "_", sep = ""), indicators, value = TRUE),
        collapse = " + ")
    )
  }

  syn <- paste(temp, collapse = "\n")

  return(syn)
}





#' Get names
#'
#' Extract the names of tests, facets, and items from the variable names of a
#' dataset.
#'
#' @param dat data frame; dataset
#'
#' @details variable names in the data have to strictly match the following
#'   scheme: "test_facet_item" or "facet_item".
#'
#' @return data frame; names of tests, facets and items
get_names <- function(dat) {
  temp <- names(dat)

  if (length(grep("_.+_", temp)) > 0) { # nested case
    nam <- data.frame(
      test = sub(
        "_.*",
        "",
        temp),
      facet = gsub(
        "_|character\\(0\\)",
        "",
        as.character(stringr::str_extract_all(temp, "_.*_"))),
      item = sub(
        ".*_",
        "",
        temp)
    )
  } else { # simple case
    nam <- data.frame(
      facet = sub(
        "_.*",
        "",
        temp),
      item = sub(
        ".*_",
        "",
        temp)
    )
  }

  return(nam)
}





#' Cor(relation) Mat(rix)
#'
#' Retrieve factor correlation matrix from lavaan model
#'
#' @param fit fitted lavaan model
#'
#' @return matrix; correlation matrix
cormat <- function (fit) {
  X <- Matrix::cov2cor(lavaan::inspect(fit, "est")$psi)
  class(X) <- "matrix"
  X <- as.matrix(Matrix::forceSymmetric(X))
  if(any(is.nan(X))) X[,] <- NA
  return(X)
}





#' Load(ing)s
#'
#' Extract the standardized factor loadings from a fitted lavaan model.
#'
#' @param fit fitted lavaan model
#' @param vars character; variables for which loadings should be extracted;
#'   defaults to NULL, in which case all variables are considered
#'
#' @return numeric; vector of standardized factor loadings
floads <- function(fit, vars = NULL) {

  x <- lavaan::standardizedSolution(fit)
  if(!is.null(vars)) {
    x <- x[x$lhs %in% vars, ]
  }
  x <- x[x$op == "=~", "est.std"]

  return(x)
}





#' Get Xarrows
#'
#' Creates a data frame for the drawing of arrows in nested charts, including
#' all correlations between facets that exceed the correlation of the respective
#' tests.
#'
#' @param cors list; list of latent correlation matrices of each model
#' @param design data frame; each facet (column "facet") is matched with its
#'   superordinate test (column "test")
#'
#' @return data frame; data frame in the required format for the drawing of
#'   arrows in nested charts, including only those latent facet correlations,
#'   that exceed the correlation between the respective tests.
get_xarrows <- function (cors, design) {
  x <- utils::combn(colnames(cors$mod3), 2, simplify = FALSE)
  y <- lapply(x, function(x) {
    test1 <- design[design$facet == x[1], "test"]
    test2 <- design[design$facet == x[2], "test"]
    if (
      cors$mod3[x[1], x[2]] >
      cors$mod2[test1,test2]
      ) {
      y <- data.frame(
        test1 = test1,
        facet1 = x[1],
        test2 = test2,
        facet2 = x[2],
        value = cors$mod3[x[1], x[2]]
      )
      return(y)

    } else {
      return(NA)
    }
  })
  y <- stats::na.omit(do.call(rbind, y))
  if (length(y) > 0) {
    y$value <- as.character(y$value)
    y$value <- y$value[y$value != 1 & y$value > 0] <-
      substr(y$value[y$value != 1 & y$value > 0], 2, 4)
  } else {
    y <- NULL
  }

  return(y)
}

#' IPV long to wide
#'
#' Helper function to convert long format data into appropriate wide format for
#' ipv_est
#'
#' @param x data frame; raw data in long format
#' @param id character; name of case identifying variable
#' @param value.var character; name of variable that contains measurement values
ipv_long_to_wide <- function(x, id = "id", value.var = "value") {

  check <- c("facet", "item")
  if (any(!check %in% names(x))) {
    stop(paste("x misses required column(s) ", check[!check %in% names(x)], sep = ""))
  }

  names(x)[which(names(x) == c(id, value.var))] <- c("id", "value")

  # simple case
  if(!"test" %in% names(x)) {
    y <- reshape2::dcast(
      x,
      formula = id ~ facet + item,
      value.var = "value")

    # nested case
  } else {
    y <- reshape2::dcast(
      x,
      formula = id ~ test + facet + item,
      value.var = "value")
  }

  row.names(y) <- y$id
  y$id <- NULL

  return(y)
}
