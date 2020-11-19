ipv_est <- function(dat, name, estimator = "ML", include_raw = TRUE, include_lav = FALSE) {

  # helper variables
  nam <- get_names(dat)
  tests <- unique(nam$test)

  # model estimation
  mods <- write_IPV_syntax(dat, name)
  fits <- lapply(mods, function(x) {
    lavaan::cfa(x, dat, estimator = estimator)
  })

  loads <- lapply(fits, loads)
  cors <- lapply(fits, cormat)

  # data formatting
  if(length(mods) == 2) { # simple case
    est_raw <- list(
      fls = data.frame(
        factor = as.factor(rep(name)),
        subfactor = as.factor(nam$test),
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
          factor_loading = loads(fits[[2]], i),
          subfactor_loading = loads(fits[[3]], facets)),
        cors = cors[[3]][rownames(cors[[3]]) %in% facets,
                         colnames(cors[[3]]) %in% facets])
    }
  }


  # calculation of center distances
  est <- input_manual_process(est_raw)


  y <- list(est = est)
  if(include_raw == TRUE) {
    y[["est_raw"]] <- est_raw
  }
  if(include_lav == TRUE) {
    y[["lav"]] <- fits
  }
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
  tests <- unique(nam$test)
  facets <- unique(nam$facet)[unique(nam$facet) != ""]

  # checks
  # duplicated facet label across tests
  temp <- unlist(tapply(nam$facet, nam$test, unique))
  temp <- temp[temp != ""]
  if(any(duplicated(temp))) {
    stop("The data contain a duplicate facet name across tests, please disambiguate.")
  }
  rm(temp)

  # duplicated item label
  if(any(duplicated(names(dat)))) {
    stop("The data contain a duplicate indicator variable name, please disambiguate.")
  }

  # construct model
  mod1 <- paste(
    name, "=~",
    paste(names(dat), collapse = " + "))

  # test model
  mod2 <- ind_lav(tests, names(dat))

  # facet model
  if(all(nam$facet == "")) {
    mod3 <- NULL
  } else {
    mod3 <- ind_lav(
      facets,
      names(dat))
  }

  y <- list(
    mod1 = mod1,
    mod2 = mod2)
  y[["mod3"]] <- mod3 # assures that no addition is made in case of mod3 == NULL

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
#' @return
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
loads <- function(fit, vars = NULL) {

  x <- lavaan::standardizedsolution(fit)
  if(!is.null(vars)) {
    x <- x[x$lhs %in% vars, ]
  }
  x <- x[x$op == "=~", "est.std"]

  return(x)
}
