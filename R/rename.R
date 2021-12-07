#' Rename
#'
#' Renames tests, facets, or items in IPV estimates
#'
#' @param data IPV estimates for chart creation or full output of \code{ipv_est}
#' @param before character; a vector of names to replace
#' @param after character; a vector of replacement names
#'
#' @return the same data with renamed values / variables
#'
#' @export
rename <- function(data, before, after) {

  # simple cases
  if (names(data)[1] %in% c("cds", "global")) {
    data <- rename_est(data, before, after)
  }

  # full ipv_est output
  if (names(data)[1] == "est") {
    data$est <- rename_est(data$est, before, after)
    if (!is.null(data$est_raw)) {
      data$est_raw <- rename_raw(data$est_raw, before, after)
    }
    if (!is.null(data$xarrow)) {
      data$xarrow <- rename_xarrow(data$xarrow, before, after)
    }
  }

  return(data)
}

#' Rename estimates
#'
#' Renames tests, facets, or items in IPV data
#'
#' @param data IPV chart creation data (nested or simple)
#' @param before character; a vector of names to replace
#' @param after character; a vector of replacement names
#'
#' @return the same data with renamed values / variables
rename_est <- function(data, before, after) {

  # simple case
  if (names(data)[1] == "cds") {
    data <- rename_simple(data, before, after)
  }

  # nested case
  if (names(data)[1] == "global"){
    data$global <- rename_simple(data$global, before, after, regex = TRUE)
    data$tests <- lapply(
      X = data$tests,
      FUN = rename_simple,
      before = before,
      after = after)
    for (i in seq_along(before)) {
      names(data$tests)[names(data$tests) == before[i]] <- after[i]
    }
  }

  return(data)
}


#' Rename raw estimates
#'
#' Renames tests, facets, or items in IPV raw estimates
#'
#' @param data IPV raw estimates (as provided by \code{ipv_est} as
#'   \code{est_raw})
#' @param before character; a vector of names to replace
#' @param after character; a vector of replacement names
#'
#' @return the same data with renamed values / variables
rename_raw <- function(data, before, after) {

  # simple case
  if (names(data)[1] == "fls") {
    data <- rename_raw_simple(data, before, after)
  }

  # nested case
  if (names(data)[1] == "global"){
    data$global <- rename_raw_simple(data$global, before, after, regex = TRUE)
    data$tests <- lapply(
      X = data$tests,
      FUN = rename_raw_simple,
      before = before,
      after = after)
    for (i in seq_along(before)) {
      names(data$tests)[names(data$tests) == before[i]] <- after[i]
    }
  }

  return(data)
}


#' Rename Simple
#'
#' @param data IPV estimates (simple)
#' @param before character; a vector of names to replace
#' @param after character; a vector of replacement names
#' @param regex logical; should items be renamed based on regular expressions
#'   for treatment of global section in nested data?; defaults to FALSE
#'
#' @details This function does not support regular expressions. Provide full
#'   names only.
#'
#' @return the same data with renamed values / variables
rename_simple <- function(data, before, after, regex = FALSE) {

  data$cds[ ,1:3] <- sapply(data$cds[1:3], as.character)

  for (i in seq_along(before)) {
    data$cds[data$cds == before[i]] <- after[i]
    colnames(data$cors)[colnames(data$cors) == before[i]] <- after[i]
    rownames(data$cors)[rownames(data$cors) == before[i]] <- after[i]
    # in nested data, the items have their test's name pasted, so
    # renaming as above fails, therefore:
    if (regex) {
      # item name
      x <- grep(paste("\\.", before[i], "$", sep = ""), data$cds$item)
      data$cds$item[x] <- gsub(
        paste("\\.", before[i], "$", sep = ""),
        paste("\\.", after[i], sep = ""),
        data$cds$item[x])
      # test name
      x <- grep(paste("^", before[i], "\\.", sep = ""), data$cds$item)
      data$cds$item[x] <- gsub(
        paste("^", before[i], "\\.", sep = ""),
        paste(after[i], "\\.", sep = ""),
        data$cds$item[x])
    }

  }

  for (i in 1:3) {
    data$cds[ ,i] <- as.factor(data$cds[ ,i])
  }

  return(data)
}


#' Rename Raw Simple
#'
#' @param data IPV raw estimates (simple)
#' @param before character; a vector of names to replace
#' @param after character; a vector of replacement names
#' @param regex logical; should items be renamed based on regular expressions
#'   for treatment of global section in nested data?; defaults to FALSE
#'
#' @details This function does not support regular expressions. Provide full
#'   names only.
#'
#' @return the same data with renamed values / variables
rename_raw_simple <- function(data, before, after, regex = FALSE) {

  data$fls[ ,1:3] <- sapply(data$fls[1:3], as.character)

  for (i in seq_along(before)) {
    data$fls[data$fls == before[i]] <- after[i]
    colnames(data$cors)[colnames(data$cors) == before[i]] <- after[i]
    rownames(data$cors)[rownames(data$cors) == before[i]] <- after[i]
    # in nested data, the items have their test's name pasted, so
    # renaming as above fails, therefore:
    if (regex) {
      # item name
      x <- grep(paste("\\.", before[i], "$", sep = ""), data$fls$item)
      data$fls$item[x] <- gsub(
        paste("\\.", before[i], "$", sep = ""),
        paste("\\.", after[i], sep = ""),
        data$fls$item[x])
      # test name
      x <- grep(paste("^", before[i], "\\.", sep = ""), data$fls$item)
      data$fls$item[x] <- gsub(
        paste("^", before[i], "\\.", sep = ""),
        paste(after[i], "\\.", sep = ""),
        data$fls$item[x])
    }

  }

  for (i in 1:3) {
    data$fls[ ,i] <- as.factor(data$fls[ ,i])
  }

  return(data)
}

#' Rename Xarrow
#'
#' @param data IPV estimates for extra arrows (as provided e.g. by
#'   \code{ipv_est})
#' @param before character; a vector of names to replace
#' @param after character; a vector of replacement names
#'
#' @return the same data with renamed values / variables
rename_xarrow <- function(data, before, after) {

  for (i in seq_along(before)) {
    data[data == before[i]] <- after[i]
  }

  return(data)
}

