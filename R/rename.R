#' Rename
#'
#' Renames tests, facets, or items in IPV data
#'
#' @param data IPV chart creation data (nested or simple)
#' @param before character; a vector of names to replace
#' @param after character; a vector of replacement names
#'
#' @return the same data with renamed values / variables
#'
#' @examples
#' rename(self_confidence,
#' c("RSES", "Ct", "7"),
#' c("Rosenberg", "control", "item_7"))
#'
#' @export
rename <- function(data, before, after) {

  if (all.equal(names(data), c("cds", "cors")) == TRUE) {
    data <- rename_simple(data, before, after)
  } else {
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



#' Rename Simple
#'
#' @param data IPV chart creation data (nested or simple)
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
