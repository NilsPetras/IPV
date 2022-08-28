validate_IPV <- function(x) {

  # highest level checks
  if(!all(names(x) %in% c("est", "est_raw", "lav", "xarrow"))) {
    warning("Unexpected or misnamed element detected on the highest level.")
  }

  if(!any(names(x) == "est")) {
    stop("There are no estimates of center distances and latent correlations.")
  }

  # est element checks
  if(!all.equal(names(x$est), c("global", "tests"))) {
    stop("Malformed or misnamed structure in $est")
  }

  for (i in c("x$est$global", paste("x$est$tests", names(x$est$tests), sep = "$"))) {
    validate_IPV_helper(get(i))
  }

  return(x)

}

# check individual elements consisting of cds and cors
validate_IPV_helper <- function(x) {
 }
