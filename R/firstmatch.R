#' firstmatch
#' 
#' \code{firstmatch} seeks matches for the elements of its first argument among those of its second, 
#' for details see [base::charmatch()]. `charmatch` returns a zero if multiple matches are found, whereas `firstmatch` 
#' returns the first partial match if multiple matches are found.  
#'
#' @param x character: the values to be matched; converted to a character vector if necessary
#' @param table character: the values to be matched against; converted to a character vector if necessary
#' @param nomatch integer: the value to be returned at non-matching positions (default: \code{NA_integer_})
#'
#' @return integer 
#' @export
#' @md
#'
#' @examples
#' firstmatch("d", c("chisq", "cauchy"))
#' charmatch("c", c("chisq", "cauchy"))
#' firstmatch("c", c("chisq", "cauchy"))
#' firstmatch("ca", c("chisq", "cauchy"))
firstmatch <- function(x, table, nomatch = NA_integer_) {
  m  <- charmatch(x, table, nomatch)
  tf <- !is.na(m) & (m==0)
  for (i in seq(x)) {
    if (tf[i]) m[i] <- which(startsWith(table, x[i]))[1]
  }
  m
}