#' transformif 
#' 
#' Transforms \code{x} if \code{cond} is \code{TRUE} by \eqn{\log(a+b*x)} if \code{p==0} and \eqn{(a+b*x)^p)} otherwise.
#' The transformation can be either applied to each element of \code{x} or to all elements of \code{x}.
#'
#' @param x vector: values
#' @param cond logical: condition if transformation should be applied
#' @param a numeric: shift (default: \code{-abs(min(x))})
#' @param b numeric: scale (default: \code{1}))
#' @param p numeric: power (default: \code{1}))
#'
#' @return transformed vector
#' @export 
#'
#' @examples
#' x <- rnorm(5)
#' transformif(x, min(x)<0)  # all transformed elements > 0
#' transformif(x, x<0)       # only negative elements are transformed
transformif <- function(x, cond, a=-abs(min(x)), b=1, p=1) {
  if (length(cond)==1) cond <- rep(cond, length(x))
  stopifnot(length(cond)==length(x))
  ifelse(cond, ifelse(p==0, log(a+b*x), (a+b*x)^p), x)
}