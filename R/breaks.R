#' breaks
#'
#' Creates a number of equidistant or non-equidistant breaks for given data `x`. 
#' If `width` is not given then it will be set to \code{diff(pretty(x))[1]}. 
#' `probs` can either be a single integer giving the number of quantiles or a vector of probabilities with values in \eqn{[0,1]}.
#' Note that if `width` is too large then using `probs` may result in equidistant breaks, too.
#'
#' @param x numeric: data
#' @param width numeric: class width (default: \code{NULL})
#' @param probs numeric: number of non-equidistant classes (default: \code{NULL})
#'
#' @md
#' @return a numeric vector of breaks
#' @export
#'
#' @examples
#' x <- rnorm(100, mean=1.8, sd=0.1)
#' breaks(x)
#' breaks(x, 0.1)
#' breaks(x, 0.1, probs=4)
breaks <- function(x, width=NULL, probs=NULL) {
  if (is.null(width)) width <- diff(pretty(x))[1]
  if (!is.null(probs)) { # not equidistant
    if (length(probs)==1) probs <- seq(0, 1, by=1/probs)
    if (min(probs)!=0) probs <- c(0, probs)
    if (max(probs)!=1) probs <- c(probs, 1)    
    ret <- round(quantile(x, probs)/width)*width
  } else { # equidistant
    bmin <- floor(min(x)/width)*width
    bmax <- ceiling(max(x)/width)*width
    ret <- seq(bmin, bmax, by=width)
  }
  if (anyDuplicated(ret)) ret <- unique(ret)
  if (min(ret)>min(x)) ret <- c(ret[1]-width, ret)
  if (max(ret)<max(x)) ret <- c(ret, ret[length(ret)]+width)

  #
  ret
}