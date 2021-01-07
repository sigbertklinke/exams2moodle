#' pintegrate
#'
#' Computes the definite integral for a polynomial function of the form (\code{q=length(pcoeff)-1}):
#' \deqn{\int_{\min(interval)}^{\max(interval)} (pcoeff[1] x^q + pcoeff[2] x^{q-1} + ... + pcoeff[q+1]) x^{power} dx}.
#'
#' @param pcoeff numeric: vector with coefficients for polynom, last entry is the intercept
#' @param interval numeric: interval to integrate over
#' @param power numeric: power of x to multiply the polynomial (default: \code{power=0})
#'
#' @return the definite integral(s)
#' @export
#'
#' @examples
#' # poly(x) = -0.5 x + 1 
#' res <- pintegrate(c(-0.5, 1), c(0, 2), 0:2)
#' # is density
#' res[1]
#' # expectation
#' res[2]
#' # variance
#' res[3]-res[2]^2
pintegrate <- function(pcoeff, interval, power=0) {
  border <- range(interval)
  if (any(!is.finite(border))) stop("non-finite interval")
  if (any(!is.finite(pcoeff))) stop("non-finite coefficients")
  if (any(power<0)) stop("negative power")
  if (equal(border[1], border[2])) warning("interval max = interval min")
  res    <- rep(0, length(power))
  for (i in seq(pcoeff)) {
    p   <- length(pcoeff)-i
    res <- res+pcoeff[i]/(p+1+power)*(border[2]^(p+1+power)-border[1]^(p+1+power))
  }
  res
}
