#' pintegrate
#'
#' Computes the definite integral for a polynomial function of the form (\code{q=length(pcoeff)-1}):
#' \deqn{\int_{\min(interval)}^{\max(interval)} (pcoeff[1] x^q + pcoeff[2] x^{q-1} + ... + pcoeff[q+1]) x^{power} dx}.
#'
#' @param pcoeff polynomial: vector with coefficients for polynom, first entry is the intercept
#' @param interval numeric: interval to integrate over
#' @param power numeric: power of x to multiply the polynomial (default: \code{power=0})
#'
#' @return the definite integral(s)
#' @export
#'
#' @examples
#' # poly(x) = -0.5 x + 1 
#' p   <- polynomial(1, -0.5)
#' res <- pintegrate(p, c(0, 2), 0:2)
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
  id  <- diag(max(power)+1)
  res <- rep(NA, length(power))
  q   <- vector("list", length(power))
  for (i in seq(length(power))) {
    if (power[i]==0) {
      q[[i]] <- pinteg(pcoeff)      
    } else {
      q[[i]] <- pinteg(pmult(pcoeff, polynomial(id[,power[i]+1])))
    }
    res[i] <- peval(q[[i]], border[2])-peval(q[[i]], border[1])    
  }
  structure(res, qpoly=q)
}
