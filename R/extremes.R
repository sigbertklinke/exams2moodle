#' extremes
#'
#' Computes the real valued extremes (minima, maxima, and saddle points) for a univariate polynomial.
#' The computation can be limited to a specific type of extremes.
#' 
#' @param p polynomial
#' @param type character: either `all` (default), `minimum`, `maximum`, or `saddle`
#' @param tol numeric: if the absolute value of the imaginary part of the zeroes of the derivative of `p` is 
#' smaller than `tol`, it will be considered as zero 
#'
#' @return numeric vector
#' @importFrom stats deriv predict
#' @export
#'
#' @examples
#' p <- polynomial(c(0,0,0,1))
#' extremes(p)
#' p <- integral(poly.calc(-1:1))
#' extremes(p)
extremes <- function(p, type=c("all", "minimum", "maximum", "saddle"), tol=1e-9) {
  type <- match.arg(type)
  p1 <- deriv(p)
  z  <- unique(polyroot(p1))
  z  <- Re(z[abs(Im(z))<tol])
  ztype <- c("maximum", "saddle", "minimum")[2+sign(predict(deriv(p1), z))]
  if (type!="all") {
    z  <- z[ztype==type]
    if (length(z)==0) return(numeric(0))
    ztype <-rep(type, length(z)) 
  }
  structure(z, type=ztype)
}

#' pminimum 
#'
#' Computes the minimum of a polynomial in the interval \eqn{[lower, upper]}. At these values and the interval borders of the 
#' polynomial `p` is evaluated and the minimum value returned.
#'
#' @param p polynomial
#' @param interval numeric:	a vector containing the end-points of the interval to be searched for the minimum
#' @param lower numeric: the lower end point of the interval to be searched (default: \code{min(interval)})
#' @param upper numeric: the upper end point of the interval to be searched (default: \code{max(interval)})
#' @param tol numeric: the desired accuracy (default: \code{1e-9})
#' 
#' @return the minimal function value
#' @export
#'
#' @examples
#' p <- polynomial(c(-5, 3, -3, 1))
#' pminimum(p, -3, 3)
pminimum <- function(p, interval, lower=min(interval), upper=max(interval), tol=1e-9) {
  if (!inherits(p, "polynomial")) p <- polynomial(as.numeric(p))
  z <- c(extremes(p, "min", tol=tol), c(lower, upper))
  min(predict(p, z))
}

