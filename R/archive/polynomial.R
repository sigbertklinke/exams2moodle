#' @rdname polynomial
#' @title polynomial
#' @aliases peval pderiv pinteg pmult pprod pdiff padd psum pminimum
#' @description 
#' * `polynomial` creates a vector of coefficients for a polynomial. The intercept is the first value
#' * `peval` evaluates the polynomial at given `x`
#' * `pderiv` computes the derivative of a polynomial
#' * `pinteg` computes the integral of a polynomial
#' * `pmult` multiplies two polynomials
#' * `pprod` computes the product of polynomials
#' * `pdiff` computes the differences of two polynomials
#' * `padd` computes the sum of two polynomials
#' * `psum` computes the sum of polynomials
#' * `pminimum` minimum of a polynomial in the interval \eqn{[lower, upper]}. With [base::polyroot] are the 
#' real zeroes for the derivative computed in the given interval. At these values and the interval borders of the 
#' polynomial `p` is evaluated and the minimum returned.
#' 
#' @param p,q polynomial
#' @param x numeric: where to evaluate the polynomial
#' @param const numeric: a constant for the integrated polynomial (default: `0`)
#' @param interval numeric:	a vector containing the end-points of the interval to be searched for the minimum
#' @param lower numeric: the lower end point of the interval to be searched (default: \code{min(interval)})
#' @param upper numeric: the upper end point of the interval to be searched (default: \code{max(interval)})
#' @param tol numeric: the desired accuracy (default: \code{1e-9})
#' @param ... vector of coefficients (first is intercept) or further polynomials
#'
#' @return a polynomial
#' @export
#'
#' @examples
#' polynomial(0,1)  # x
#' polynomial(1,1)  # x+1
#' polynomial(1:3)  # 3*x^2+2*x+1
#' #
#' p <- polynomial(1:3)     # 3*x^2+2*x+1
#' peval(p, 0:2)            # evaluates it at x=0, x=1, x=2
#' pderiv(p)                # derivative
#' pinteg(p)                # integral
#' pmult(p, p)              # squared polynomial
#' pdiff(p, p)              # the null polynomial
#' #
#' p <- polynomial(c(-5, 3, -3, 1))
#' pminimum(p, -3, 3)
polynomial <- function(...) {
  p <- as.numeric(unlist(list(...)))
  structure(p, class=c("polynomial", class(p)), names=sprintf(".^%.0f", seq(length(p))-1))
}

#' @rdname polynomial
#' @export
peval  <- function(p, x) {
  stopifnot("polynomial" %in% class(p))  
  as.numeric(outer(x, seq(length(p))-1, "^")%*%p)
}

#' @rdname polynomial
#' @export
pderiv <- function(p) {
  stopifnot("polynomial" %in% class(p))
  q <- (p*(seq(length(p))-1))[-1]
  if (length(q)==0) q <- 0
  structure(q, class=class(p), names=sprintf(".^%.0f", seq_along(q)-1))
}

#' @rdname polynomial
#' @export
pinteg <- function(p, const=0) {
  stopifnot("polynomial" %in% class(p))
  q <- c(const, p/seq(length(p)))
  structure(q, class=class(p), names=sprintf(".^%.0f", seq(length(q))-1))  
}

#' @rdname polynomial
#' @export
pmult <- function(p, q) {
  if (!inherits(p, "polynomial")) p <- polynomial(as.numeric(p))
  if (!inherits(q, "polynomial")) q <- polynomial(as.numeric(q))
  q <- tapply(outer(p, q), outer(seq(length(p))-1, seq(length(q))-1, "+"), sum)
  structure(q, class=class(p), names=sprintf(".^%.0f", seq(length(q))-1))  
}

#' @rdname polynomial
#' @export
pprod <- function(p, ...) {
  if (!inherits(p, "polynomial")) p <- polynomial(as.numeric(p))
  args <- list(...)
  if (length(args)>0) {
    for (i in 1:length(args)) p <- pmult(p, args[[i]])
  }
  structure(p, class=class(p), names=sprintf(".^%.0f", seq(length(p))-1))  
}

#' @rdname polynomial
#' @export
pdiff <- function(p, q) {
  if (!inherits(p, "polynomial")) p <- polynomial(as.numeric(p))
  if (!inherits(q, "polynomial")) q <- polynomial(as.numeric(q))
  lpq <- length(p)-length(q) 
  if (lpq>0) q <- c(q, rep(0, lpq))
  if (lpq<0) p <- c(p, rep(0, -lpq))
  structure(p-q, class=class(p), names=sprintf(".^%.0f", seq(length(q))-1))  
}

#' @rdname polynomial
#' @export
padd <- function(p, q) {
  if (!inherits(p, "polynomial")) p <- polynomial(as.numeric(p))
  if (!inherits(q, "polynomial")) q <- polynomial(as.numeric(q))
  lpq <- length(p)-length(q) 
  if (lpq>0) q <- c(q, rep(0, lpq))
  if (lpq<0) p <- c(p, rep(0, -lpq))
  structure(p+q, class=class(p), names=sprintf(".^%.0f", seq(length(q))-1))  
}

#' @rdname polynomial
#' @export
psum <- function(p, ...) {
  if (!inherits(p, "polynomial")) p <- polynomial(as.numeric(p))
  args <- list(...)
  if (length(args)>0) {
    for (i in 1:length(args)) p <- padd(p, args[[i]])
  }
  structure(p, class=class(p), names=sprintf(".^%.0f", seq(length(p))-1))  
}

#' @rdname polynomial
#' @export
pminimum <- function(p, interval, lower=min(interval), upper=max(interval), tol=1e-9) {
  stopifnot("polynomial" %in% class(p))
  pz   <- polyroot(as.numeric(pderiv(p)))
  pz   <- Re(pz)[abs(Im(pz))<tol]
  keep <- (pz>=lower) & (pz<=upper)
  pz   <- c(lower, pz[keep], upper)
  min(peval(p, pz))
}
