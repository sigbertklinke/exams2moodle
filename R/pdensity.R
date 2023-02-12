#' pdensity
#' 
#' Creates a linear (\code{power=1}) or constant (\code{power=0}) density function in a interval \deqn{[a, b]} where
#' \code{a} and \code{b} are sampled from \code{x}. It samples \code{size} elements without replacement and computes 
#' the value of the distribution function. 
#'
#' @param x numeric: range of density with \eqn{a=min(x, na.rm=TRUE)} and \eqn{b=max(x, na.rm=TRUE)}
#' @param size numeric: number of elements to be sampled (without replacement) from x
#' @param power numeric: constant or linear density function
#' @param tol numeric: disallow for density coefficients near zero (default: \code{1e-6}). A negative value will allow for zero coefficients.
#'
#' @md
#' @return a list with 
#' * \code{a} the minimum of the interval
#' * \code{i} the maximum of the interval
#' * \code{x} the \code{size} sampled values
#' * \code{fx} the distribution function at \code{x}
#' * \code{pcoeff} polynomial (intercept = first value)
#' * \code{qcoeff} indefinite integral of the polynomial (intercept = first value)
#' * \code{pint} result of \code{integral(pcoeff, c(a,b), 0:2)}
#' @importFrom polynom polynomial integral polylist
#' @export
#'
#' @examples
#' pdensity(-5:5)
#' pdensity(-5:5, power=1)
pdensity <- function(x, size=3, power=1, tol=1e-6) {
  fn <- function(x, range, denom) { 
    if (!inherits(x, "polynomial")) x <- polynomial(x)
    abs(integral(x/denom, range)-1) 
  }
  #
  stopifnot(power %in% 0:1)
  stopifnot(size>=2)
  repeat {
    r  <- sort(sample(x, size=size))
    dr <- diff(range(r, na.rm=TRUE))
    if (power==0) pcoeff <- polynomial(1/dr)
    if (power==1) { # can be generalized to power>2
      a <- min(r, na.rm=TRUE)
      b <- max(r, na.rm=TRUE)
      if (runif(1)<0.5) {
        pcoeff <- 2/(b^2-a^2-2*a*(b-a))
        pcoeff <- polynomial(pcoeff*c(-a, 1))
      } else {
        pcoeff <- 2/(b^2-a^2-2*b*(b-a))
        pcoeff <- polynomial(pcoeff*c(-b, 1))
      }
    }
    if (all(abs(as.numeric(pcoeff))>tol)) break
  }
  fx <- rep(NA, length(r))
  for (i in seq(length(r))) fx[i] <- if(i==1) 0 else integral(pcoeff, c(min(r), r[i]))
  pl <- polylist(1, c(0,1), c(0,0,1))
  list(a=min(r), b=max(r), x=r, fx=fx, pcoeff=pcoeff, 
       pint=sapply(pl, function(pr) { integral(pr*pcoeff, range(r)) }))
}