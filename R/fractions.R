#' fractions
#'
#' Find rational approximations to the components of a real numeric object using a standard continued fraction method.
#' Calls [MASS::fractions()], see details there.
#'
#' @param x Any object of mode numeric. Missing values are now allowed.
#' @param cycles The maximum number of steps to be used in the continued fraction approximation process. 
#' @param max.denominator An early termination criterion. If any partial denominator exceeds max.denominator the continued fraction stops at that point.
#' @param ... arguments passed to or from other methods.
#'
#' @return An object of class "fractions". A structure with .Data component the same as the input numeric x, but with the rational approximations held as a character vector attribute, "fracs". Arithmetic operations on "fractions" objects are possible.
#' @export
#' @md
#'
#' @examples
#' X <- matrix(runif(25), 5, 5)
#' fractions(X) #;)
#' fractions(solve(X, X/5))
#' fractions(solve(X, X/5)) + 1
fractions <- function (x, cycles = 10, max.denominator = 2000, ...) {
  MASS::fractions(x, cycles, max.denominator, ...)                      
}