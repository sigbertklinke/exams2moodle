#' num_result
#'
#' Creates a list with the elements
#' 
#' * `x` the original values
#' * `fx` the rounded values with [exams::fmt()] as character
#' * `tolerance` the tolerance
#' * `digits` the digits used for rounding
#' 
#' @param x numeric: rounded data
#' @param digits numeric: number of digits to rounding (default: \code{NULL})
#' @param tolerance numeric: tolerance for rounded data (default: \code{NULL})
#' @param tolmult numeric: mutiplier for tolerance 
#' @param ... further parameters to [exams::fmt()]
#' 
#' @return a list
#' @md
#' @importFrom exams fmt
#' @export
#'
#' @examples
#' # height for german man (in meter)
#' x <- rnorm(10, mean=1.8, sd =0.25)
#' num_result(c(mean(x), x), digits=2)
num_result <- function(x, digits=NULL, tolerance=NULL, tolmult = 1, ...) {
  if (length(x)<2) {
    if (is.null(digits)) digits <- 2
  } else {
    if (is.null(digits)) {
      digits <- ceiling(-log10(min(diff(sort(x)), na.rm=TRUE)))
    } 
  }
  if (is.null(tolerance)) tolerance <- tolmult*10^(-digits)
  list(x=x, fx=fmt(x, digits=digits, ...), tolerance=tolerance, digits=digits)
}