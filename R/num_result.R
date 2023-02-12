#' num_result
#'
#' `num_result` creates a list with the elements
#' 
#' * `x` the original values
#' * `fx` the rounded values with [exams::fmt()] as character
#' * `tolerance` the tolerance
#' * `digits` the digits used for rounding
#' 
#' Note that `x` may contain more than one numeric value to determine the rounding and tolerance. 
#' Make sure that you use for numeric exercises `...$x[1]`.
#' 
#' If `digits` are not given and `length(x)>1` then `ceiling(-log10(min(diff(sort(x)), na.rm=TRUE)))` is used.
#' If `digits` are not given and `length(x)==1` then `3+ceiling(-log10(abs(x)))` is used.
#' If no `tolerance` is given then `tolmult*10^(1-digits)` is used.
#' 
#' `int_result` can be used if the result is an integer number and calls `num_result(x, 0, 0.1, 1, ...)` with 
#' a tolerance of 0.1.
#' 
#' @param x numeric: rounded data
#' @param digits numeric: number of digits to rounding (default: \code{NULL})
#' @param tolerance numeric: tolerance for rounded data (default: \code{NULL})
#' @param tolmult numeric: multiplier for tolerance 
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
#' int_result(mean(x))
#' #
#' str(num_result(pi, 3))
#' str(num_result(pi, 6))
#' str(num_result(pi, 6, tolmult=5))
#' str(num_result(pi, 6, tolmult=5, tolerance=1e-6))
num_result <- function (x, digits = NULL, tolerance = NULL, tolmult = 2, ...) { 
  if (is.null(digits)) {
    if (length(x) < 2) {
      if ((x < 0) || (x > 0)) 
        digits <- 3 + ceiling(-log10(abs(x)))
      if (is.null(digits)) digits <- 4
    } else {
      digits <- ceiling(-log10(min(diff(sort(x)), na.rm = TRUE)))
    }
  }
  tolerance <- if (is.null(tolerance)) 
    tolmult * 10^(1 - digits)
  else tolmult * tolerance
  list(x = x, fx = fmt(x, digits = digits, ...), tolerance = tolerance,  digits = digits)
}

#' @rdname num_result
#' @export
int_result <- function(x, ...) { num_result(as.integer(x), digits=0, tolerance=0.1, tolmult=1, ...) }
