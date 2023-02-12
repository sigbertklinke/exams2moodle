#' numst 
#'
#' Rounds `value` to `digits` and returns a tolerance by the formula: $tolmult*10^{toladd-digits}$.
#'
#' @param value numeric: result value
#' @param digits integer: numbers of digits to round (default: \code{4})
#' @param tolmult numeric: multiplicator for tolerance (default: \code{1})
#' @param toladd  numeric: 
#'
#' @return a list with rounded solution `solution` and tolerance `tol`
#' @export
#'
#' @examples
#' ex <- numst(0.5)
#' ex
#' ex <- numst(0.5, 2)
#' ex
#' ex <- numst(0.5, 3, 2)
#' ex
#' ex <- numst(0.5, 3, 2, 0)
#' ex
numst <- function(value, digits=4, tolmult=1, toladd=1) {
  list(solution=round2(value, digits), tol=tolmult*10^(toladd-digits))
}