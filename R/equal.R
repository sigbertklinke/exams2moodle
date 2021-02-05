#' equal
#'
#' Computes \code{abs(x-y)<tol}.
#'
#' @param x numeric
#' @param y numeric
#' @param tol numeric: tolerance (default: \code{1e-6})
#'
#' @return logical
#' @export
#'
#' @examples
#' equal(9*1/9, 1)
equal <- function(x, y, tol=1e-6) {
  abs(x-y)<tol
}