#' unique_max
#'
#' Checks if `x` has a unique maximum. The largest and the second largest value must have at least a distance of `tol`.
#'
#' @param x numeric: values to check
#' @param tol numeric: minimal distance between  largest and the second largest value (default: \code{1e-3})
#'
#' @return logical
#' @md
#' @export
#'
#' @examples
#' x <-runif(100)
#' unique_max(x)
#' unique_max(x, tol=0.1)
unique_max <- function(x, tol=1e-3) {
  x <- sort(x, decreasing = TRUE)
  (x[1]-x[2])>tol
}