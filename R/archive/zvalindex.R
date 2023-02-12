#' zvalindex
#'
#' Returns an index where the standardized `x` values have `digits` decimal places and the absolute 
#' values are smaller than `max`.
#'
#' @param x numeric: vector of values
#' @param mean numeric: mean value
#' @param sd numeric: standard deviation
#' @param digits integer: number of decimal places (default: `2`)
#' @param zmax numeric: maximum absolute standardized value (default: `4`)
#'
#' @return an index of standardized values which fulfill the conditions
#' @export
#'
#' @examples
#' x <- c(0, runif(5), 1:5)
#' zvalindex(x, 0, 1)
#' zvalindex(x, 0, 1, max=Inf)
#' zvalindex(x, 0, 1, max=2.5)
zvalindex <- function(z, digits=2, zmax=4) {
  which(has_digits(z, digits) & (abs(z)<=zmax))
}