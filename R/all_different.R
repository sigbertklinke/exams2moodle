#' all_different
#'
#' Test if the differences between the entries in \code{obj} is larger than \code{tol}.
#'
#' @param obj object: numeric R object which can be converted to a vector
#' @param tol numeric: minimum value
#'
#' @return logical
#' @importFrom stats dist
#' @export
#'
#' @examples
#' x <- runif(10)
#' all_different(x, 0.0001)
#' all_different(x, 1)
all_different <- function(obj, tol) {
  stopifnot(!missing(tol))
  if (!is.data.frame(obj)) obj <- as.data.frame(obj)
  all(rowSums(as.matrix(dist(obj, "maximum"))<tol)<2)
}
