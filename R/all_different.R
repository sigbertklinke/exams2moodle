#' all_different
#'
#' Test if the differences between the entries in \code{obj} is larger than \code{mind}.
#'
#' @param obj object: numeric R object which can be converted to a vector
#' @param mind numeric: minimum value
#'
#' @return logical
#' @importFrom stats dist
#' @export
#'
#' @examples
#' x <- runif(10)
#' all_different(x, 0.0001)
#' all_different(x, 1)
all_different <- function(obj, mind) {
  stopifnot(!missing(mind))
  if (!is.data.frame(obj)) obj <- as.data.frame(obj)
  all(rowSums(as.matrix(dist(obj, "maximum"))<mind)<2)
}
