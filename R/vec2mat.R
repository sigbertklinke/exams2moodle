#' vec2mat
#' 
#' Converts a vector horizontal or vertical matrix and sets row- or colnames. 
#' If \code{rownames} or \code{colnames} are given then existing row names or column names are overwritten.
#'
#' @param x vector: 
#' @param colnames character: vector of new column names (default: \code{NULL})
#' @param rownames character: vector of new row names (default: \code{NULL})
#' @param horizontal logical: horizontal or vertical matrix (default: \code{TRUE})
#'
#' @return matrix
#' @export
#'
#' @examples
#' x <- runif(5)
#' vec2mat(x)
#' vec2mat(x, horizontal=FALSE)
vec2mat <- function(x, colnames=NULL, rownames=NULL, horizontal=TRUE) {
  x <- matrix(x, nrow=ifelse(horizontal, 1, length(x)), ncol=ifelse(horizontal, length(x), 1))
  if (!is.null(colnames)) colnames(x) <- colnames
  if (!is.null(rownames)) rownames(x) <- rownames
  x
}
