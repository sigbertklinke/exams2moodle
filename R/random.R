#' random
#'
#' Returns a index from \code{1:length(v)} randomly ordered.
#'
#' @param v vector: vector with elements
#'
#' @return index
#' @export
#'
#' @examples
#' random(-3:3)
random <- function(v) {
  sample(length(v), size=length(v)) 
}