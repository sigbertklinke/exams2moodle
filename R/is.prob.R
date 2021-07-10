#' is.prob
#'
#' Checks if `x` is in an opened/closed interval between `min` and `max`. 
#' The defaults are choosen such that the interval is an the interval \eqn{(0,1)}, for example if `x` is a probability.
#'
#' @param x numeric: values to check
#' @param min numeric: minimal value (default: \code{0})
#' @param max numeric: maximal value (default: \code{1})
#' @param open logical(1): are the left and right borders are open or closed (default: \code{TRUE})
#'
#' @md
#' @return a logical vector with the same length as `x`
#' @export
#'
#' @examples
#' is.prob(runif(1))
is.prob <- function(x, open=TRUE, min=0, max=1)  {
  if (length(open)==1) open <- c(open, open)
  (if (open[1]) (x>min) else (x>=min)) & (if (open[2]) (x<max) else (x<=max))
}