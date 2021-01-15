#' scale_to
#'
#' Rescales \code{x} such that for the rescaled data holds: \code{mean(scale_to(x, mean=target))==target} and 
#' \code{sd(scale_to(x, sd=target)==abs(target)}. A negative value of \code{sd} will change the sign of the \code{x} values.
#'
#' @param x numeric: vector of values 
#' @param mean numeric: mean of rescales \code{x} (default: \code{0})
#' @param sd numeric: standard deviation of transformed \code{x} (default: \code{1})
#'
#' @return rescaled data
#' @export
#'
#' @examples
#' x <- runif(50)
#' y <- scale_to(x, mean=0.1, sd=0.2)
#' mean(y)
#' sd(y)
#' y <- scale_to(x, mean=0.1, sd=-0.2)
#' mean(y)
#' sd(y)
scale_to <- function(x, mean=0, sd=1) {
  x <- sd*x/sd(x)
  x-mean(x)+mean
}