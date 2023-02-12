#' q2norm
#'
#' Given two (or more) quantiles it computes a (approximate) mean and standard deviation for a corresponding normal distribution. 
#'
#' @param x numeric(2): the quantiles
#' @param probs numeric(2): probabilities with values in \eqn{[0,1]} (default: `c(0.025, 0.975)`)
#'
#' @return a list with a component `mean` and `sd`
#' @export
#'
#' @examples
#' q2norm(c(100,200))
q2norm <- function(x, probs=c(0.025, 0.975)) {
  stopifnot(length(x)==length(probs))
  lr <- lm(x~qnorm(probs))
  list(mean=as.numeric(lr$coefficients[1]), sd=as.numeric(lr$coefficients[2]))
}