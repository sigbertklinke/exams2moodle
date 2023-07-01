#' @rdname nobs
#' @aliases nobs_25
#' @aliases nobs_sq
#' @title Number of observations
#' @description Generates a sequence of sample sizes in a range from `min=5` to `max` 
#' * whose root is an integer (`nobs_sq`), and
#' * that are divisible only by 2 and 5 (`nobs_25`)
#'
#' @param max integer: maximum sample size
#' @param min integer: minimum sample size (default: `5`)
#'
#' @return a sequence of integers
#' @export
#'
#' @examples
#' nobs(10)
#' nobs_sq(1000)
#' nobs_25(1000)
nobs <- function(max, min=5) {
  ceiling(min):floor(max)
}
  
#' @rdname nobs
#' @export
nobs_sq <- function(max, min=5) {
  nobs <- sqrt(round(c(min, max)))
  (ceiling(nobs[1]):floor(nobs[2]))^2
}

#' @rdname nobs
#' @export
nobs_25 <- function(max, min=5) {
  nobs <- round(min):round(max)
  nobs[divisor_25(nobs)]
}