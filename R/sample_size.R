#' @rdname sample_size 
#' @title sample_size_freq
#' @description Checks if a vector of possible sample sizes and relative frequencies create integer absolute frequencies.
#' @param n numeric: vector of sample size(s) to check
#' @param f numeric: vector of relative frequencies
#' @param which numeric: if several `n`s are possible then return the `which`th one (default: `NA` = choose randomly one)
#'
#' @return one sample size
#' @export
#'
#' @examples
#' f <- ddiscrete(runif(5), unit=100)
#' sample_size_freq(seq(10, 200, 1), f)
#' sample_size_freq(seq(10, 200, 1), f, which=200)
sample_size_freq <- function(n, f, which=NA) {
  n <- n[apply(outer(n, f), 1, all_integer)]
  stopifnot(length(n)>0)
  if (length(n)==1) return(n)
  if (is.na(which)) return(sample(n, 1))
  i <- as.integer(which)
  if (i<1) i <- 1
  if (i>length(n)) i <- length(n)
  n[i]
}