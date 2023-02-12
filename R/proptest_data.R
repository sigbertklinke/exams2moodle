#' proptest_data 
#'
#' Creates data for a Binomial test based on the properties for the test.
#'
#' @param size numeric: vector of sample sizes (default \code{10:100})
#' @param prob numeric: vector of probabilities for the hypothetical proportion \eqn{\pi_0} (default \code{=seq(0.05, 0.45, by=0.05)})
#' @param reject logical: should `x` generated lead to a rejection of the null hypothesis (default \code{TRUE}), if equals \code{NA} then this will be igenored
#' @param alternative character: a character string specifying the alternative hypothesis, must be one of \code{two.sided} (default), \code{greater} or \code{less} 
#' @param alpha numeric: vector of significance levels (default \code{c(0.01, 0.05, 0.1)})
#' @param norm.approx logical: should a normal approximation possible (\eqn{size*prob*(1-prob)>9})
#' @param maxit integer: maximal numbers of trials to find a solution (default \code{1000})
#'
#' @return a list with the components:
#' * \code{pi0} hypothetical proportion,
#' * \code{x} counts of successes in the sample,
#' * \code{n} sample size,
#' * \code{alpha} significance level, and
#' * \code{alternative} specifying the alternative hypothesis (either \code{two.sided}, \code{greater} or \code{less}).
#' 
#' @importFrom stats pbinom
#' @export
#'
#' @examples
#' proptest_data()
proptest_data <- function(size=10:100, prob=seq(0.05, 0.45, by=0.05), reject=TRUE, alternative=c("two.sided", "less", "greater"), alpha=c(0.01, 0.05, 0.1), 
                          norm.approx=NA, maxit=1000) {
  stopifnot(length(size)>2, length(prob)>2)
  alternative <- match.arg(alternative)
  i <- 0
  repeat {
    #browser()
    n     <- sample(size, 1) 
    siglv <- if (length(alpha)==1) alpha else sample(alpha, 1)
    pi0   <- sample(prob, 1)
    x     <- setdiff(floor(min(prob)*n):ceiling(max(prob)*n), 1)
    if (is.na(reject)) {
      xp <- rep(TRUE, length(x))
    } else {
      if (alternative=="two.sided") xp <- (pbinom(x, n, pi0)>= siglv/2) & (pbinom(x, n, pi0)<=1-siglv/2)
      if (alternative=="less")      xp <- (pbinom(x, n, pi0)>= siglv) 
      if (alternative=="greater")   xp <- (pbinom(x, n, pi0)<= 1-siglv) 
      if (reject) xp <- !xp  
    }
    if (sum(xp)) {
      x <- sample(x, size=1, prob=xp)
      if (is.na(norm.approx)) break
      if (norm.approx && (n*pi0*(1-pi0)>9)) break
      if (!norm.approx && (n*pi0*(1-pi0)<9)) break
    }
    i <- i+1
    stopifnot(i!=maxit)
  }
  list(pi0=pi0, x=x, n=n, alpha=siglv, alternative=alternative)
}