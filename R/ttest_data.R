#' ttest_data 
#'
#' Creates data for a t test for one mean based on the properties for the test.
#'
#' @param size numeric: vector of possible sample sizes (default \code{(3:20)^2,})
#' @param mean numeric: vector of possible means (default \code{-5:5})
#' @param sd numeric: vector of possible standard deviations (default \code{sd=seq(0.1, 1, by=0.1})
#' @param reject logical: should `x` generated lead to a rejection of the null hypothesis (default \code{TRUE}), if equals \code{NA} then this will be igenored
#' @param alternative character: a character string specifying the alternative hypothesis, must be one of \code{two.sided} (default), \code{greater} or \code{less} 
#' @param alpha numeric: vector of significance levels (default \code{c(0.01, 0.05, 0.1)})
#' @param z numeric: vector of possible \eqn{z} values (default \code{seq(-4.49, 4.49, by=0.01)})
#' @param use.sigma logical: should the standard deviation of the population (default) or the sample used 
#'
#' @return a list with the components:
#' * \code{mu0} hypothetical mean,
#' * \code{sigma} standard deviation in the population,
#' * \code{sd} vector of possible standard deviations in the sample,
#' * \code{xbar} mean in the sample, 
#' * \code{n} sample size, 
#' * \code{alpha} significance level, 
#' * \code{alternative} specifying the alternative hypothesis (either \code{two.sided}, \code{greater} or \code{less}), and
#' * \code{altsd} alternative values usable for \code{sd} (if \code{use.sigma==TRUE}) or \code{sigma} (if \code{use.sigma==FALSE})
#' 
#' @importFrom stats pnorm
#' @export
#'
#' @examples
#' ttest_data()
ttest_data <- function(size=(3:20)^2, mean=-5:5, sd=seq(0.1, 1, by=0.1), reject=NA, alternative=c("two.sided", "less", "greater"), 
                       alpha=c(0.01, 0.05, 0.1), z=seq(-4.49, 4.49, by=0.01), use.sigma=TRUE) {
  stopifnot(length(size)>2, length(mean)>2, length(sd)>2)
  alternative <- match.arg(alternative)
  sds <- expand.grid(sd=sd, size=size)
  sds <- sds[is_terminal(sds$sd/sqrt(sds$size)),]
  stopifnot(nrow(sds)>0)
  repeat {
    ind   <- sample(nrow(sds), 1)
    n     <- sds$size[ind]  
    sigma <- sds$sd[ind]  
    ind2  <- which(sds$size==n)
    sd    <- sds$sd[if(length(ind2)==1) ind2 else sample(ind2, 1)] 
    #
    mu    <- sample(mean, size=1)
    siglv <- if (length(alpha)==1) alpha else sample(alpha, 1)
    if (!is.na(reject)) {
      if (alternative=="two.sided") zp <- (pnorm(z)>=siglv/2) & (pnorm(z)<=1-siglv/2/2)
      if (alternative=="less")      zp <- (pnorm(z)>=siglv) 
      if (alternative=="greater")   zp <- (pnorm(z)<=1-siglv) 
      if (reject) zp <- !zp
    } else {
      zp <- rep(TRUE, length(z))
    }
    z <- z[zp]
    x <- z*(if (use.sigma) sigma else sd)/sqrt(n)+mu
    x <- x[is_terminal(x)]
    if(length(x)>0) break
  }
  x <- if (length(x)==1) x else sample(x, 1)
  list(mu0=mu, sigma=sigma, xbar=x, sd=sd, n=n, alpha=siglv, altsd=sds$sd[which(sds$size==n)], alternative=alternative)
}
