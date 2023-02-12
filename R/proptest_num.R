#' @title ptest_num
#' @description Computes all results for test on proportion using either [stats::binom.test()] or 
#' a normal approximation without continuity correction.
#' Either named parameters can be given or a `arglist` with the parameters.
#' * `x` number of successes
#' * `n` sample size (default: `sd(x)`) 
#' * `pi0` true value of the proportion (default: `0.5`)
#' * `alternative` a string specifying the alternative hypothesis (default: `"two.sided"`), otherwise `"greater"` or `"less"` can be used
#' * `alpha` significance level (default: `0.05`)
#' * `binom2norm` can the binomial distribution approximated a normal distribution (default: `NA` = use `binom2norm` function)
#' @param ...  named input parameters
#' @param arglist list: named input parameters, if given `...` will be ignored
#' @details The results of `proptest_num` may differ from [stats::binom.test()]. `proptest_num` is designed to return results
#' when you compute a binomial test by hand. For example, for computing the test statístic the approximation \eqn{t_n \approx N(0; 1)} 
#' is used if \eqn{n>n.tapprox}. The `p.value` is computed by [stats::binom.test] and may not be reliable, for Details see Note!
#' @note The computation of a p-value for non-symmetric distribution is not well defined, see \url{https://stats.stackexchange.com/questions/140107/p-value-in-a-two-tail-test-with-asymmetric-null-distribution}.
#' @return a list with the input parameters and
#' * `X` distribution of the random sampling function
#' * `Statistic` distribution of the test statistics 
#' * `statistic` test value
#' * `critical` critical value(s)
#' * `criticalx` critical value(s) in x range
#' * `acceptance0` acceptance interval for H0
#' * `acceptance0x` acceptance interval for H0 in x range
#' * `accept1` is H1 accepted?
#' * `p.value` p value for test (note: the p-value may not be reliable see Notes!)
#' * `alphaexact` exact significance level
#' * `stderr` standard error of the proportion used as denominator
#' @importFrom stats binom.test dbinom
#' @export
#' @md
#' @examples
#' n <- 100
#' x <- sum(runif(n)<0.4)
#' proptest_num(x=x, n=n)
proptest_num <- function(..., arglist=NULL) {
  ret  <- list(pi0=0.5, x=NA, n=NA_integer_, alternative="two.sided",
               X=NA, Statistic=NA, statistic=NA, p.value=NA, stderr=NA, 
               binom2norm=NA, alphaexact=NA, alpha=0.05,
               critical=NA, acceptance0=NA, criticalx=NA, acceptance0x=NA,
               accept1=NA
               )
  if(is.null(arglist)) arglist <- list(...)
  stopifnot(nchar(names(arglist))>0)
  for (name in names(arglist)) ret[[name]] <- arglist[[name]][sample(length(arglist[[name]]))]
  alt <- pmatch (ret$alternative, c("two.sided", "less", "greater"))
  # browser()
  stopifnot(is.finite(alt))
  stopifnot(is.finite(ret$x))
  stopifnot(is.finite(ret$n))
  stopifnot((ret$pi0>0) && (ret$pi0<1))
  #
  ret$alternative <- c("two.sided", "less", "greater")[alt]
  # Xbar, Statistic
  ret$X      <- distribution("binom", size=ret$n, prob=ret$pi0)
  ret$stderr <- sqrt(ret$pi0*(1-ret$pi0)/ret$n)
  if(is.na(ret$binom2norm)) ret$binom2norm <- binom2norm(ret$n, ret$pi0) 
  if (ret$binom2norm) {
    ret$Statistic <- distribution("norm", mean=0, sd=1)
    ret$statistic <- (ret$x/ret$n-ret$pi0)/ret$stderr
    if(alt==1) {
      ret$critical    <- quantile(ret$Statistic, c(ret$alpha/2, 1-ret$alpha/2))
      ret$acceptance0 <- ret$critical
      ret$p.value     <- 2*(if (ret$statistic<0) cdf(ret$Statistic, ret$statistic) else 1-cdf(ret$Statistic, ret$statistic))
    }     
    if(alt==2) {
      ret$critical    <- quantile(ret$Statistic, ret$alpha)
      ret$acceptance0 <- c(ret$critical, (1-ret$pi0)/ret$stderr)
      ret$p.value     <- 1-cdf(ret$Statistic, ret$statistic)
    }     
    if(alt==3) {
      ret$critical    <- quantile(ret$Statistic, 1-ret$alpha)
      ret$acceptance0 <- c(-ret$pi0/ret$stderr, ret$critical)
      ret$p.value     <- cdf(ret$Statistic, ret$statistic)
    }    
    ret$criticalx    <- ret$n*(ret$pi0+ret$stderr*ret$critical)
    ret$acceptance0x <- ret$n*(ret$pi0+ret$stderr*ret$acceptance0)
    ret$accept1      <- (ret$p.value<ret$alpha)

  } else {
    ret$Statistic <- ret$X
    ret$statistic <- ret$x
    cdf           <- cdf(ret$Statistic, 0:ret$n)
    exacttest     <- binom.test(x=ret$statistic, n=ret$n, p=ret$pi0, alternative=ret$alternative)
    if(alt==1) {
      ret$criticalx    <- c(sum(cdf<ret$alpha/2), ret$n-sum(cdf>1-ret$alpha/2)+1)
      ret$acceptance0x <- ret$criticalx
      ret$accept1      <- (ret$statistic<ret$criticalx[1]) || (ret$statistic>ret$criticalx[2])
    }    
    if (alt==2) { # less
      ret$criticalx    <- sum(cdf<ret$alpha)
      ret$acceptance0x <- c(ret$criticalx, ret$n)      
      ret$accept1      <- (ret$statistic<ret$criticalx)
    }
    if (alt==3) { # greater
      ret$criticalx    <- ret$n-sum(cdf>1-ret$alpha)+1
      ret$acceptance0x <- c(0, ret$criticalx)   
      ret$accept1      <- (ret$statistic>ret$criticalx)
      
    }      
    ret$acceptance0 <- ret$acceptance0x/ret$n
    ret$critical    <- ret$criticalx/ret$n
    ret$alphaexact  <- sum(dbinom(setdiff(0:ret$n, ret$acceptance0x[1]:ret$acceptance0x[2]), size=ret$n, prob=ret$pi0))
    ret$p.value     <- exacttest$p.value
    #ret$accept1     <- (ret$p.value<ret$alpha)
  }
  structure(ret, class=c("proptest", class(ret)))
}