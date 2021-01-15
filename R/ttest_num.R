#' @title ttest_num
#' @description Computes all results for a t-test. Note that the results may differ from [stats::t.test()], see the details.
#' Either named parameters can be given or a `list` with the parameters.
#' You must provide either `x` or `mean`, `sd` and `n`. If `x` is given then any values 
#' given for `mean`, `sd` and `n` will be overwritten. Also either `sd` or `sigma` or both must be given. 
#' * `x` sample (default: `numeric(0)`)
#' * `mean` sample mean (default: `mean(x)`)
#' * `n` sample size (default: `sd(x)`) 
#' * `sd` sample standard deviation (default: `length(x)`)
#' * `sigma` population standard deviation (default: `NA` = unknown)
#' * `mu0` true value of the mean (default: `0`)
#' * `alternative` a string specifying the alternative hypothesis (default: `"two.sided"`), otherwise `"greater"` or `"less"` can be used
#' * `alpha` significance level (default: `0.05`)
#' * `n.clt` when central limit theorem holds (default: `getOption("n.clt", 30)`)
#' * `t2norm` does the approxmation \eqn{t_n \approx N(0;1)} hold? `(default: `NA` = use `t2norm` function)
#' @param ...  named input parameters
#' @param arglist list: named input parameters, if given `...` will be ignored
#' @details The results of `ttest_num` may differ from [stats::t.test()]. `ttest_num` is designed to return results
#' when you compute a t test by hand. For example, for computing the test statístic the approximation \eqn{t_n \approx N(0; 1)} 
#' is used if \eqn{n>n.tapprox}. The `p.value` is computed from the cumulative distribution function of the normal or 
#' the t distribution.
#' @return a list with the input parameters and
#' * `Xbar` distribution of the random sampling function \eqn{\bar{X}}, only available if `sigma` given
#' * `Statistic` distribution of the test statistics 
#' * `statistic` test value
#' * `critical` critical value(s)
#' * `criticalx` critical value(s) in x range
#' * `acceptance0` acceptance interval for H0
#' * `acceptance0x` acceptance interval for H0 in x range
#' * `accept1` is H1 accepted?
#' * `p.value` p value for test
#' @importFrom stats sd
#' @export
#' @md
#' @examples
#' x <- runif(100)
#' ttest_num(x=x)
#' ttest_num(mean=mean(x), sd=sd(x), n=length(x))
#' ret <- ttest_num(x=x)
#' ret$alternative <- "less"
#' ttest_num(arglist=ret)
ttest_num<- function(..., arglist=NULL) {
  ret  <- list(mu0=0, x=numeric(0), sigma=NA, norm=FALSE, mean=NA, sd=NA, n=NA_integer_, alternative="two.sided",
               Xbar=NA, Statistic=NA, statistic=NA, p.value=NA, stderr=NA, 
               n.clt=getOption("n.clt", 30), t2norm=NA,
               critical=NA, acceptance0=NA, criticalx=NA, acceptance0x=NA, alpha=0.05
  )
  if(is.null(arglist)) arglist <- list(...)
  stopifnot(nchar(names(arglist))>0)
  for (name in names(arglist)) ret[[name]] <- arglist[[name]][sample(length(arglist[[name]]))]
  alt <- pmatch (ret$alternative, c("two.sided", "less", "greater"))
  if (length(ret$x)) {
    ret$mean <- mean(ret$x)
    ret$sd   <- sd(ret$x)
    ret$n    <- length(ret$x)
  }
  # browser()
  stopifnot(is.finite(alt))
  stopifnot(is.finite(ret$mean))  
  stopifnot(is.finite(ret$n))
  stopifnot(is.finite(ret$sigma) || is.finite(ret$sd))
  #
  ret$alternative <- c("two.sided", "less", "greater")[alt]
  ret$t2norm      <- t2norm(ret$n)
  # Xbar, Statistic
  if (ret$norm || (ret$n>=ret$n.clt)) { # GG is normal or ZGS holds
    sigmaknown <- !is.na(ret$sigma)
    if (sigmaknown) {
      ret$stderr    <- ret$sigma/sqrt(ret$n) 
      ret$Xbar      <- distribution("norm", mean=ret$mu0, sd=ret$stderr)
      ret$statistic <- (ret$mean-ret$mu0)/ret$stderr
      ret$Statistic <- distribution("norm", mean=0, sd=1)
    } else {
      ret$stderr    <- ret$sd/sqrt(ret$n)
      ret$statistic <- (ret$mean-ret$mu0)/ret$stderr
      if (ret$t2norm) {
        ret$Statistic <- distribution("norm", mean=0, sd=1)
      } else {
        ret$Statistic <- distribution("t", df=ret$n-1)
      }       
    }
    if(alt==1) {
      ret$critical    <- quantile(ret$Statistic, c(ret$alpha/2, 1-ret$alpha/2))
      ret$acceptance0 <- ret$critical  
      ret$p.value     <- 2*(if (ret$statistic<0) cdf(ret$Statistic, ret$statistic) else 1-cdf(ret$Statistic, ret$statistic))
    }
    if(alt==2) {
      ret$critical     <- quantile(ret$Statistic, ret$alpha)        
      ret$acceptance0  <- c(-Inf, ret$critical) 
      ret$p.value      <- 1-cdf(ret$Statistic, ret$statistic)
    }
    if (alt==3) {
      ret$critical    <- quantile(ret$Statistic, 1-ret$alpha)            
      ret$acceptance0 <- c(ret$critical, +Inf) 
      ret$p.value     <- cdf(ret$Statistic, ret$statistic)
    }
    ret$criticalx    <- ret$mu0 + ret$critical * ret$stderr
    ret$acceptance0x <- ret$mu0 + ret$acceptance0 * ret$stderr
    ret$accept1      <- as.logical(ret$p.value<ret$alpha)
  }
  else stop("t test can not be computed")
  structure(ret, class=c("ttest", class(ret)))
}