#' CImulen_data
#'
#' Data generation for the necessary sample size of a confidence interval for the population mean value. 
#' Either the estimation error `e` or the length of the interval `l` must be given (\eqn{l=2*e}).
#' It is ensured that the computed `s` deviates from `sigma`.
#'
#' @param sigma numeric: vector of possible variance
#' @param e numeric: vector of estimation errors
#' @param l numeric: vector of lengths of the interval 
#' @param conf.level numeric: vector of confidence levels of the interval (default: `c(0.9, 0.95, 0.99)`)
#' @param nmin numeric: minimal value of necessary observation (default: `30`)
#' @param size numeric: sample size for computing a sample standard deviation. Default `NA` means that the solution of estimation is used
#' @param u numeric: vector of quantiles to sample the sample standard deviation (default: `c(seq(0.15, 0.45, 0.001), seq(0.55, 0.85, 0.001))`)
#' @param full logical: if `TRUE` then a data frame with possible solution is returned, otherwise a list with a randomly choosen solution (default: `FALSE`)
#'
#' @return a data frame or a list with
#' * `e` estimation error
#' * `sigma` population variance
#' * `conf.level` confidence level
#' * `l` interval length
#' * `x` \eqn{1-alpha/2}
#' * `q` \eqn{z_{1-alpha/2}}
#' * `q2` \eqn{z^2_{1-alpha/2}}
#' * `n` computed minimal sample size 
#' * `N` the smallest integer not less than `n`
#' * `s` sample standard deviation
#' @importFrom stats qchisq qnorm
#' @export
#'
#' @examples
#' # one solution
#' CImulen_data (1:10, e=(1:10)/10)
#' # all solutions
#' mul <- CImulen_data (1:10, e=(1:10)/10, full=TRUE)
#' str(mul)
CImulen_data <- function(sigma, e=NULL, l=NULL, conf.level=c(0.9, 0.95, 0.99), nmin=30, size=NA, 
                         u=c(seq(0.1, 0.4, 0.001), seq(0.6, 0.9, 0.001)),
                         full=FALSE) {
  stopifnot(xor(is.null(e), is.null(l)))
  if (is.null(e)) e <- l/2
  ret   <- expand.grid(e=e, sigma=sigma, conf.level=conf.level, KEEP.OUT.ATTRS = FALSE)
  res <- apply(ret, 1, function(r) { 
    a <- 1-(1-r[3])/2
    q <- qnorm(a)
    c(a=a, q=q, q2=q^2, n=q^2*r[2]^2/r[1]^2) 
  }) 
  ret$l  <- 2*ret$e
  ret$x  <- res[1,]
  ret$q  <- res[2,]
  ret$q2 <- res[3,] 
  ret$n  <- res[4,]
  ret$N  <- ifelse((abs(ret$n)%%1)>0, ceiling(ret$n), ret$n)
  stopifnot(any(ret$N>nmin))
  ret <- ret[ret$N>nmin,]
  if (is.na(size)) size <- ret$N
  ret$s  <- apply(cbind(size, ret$sigma), 1, function(r) { 
    sqrt(r[2]^2/(r[1]-1)*qchisq(sample(u, 1), df=r[1]-1))
  })
  if (full) return(ret)
  as.list(ret[sample(nrow(ret), 1),])
}
