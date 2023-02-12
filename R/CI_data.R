#' @importFrom stats qt
CImu_data <- function(x=NULL, n=length(x), xbar=NULL, conf.level=c(0.9, 0.95, 0.99), mu=NULL, sigma=NULL) {
  if (is.null(x)) {
    if (n<1) n <- 5
    if (length(sigma)>1) sigma <- sample(sigma, 1)
    if (length(mu)>1) mu <- sample(mu, 1)
    x <- rnorm(n, mean=mu, sd=sigma)
  } 
  if (is.null(xbar)) xbar <- mean(x)
  sd <- sd(x)
  ret <- list(a=1-(1-conf.level)/2, n=length(x), xbar=xbar, mu=mu, sd=sd, sigma=sigma, df=NULL)
  if (is.null(sigma)) {
    ret$q  <- qnorm(ret$a)
    ret$ss <- ret$sd
  } else {
    ret$df <- length(x)-1
    ret$q  <- qt(ret$a, ret$df)
    ret$ss <- ret$sigma
  }
  ret$e <- ret$q*ret$ss/sqrt(ret$n)
  ret$l <- 2*ret$e
  ret$v <- xbar+c(-ret$e, ret$e)
  ret
}


