#' binom_param
#'
#' @param n integer: number of observations
#' @param p numeric: vector of probabilities
#' @param mean integer: number of digits the mean should have
#' @param sd integer: number of digits the standard deviation should have
#' @param norm logical: normal approximation possible
#' @param pois logical: poisson approximation possible
#'
#' @return a data frame with possible choices of `n` , `p`, `mean` and `sd` 
#' @export
#'
#' @examples
#' binom_param(1000:50000, (5:25)/100, 0, 0)
binom_param <- function(n, p, mean=NA, sd=NA, norm=NA, pois=NA) {
  if (is.na(mean)) {
    mean <- ceiling(mean(log10(c(min(n)*min(p), max(n)*max(p)))))
    if (mean>0) mean <- 0 else mean <- 1-mean
  }
  mean <- round(mean)
  if (is.na(sd)) sd <- mean
  res <- list(n=numeric(0), p=numeric(0), mean=numeric(0), sd=numeric(0))
  for (i in 1:length(p)) {
    m   <- n*p[i]
    s   <- sqrt(n*p[i]*(1-p[i]))
    ind <- which(has_digits(m, mean) & has_digits(s, sd)) 
    if (length(ind)) {
      res$n <- c(res$n, n[ind])
      res$p <- c(res$p, rep(p[i], length(ind)))
      res$mean <- c(res$mean, m[ind])
      res$sd   <- c(res$sd, s[ind])
    }
  }
  res <- as.data.frame(res)
  if (isTRUE(norm))  res <- res[res$sd>9,]
  if (isFALSE(norm)) res <- res[res$sd<9,]
  if (isTRUE(pois))  res <- res[(res$n>10) & (res$p<0.05),]
  if (isFALSE(pois)) res <- res[(res$n<10) | (res$p>0.05),]
  res
}