#' sqrtnp
#'
#' Computes \code{sqrt(n*p*(1-p))} for all combinations of \code{n} and \code{p}. 
#' If the result has only \code{digits} after the decimal point then these \code{n}, \code{p}, 
#' and \code{sqrt(n*p*(1-p))} is returned in a data frame.
#' 
#' If \code{abs(v-round(v, digits))<tol} then a number \code{v} is consired as a number 
#' with only \code{digits} after the decimal point.
#' 
#' @param n numeric: vector of observations numbers
#' @param p numeric: vector of probabilities
#' @param digits numeric: number of digits to check (default: \code{2})
#' @param tol numeric: tolerance (default: \code{10^(-digits-4)})
#'
#' @return a data frame with the columns \code{n}, \code{p}, \code{np} (\eqn{=np}) and \code{snp} (\eqn{=sqrt(np(1-p))})
#' @export
#'
#' @examples
#' n <- 30:250
#' p <- (10:40)/100
#' sqrtnp(n, p)
sqrtnp <- function(n, p, digits=2, tol=10^(-digits-4)) {
  pp  <- outer(n, p*(1-p))
  spp <- sqrt(pp)
  ii  <- which(has_digits(spp, digits=digits, tol=tol))
  df    <- data.frame(n=n[row(pp)[ii]], p=p[col(pp)[ii]])
  df$np <- df$n*df$p
  df$snp <- sqrt(df$n*df$p*(1-df$p))
  df
}