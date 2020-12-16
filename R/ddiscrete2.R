#' ddiscrete
#'
#' Creates a bivariate discrete probability function based on \code{x} with resolution \code{unit}. 
#' If \code{unit} is not given then unit will be 10, 100, 1000, ... depending on the length of 
#' the discrete probability function. 
#'
#' @param n numeric(2): rows and columns of the joint probability
#' @param unit integer: reciprocal of smallest non-zero probability (default: \code{NULL})
#' @param zero logical: zeros in the final probabilities allowed (default: \code{FALSE})
#'
#' @return a discrete probability function
#' @export
#'
#' @examples
#' ddiscrete2(c(2,3))
#' ddiscrete2(c(2,3), unit=100)
#' ddiscrete2(c(2,3), unit=10, zero=TRUE)
ddiscrete2 <- function(n, unit=NULL, zero=FALSE) {
  lx <- prod(n)
  if (is.null(unit)) unit <- 10^ceiling(0.2+log10(lx))
  unit <- as.integer(unit)
  x    <- runif(lx)
  fx   <- matrix(as.integer(x/sum(x)*unit), nrow=n[1], ncol=n[2])
  if (!zero) fx[fx==0] <- 1
  while(abs(sum(fx)-unit)>0.05) {
    i <- sample(length(fx), 1)
    if (sum(fx)>unit) {
      if (fx[i]>!zero) fx[i] <- fx[i]-1
    } else {
      fx[i] <- fx[i]+1
    }
  }
  fx/unit
}