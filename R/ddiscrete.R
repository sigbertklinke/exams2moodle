#' ddiscrete
#'
#' Creates a discrete probability function based on \code{x} with resolution \code{unit}. 
#' If \code{unit} is not given then unit will be 10, 100, 1000, ... depending 
#' on the length of the discrete probability function. 
#'
#' @param x numeric: number of elements of vector of initial probabilities
#' @param unit integer: reciprocal of smallest non-zero probability (default: \code{NULL})
#' @param zero logical: zeros in the final probabilities allowed (default: \code{FALSE})
#'
#' @return a discrete probability function
#' @export
#'
#' @examples
#' ddiscrete(runif(6))
#' ddiscrete(6)
#' ddiscrete(6, 20)
#' ddiscrete(c(1,0,0,0), zero=TRUE)
ddiscrete <- function(x, unit=NULL, zero=FALSE) {
  if (length(x)<2) x <- runif(as.integer(x))
  if (is.null(unit)) unit <- 10^ceiling(0.2+log10(length(x)))
  unit <- as.integer(unit)
  fx   <- as.integer(x/sum(x)*unit)
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

