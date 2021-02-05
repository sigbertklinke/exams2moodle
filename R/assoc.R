#' @rdname assoc
#' @title Association
#' @description Computation of association and correlation measure:
#' * \code{nom.cc} (corrected) contingency coefficent
#' * \code{nom.cramer} Cramer's V or Phi
#' * \code{ord.spearman} Spearman's rank correlation
#' * \code{ord.kendall} Kendall's rank correlation
#'
#' @param tab table: contingency table with absolute frequencies
#' @param correct logical: if a correction should be applied (default: \code{FALSE})
#' @param ... further parameters (unused)
#'
#' @return numeric
#' @export
#' @md
#'
#' @examples
#' tab <- matrix(round(10*runif(15)), ncol=5)
#' nom.cc(tab)
#' nom.cc(tab, correct=TRUE)
#' nom.cramer(tab)
#' ord.spearman(tab)
#' ord.kendall(tab)
nom.cc <- function(tab, correct=FALSE) {
  nthroot <- function (x, n) {
    if (!is.numeric(x)) 
      stop("Argument 'x' must be numeric.")
    if (missing(n) || n <= 0 || ceiling(n) != floor(n)) 
      stop("Argument 'n' must be a positive integer.")
    if (any(x[!is.na(x)] < 0) && n%%2 == 0) 
      stop("If argument 'x' is negative, 'n' must be an odd integer.")
    sx <- sign(x)
    return(sx * (sx * x)^(1/n))
  }
  #
  n     <- sum(tab)
  row   <- rowSums(tab)
  col   <- colSums(tab)
  expe  <- (row%o%col)/n 
  chi2  <- sum((tab-expe)^2/expe)
  coeff <- sqrt(chi2/(chi2+n))
  if (correct) {
    nr <- length(row)
    nc <- length(col)
    coeff <- coeff/nthroot((nr-1)/nr*(nc-1)/nc, 4)
  }
  coeff
}

#' @rdname assoc
#' @export
nom.cramer <- function(tab, ...) {
  n     <- sum(tab)
  row   <- rowSums(tab)
  col   <- colSums(tab)
  expe  <- (row%o%col)/n 
  chi2  <- sum((tab-expe)^2/expe)
  k    <- min(length(row), length(col))
  sqrt(chi2/(k*n))
}

#' @rdname assoc
#' @importFrom stats cor
#' @export
ord.spearman <- function(tab, ...) {
  wtab  <- as.data.frame.table(tab)
  index <- rep(1:nrow(wtab), wtab$Freq)
  utab  <- wtab[index,]
  cor(as.numeric(utab[,1]), as.numeric(utab[,2]), ..., method="spearman")
}
 
#' @rdname assoc
#' @importFrom stats cor
#' @export
ord.kendall <- function(tab, ...) {
  wtab  <- as.data.frame.table(tab)
  index <- rep(1:nrow(wtab), wtab$Freq)
  utab  <- wtab[index,]
  cor(as.numeric(utab[,1]), as.numeric(utab[,2]), ..., method="kendall")
} 