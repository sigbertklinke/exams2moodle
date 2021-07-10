#' table_data
#'
#' Creates a frequency table where all entries can be written as \eqn{2^{p_{ij}} 5^{q_{ij}}}, it holds that \eqn{p_{ij}<m2} and  \eqn{q_{ij}<m5}.
#' If the algorithm does not find a solutio then an error is thrown. Try to increase `unit` to 20, 50, 100 and so on. 
#' Once a table is found the table is normalized by dividing all entries by a number such that the entries are still integer. 
#' Finally a multiplicator of the form \eqn{2^p 5^5} is randomly choosen such that the sum of the entries is less equal `n`.  
#' 
#' @param nrow integer: number of rows
#' @param ncol integer: number of columns
#' @param unit integer: reciprocal of smallest non-zero probability (default: \code{10})
#' @param maxit integer: maximal number of iterations (default: \code{1000})
#' @param n integer: maximal sum of table entries (default: \code{100})
#' @param m2 integer: maximal power of two used on normalized table (default: \code{ceiling(log(n)/log(2))})
#' @param m5 integer: maximal power of five used on normalized table  (default: \code{ceiling(log(n)/log(5))})
#'
#' @md
#' @return a frequency table where all entries can be written as \eqn{2^{p_{ij}} 5^{q_{ij}}}
#' @export
#'
#' @examples
#' tab22 <- table(2, 2)
#' tab22
#' divisor_25(tab22)
#' nom.cc(tab22)         # should be zero
#' #
#' table(3, 2)
#' table(4, 2)
table_data <- function(nrow, ncol, unit=10, maxit=1000, n=100, m2=ceiling(log(n)/log(2)), m5=ceiling(log(n)/log(5))) {
  i <- 0
  while (i<maxit) {
    m    <- outer(ddiscrete(nrow, unit=unit), ddiscrete(ncol, unit=unit))
    if (all(divisor_25(unit^2*m))) break
    i <- i+1
  }
  stopifnot(i<maxit)
  x   <- as.vector(unit^2*m)
  # normalize
  repeat {
    by2 <- (x%%2)==0
    by5 <- (x%%5)==0
    if (all(by2)) x <- x%/%2
    if (all(by5)) x <- x%/%5
    if (all(by2) || all(by5)) break
  }
  nx  <- sum(x)*outer(2^(0:m2), 5^(0:m5))
  m   <- 1
  ind <- which(nx<=n, arr.ind = TRUE)
  if (length(ind)>2) {
    ind <- ind[sample(1:nrow(ind), 1),]
    m   <- nx[ind[1], ind[2]]/sum(x)
  }
  structure(matrix(m*x, ncol=ncol), maxit=i)
}
