#' grouped_data
#'
#' Computes mean, mode or quantile/median of grouped data. 
#'
#' @param x numeric: borders
#' @param n numeric: absolute frequencies for each group
#' @param compute numeric/character: coefficint to compute 
#' @param tol numeric: tolerance for numerical comparison
#'
#' @return a list with the class, result and a table
#' @export
#'
#' @examples
#' x <- 1:4
#' n <- ddiscrete(runif(3))
#' grouped_data(x, n)
grouped_data <- function(x, n, compute=c("mean", "fine", "coarse"), tol=1e-6) {
  stopifnot(length(x)==length(n)+1)
  x  <- sort(x)
  xl <- x[-length(x)]
  xu <- x[-1]
  xm <- (xl+xu)/2
  xw <- xu-xl
  f  <- n/sum(n)
  cf  <- cumsum(f)
  fk <- f/(xu-xl)
  tab <- cbind(xl, xu, xm, xw, n, f, cf, fk)
  rownames(tab) <- 1:nrow(tab)
  colnames(tab) <- c("lower", "upper", "mid", "width", "absfreq", "relfreq", "cumfreq", "density")
  if (is.numeric(compute)) { # quantile
    stopifnot((compute>0) && (compute<1))
    group  <- which(cf>=compute-tol)[1]
    cf     <- c(0, cf)
    result <- xl[group]+(compute-cf[group])/f[group]*xw[group]
  }
  if (is.character(compute)) {
    compute <- match.arg(compute)
    if (compute=="mean") {
      group  <- NA
      result <- sum(xm*f)
    }
    if (compute=="coarse") {
      o <- order(fk, decreasing = TRUE)
      stopifnot(fk[o[1]]>fk[o[2]]+tol)
      group  <- o[1]
      result <- xm[group]    
    }
    if (compute=="fine") {
      o <- order(fk, decreasing = TRUE)
      stopifnot(fk[o[1]]>fk[o[2]]+tol)
      group  <- o[1]
      fk     <- c(0, fk, 0)
      result <- xl[group]+xw[group]*(fk[group+1]-fk[group])/(2*fk[group+1]-fk[group+1]-fk[group])    
    }
  }
  list(result=result, group=group, tab=tab, compute=compute)
}