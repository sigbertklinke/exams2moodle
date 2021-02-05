#' ddiscrete2
#'
#' Creates a bivariate discrete probability function based on the marginal probability fuctions 
#' \code{row} and \code{col}. If \code{unit} is not given then unit will be the product of the units
#' used in \code{row} and \code{col} otherwise the least common multiple of \code{unit} product of 
#' the units used in \code{row} and \code{col}. \code{zero} allows for zero entries in the common
#' distribution.
#' If \code{target} is \code{NA} then the common distribution of two independent random variables
#' is returned otherwise an iterative algorithm is run to approach a \code{target} association or 
#' correlation measure. \code{FUN} computes the association or correlation measure based on a
#' frequency table. \code{tol} gives the maximal deviation of the association or correlation measure
#' and the \code{target} value. \code{maxit} limits the number of steps.
#' Note that a solution is not guaranteed, especially for extreme values for \code{target}, for example
#' for +1 or -1 or values nearby.
#' If \code{attr(joint, "iterations")==maxit} then you need 
#' either to increase \code{maxit}, to decrease \code{tol} or to check if you have choosen an 
#' appropriate \code{target} value (for a nominal measure in [0,1], for ordinal measure in [-1, 1]).
#'
#' @param row numeric: marginal row distribution
#' @param col numeric: marginal col distribution
#' @param unit integer: reciprocal of smallest non-zero probability (default: \code{NULL})
#' @param zero logical: zeros in the final probabilities allowed (default: \code{FALSE})
#' @param FUN function: association or correlation function (default: \code{nom.cc})
#' @param target numeric: target association or correlation (default: \code{NA})
#' @param tol numeric: tolerance for target association or correlation (default: \code{0.001})
#' @param maxit integer: maximal number of iterations (default: \code{100})
#' @param ... further parameter for \code{FUN}
#' 
#' @return a discrete probability function
#' @export
#'
#' @examples
#' row <- ddiscrete(runif(5))
#' col <- ddiscrete(runif(3))
#' joint <- ddiscrete2(row, col)
#' joint
#' joint <- ddiscrete2(row, col, target=0.5)
#' joint
#' nom.cc(joint*attr(joint, "unit"))
ddiscrete2 <- function(row, col, unit=NULL, zero=FALSE, FUN=nom.cc, target=NA, tol=0.001, maxit=500, ...) {
  lcm <- function(x) {
    xm <- x <- as.integer(x)
    while(!all(xm==xm[1])) {
      ind     <- which(xm==min(xm))
      xm[ind] <- xm[ind] + x[ind]
    } 
    xm[1]
  }
  #
  #browser()
  stopifnot(is.na(target) || abs(target)<=1)
  rowunit <- attr(row, "unit")
  colunit <- attr(col, "unit")
  if (is.null(rowunit)) {
    fracs   <- attr(fractions(row), "fracs")
    units   <- as.integer(sapply(strsplit(fracs, "/", fixed=TRUE), '[[', 2))
    rowunit <- lcm(units)
  }
  if (is.null(colunit)) {
    fracs   <- attr(fractions(col), "fracs")
    units   <- as.integer(sapply(strsplit(fracs, "/", fixed=TRUE), '[[', 2))
    colunit <- lcm(units)
  } 
  unit <- lcm(c(unit, rowunit*colunit))
  fx   <- matrix(as.integer(unit*(row %o% col)), ncol=length(col), nrow=length(row))
  it   <- 0
  curr <- 0
  if (!is.na(target)) {
    # browser()
    fun  <- match.fun(FUN)
    curr <- fun(fx/unit, ...)
    d    <- abs(curr-target)
    if (!equal(d,0, tol)) {
      while(it<maxit) {
        i    <- sample(length(row), size=2)
        j    <- sample(length(col), size=2)
        doit <- (if(zero) all(fx[i,j]>=0) else all(fx[i,j]>0)) && all(fx[i,j]<=unit)
        if (doit) {
          fxt <- fx
          fxt[i[1], j[1]] <- fxt[[i[1], j[1]]]+1
          fxt[i[1], j[2]] <- fxt[[i[1], j[2]]]-1
          fxt[i[2], j[1]] <- fxt[[i[2], j[1]]]-1
          fxt[i[2], j[2]] <- fxt[[i[2], j[2]]]+1
          curr <- fun(fxt/unit, ...)
          dt   <- abs(curr-target)
          if (dt<d) {
            d    <- dt
            fx   <- fxt
            if (equal(d,0, tol)) break
          }
        }
        it <- it+1
      }
    }
  }
  structure(fx/unit, unit=unit, iterations=it, target=curr)
}