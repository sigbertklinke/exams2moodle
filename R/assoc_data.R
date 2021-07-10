#' @title assoc_data
#' @description Given a frequency table the function reorders the observations such that the given `target` association will be approximated and the marginal
#' frequencies are unchanged. Note that the `target` association may not be reached!
#' \code{zero} allows for zero entries in the common distribution.
#' If \code{target} is \code{NA} then the table is simply returned. \code{FUN} computes the association (or correlation) measure based on a
#' frequency table. \code{tol} gives the maximal deviation of the association (or correlation) measure
#' and the \code{target} value. \code{maxit} limits the number of steps.
#' Note that a solution is not guaranteed, especially for extreme values for \code{target}, for example
#' for +1 or -1 or values nearby.
#' If \code{attr(joint, "iterations")==maxit} then you need either to increase \code{maxit}, to decrease \code{tol} or 
#' to check if you have choosen an appropriate \code{target} value (for a nominal measure in \eqn{0 <= target <= 1}, for ordinal measure in \eqn{-1 <= target <= +1}).
#' \code{attr(joint, "target")} contains the achieved association.
#'
#' @param tab table: table of absolute frequencies
#' @param zero logical: zeros in the final probabilities allowed (default: \code{FALSE})
#' @param FUN function: association or correlation function (default: \code{nom.cc})
#' @param target numeric: target association or correlation (default: \code{NA})
#' @param tol numeric: tolerance for target association or correlation (default: \code{0.001})
#' @param maxit integer: maximal number of iterations (default: \code{100})
#' @param ... further parameter for \code{FUN}
#' 
#' @return a modified frequency table
#' @export
#'
#' @examples
#' tab <- table_data(3, 2)
#' tab
#' tab2 <- assoc_data(tab, target=0.5)
#' tab2
assoc_data <- function(tab, zero=FALSE, FUN=nom.cc, target=NA, tol=0.001, maxit=500, ...) {
  stopifnot(!is.null(dim(tab)))
  it   <- 0
  fun  <- match.fun(FUN)
  curr <- fun(tab, ...)
  if (!is.na(target)) {
    #browser()
    d    <- abs(curr-target)
    if (!equal(d,0, tol)) {
      n  <- sum(tab)
      while(it<maxit) {
        i    <- sample(nrow(tab), size=2)
        j    <- sample(ncol(tab), size=2)
        tabt <- tab
        tabt[i[1], j[1]] <- tabt[[i[1], j[1]]]+1
        tabt[i[1], j[2]] <- tabt[[i[1], j[2]]]-1
        tabt[i[2], j[1]] <- tabt[[i[2], j[1]]]-1
        tabt[i[2], j[2]] <- tabt[[i[2], j[2]]]+1
        doit <- all(if (zero) (tabt[i,j]>=0) else (tabt[i,j]>0), tabt[i,j]<=n)
        if (doit) {
          curr <- fun(tabt, ...)
          dt   <- abs(curr-target)
          if (dt<d) {
            d    <- dt
            tab  <- tabt
            if (equal(d,0, tol)) break
          }
        }
        it <- it+1
      }
    }
  }
  structure(tab, iterations=it, target=fun(tab, ...))
}