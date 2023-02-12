#' @rdname dunif2
#' @title Sum of two independent discrete uniform distributions
#' @description Probability mass function, distribution function, quantile function and random generation 
#' for the sum of two independent discrete uniform distribution.
#'
#' @param x,q numeric: vector of quantiles
#' @param p numeric: vector of probabilities
#' @param n numeric: number of observations. If \code{length(n)>1}, the length is taken to be the number required.
#' @param min numeric: lower limit of the distribution (default: \code{1})
#' @param max numeric: upper limit of the distribution (default: \code{6})
#'
#' @return numeric vector with the same length as \code{x}
#' @export
#'
#' @examples
#' ddunif2(1:13)
#' pdunif2(1:13)
#' qdunif2((0:4)/4)
#' rdunif2(10)
ddunif2 <- function (x, min=1, max=6) {
  d2 <- outer(min:max, min:max, "+")
  pd <- matrix(1, nrow=nrow(d2), ncol=ncol(d2))
  pd <- tapply(pd, d2, sum)/prod(dim(d2))
  dx <- pd[pmatch(x, names(pd))]
  dx[is.na(dx)] <- 0
  structure(dx, names=as.character(x))
}

#' @rdname dunif2
#' @export
pdunif2 <- function (q, min=1, max=6) {
  d2 <- as.vector(outer(min:max, min:max, "+"))
  structure(colSums(outer(d2, q, "<="))/length(d2), names=as.character(q))
}

#' @rdname dunif2
#' @export
qdunif2 <- function (p, min=1, max=6) {
  d2 <- outer(min:max, min:max, "+")
  pd <- matrix(1, nrow=nrow(d2), ncol=ncol(d2))
  pd <- cumsum(tapply(pd, d2, sum)/prod(dim(d2)))
  pos <- 1+rowSums(outer(p, pd, ">="))
  npd <- as.numeric(names(pd))
  pos[pos>length(npd)] <- length(npd)
  ret <- npd[pos]
  ret[(p<0) | (p>1)] <- NA
  ret
}

#' @rdname dunif2
#' @export
rdunif2 <- function (n, min=1, max=6) {
  d2 <- outer(min:max, min:max, "+")
  sample(d2, size=if(length(n)>1) length(n) else n, replace=TRUE)
}