#' sumofsquares
#'
#' Decomposes an integer `n` into a sum of squared integers (\eqn{n = \sum_{i=1}^k x_i^2}; \eqn{1\leq x_i <n}) 
#' with \eqn{k\leq nmax}. 
#' If `zerosum` is true then it is ensured that \eqn{\sum_{i=1}^k c_i x_i = 0} with \eqn{c_i=-1} or \eqn{c_i=+1}. 
#' The computation of the \eqn{x_i}'s is limited by `maxt` seconds which may result that not all possible
#' solutions are found. To reduce computing time, \code{rbind}'s in the function are replaced by allocating 
#' matrices with \code{size} rows to fill in the results. 
#' Note that the following data sets are available: 
#' * \code{sos100=sumofsquares(100, 10, zerosum=TRUE, maxt=Inf)}, 
#' * \code{sos200=sumofsquares(200, 10, zerosum=TRUE, maxt=Inf)}, 
#' * \code{sos400=sumofsquares(400, 10, zerosum=TRUE, maxt=Inf)}, 
#' * \code{sos800=sumofsquares(800, 10, zerosum=TRUE, maxt=Inf)}, and 
#' * \code{sos1000=sumofsquares(100, 10, zerosum=TRUE, maxt=Inf)} 
#' 
#' @param n integer: number to decompose as sum of squares
#' @param nmax integer: maximal number of squares in the sum
#' @param zerosum logical: should the solution sum up to one (default: \code{FALSE})
#' @param maxt numeric: maximal number of seconds the routine should run
#' @param size numeric: length of additional matrix size (default: \code{100000L}) 
#'
#' @md
#' @return matrix witj `nmax` column with \eqn{x_i}'s. \code{NA} means number has not been used``
#' @export
#'
#' @examples
#' sos <- sumofsquares(100, 6) # 23 solutions
#' head(sos)
#' table(rowSums(!is.na(sos))) 
#' # one solution with one or two x_i
#' # five solutions with four x_i
#' # six solutions with five x_i
#' # ten solutions with six x_i
#' rowSums(sos^2, na.rm=TRUE)  # all 100
#' sos <- sumofsquares(100, 6, zerosum=TRUE)
#' head(sos)
#' rowSums(sos^2, na.rm=TRUE)  # all 100
#' rowSums(sos, na.rm=TRUE)    # all 0 
sumofsquares <- function(n, nmax=10, zerosum=FALSE, maxt=30, size=100000L) {
  #browser()
  stopifnot(nmax>3)
  smax <- floor(sqrt(n))
  smin <- ceiling(sqrt(n)/nmax)
  sos  <- matrix(NA_integer_, nrow=size, ncol=nmax+1)
  nsos <- (smax-smin+1)
  sos[1:nsos,1] <- n-(smax:smin)^2
  sos[1:nsos,2] <- smax:smin
  i <- 1
  t <- as.numeric(Sys.time())
  while(i<=nrow(sos)) {
    if ((i%%1000)==0) {
#      cat(sprintf("%.0f/%.0f/%0.f\n", i, nsos, nsos-i))
      if ((as.numeric(Sys.time())-t)>maxt) break
    }
    ni <- sos[i,1]
    if (is.na(ni)) break
    if (ni>0) {
      nas  <- is.na(sos[i,])
      if (any(nas)) {
        col  <- min(which(nas))
        smax <- min(sos[i,col-1], floor(sqrt(ni)))
        smin <- 1 # floor(sqrt(ni/(nmax-col)))      
        if (smin==smax) {
          sosi      <- sos[i,]
          sosi[col] <- smin
          sosi[1]   <- n-sum(sosi[-1]^2, na.rm=TRUE)    
          if (nrow(sos)==nsos) sos <- rbind(sos, matrix(NA_integer_, nrow=size, ncol=nmax+1))
          nsos <- nsos+1
          sos[nsos,] <- sosi
        }
        if (smin<smax) {
          sosi       <- t(t(rep(1, smax-smin+1))) %*% sos[i,]          
          sosi[,col] <- sample(smax:smin)
          sosi[,1]   <- n-rowSums(sosi[,-1]^2, na.rm=TRUE)
          if (nrow(sos)<nsos+nrow(sosi)) sos <- rbind(sos, matrix(NA_integer_, nrow=size, ncol=nmax+1))      
          sos[nsos+(1:nrow(sosi)),] <- sosi
          nsos <- nsos+nrow(sosi)
        }
      }
    }
    i <- i+1
  }
  full <- i>nsos
  sos <- sos[1:nsos,]
  #browser()
  sos  <- sos[sos[,1]==0,-1]
  if (zerosum) {
    soszero  <- matrix(0, nrow=size, ncol=ncol(sos))
    nsoszero <- 0
    sos[is.na(sos)] <- 0
    bits <- sapply(0:((2^nmax)-1), intToBits)
    ones <- matrix(-1, nrow=nrow(bits), ncol=ncol(bits))
    ones[bits>0] <- +1
    ones <- ones[1:nmax,]
    #browser()
    msum <- sos%*%ones
    ind  <- which(msum==0, arr.ind = TRUE)
    if (length(ind)) {
      for (i in 1:nrow(ind)) {
        sosi <- sos[ind[i,1],]*ones[,ind[i,2]]
        #browser()
        if (nrow(soszero)==nsoszero) soszero <- rbind(soszero, matrix(0, nrow=size, ncol=ncol(sos)))
        nsoszero <- nsoszero+1
        soszero[nsoszero,] <- sosi
      }      
    }
    sos <- soszero
    sos <- sos[1:nsoszero,]
    sos[sos==0] <- NA_integer_
  }
  sos <- t(apply(sos, 1, sort, na.last=TRUE))
  sos <- sos[!duplicated(sos),]
  attr(sos, "full") <- full
  sos
}
