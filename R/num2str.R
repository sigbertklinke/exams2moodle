#' num2str
#'
#' Converts a set of numeric variables to a list as string representation, either as decimal or fractional numebr.
#'
#' @param ... numeric variables
#' @param denom integer: denominator for fractional number
#'
#' @return a list
#' @export
#'
#' @examples
#' x <- 1
#' l <- num2str(x)         # returns in l$x the string representation
#' l <- num2str(x, y=x+1)  # returns in l$x and l$y the string representations
num2str <- function(..., denom=-1) {
  call <- match.call()
  stopifnot(length(call)>1)
  args  <- list(...)
  nargs <- names(args)
  if (is.null(nargs)) nargs <- rep('', length(args))
  oo <- options("scipen"=getOption('exams.scipen', 25))
  on.exit(options(oo))
  for (i in 2:length(call)) {
    if (nargs[i-1]=='') {
      stopifnot(class(call[[i]])=="name")
      nargs[i-1] <- as.character(call[[i]])
    } 
#    args[[i-1]] <- as.character(args[[i-1]])
    args[[i-1]] <- fcvt(args[[i-1]], denom=denom)
  }
  names(args) <- nargs
  args
}

#' fcvt
#'
#' Converts a number to a string containing either a floating point or a fractional number. 
#' Note that a repeating or recurring decimal (a number whose decimal representation becomes periodic)
#' can be also expressed by rational number, e.g. \eqn{\frac{1}{3}=0.333333333...=0.\overline{3}}.
#' It is the workhorse used in `num2str`.
#'
#' * If `denom` is negative then always decimal point numbers are used (default).
#' * If `denom` is zero then a mix of decimal point and fractional numbers are used (whatever is shorter).
#' * If `denom` is one then fractional numbers are used except for integers.
#' * If `denom` is larger one then the denominator is set to `denom` if possible.
#'
#' @param x numeric: numbers to convert
#' @param nsmall integer: number of significant digits for the mantissa/significand (default: `16`)
#' @param plus logical: should for positive numbers a plus sign used (default: `FALSE`)
#' @param denom integer: denominator for fractional number
#'
#' @return character
#' @export
#'
#' @examples
#' x1 <- c(NA, NaN, -Inf, Inf, 0, pi*10^(-20:20))
#' fcvt(x1)
#' x2 <- c(-0.36, 3.6, -30.6, 0.36)
#' fcvt(x2)
#' x3 <- c((0:16)/8, 1/3)
#' fcvt(x3)           # as floating point number, equals denom=-1
#' fcvt(x3, denom=0)  # as floating point or fractional number
#' fcvt(x3, denom=1)  # as fractional number except for integers
#' fcvt(x3, denom=8)  # as fractional number with denominator denom if possible
fcvt <- function(x, nsmall=15, plus=FALSE, denom=-1) {
  if (length(x)==0) return(character(0))
  denom <- as.integer(denom)
  indx  <- is.finite(x)
  dsign <- sign(x)
  dabsx <- abs(x)
  # Step 1: floating point number
  if(denom<=0) {
    zero  <- "000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    dexp  <- ifelse(x==0, 0, 1+floor(log10(dabsx)))
    dfrac <- substring(format(dabsx/10^dexp, scientific=FALSE, nsmall=nsmall), 3)
    ind   <- which(indx & (dexp==0))
    if (length(ind)) dfrac[ind] <- paste0("0.", dfrac[ind])  
    ind   <- which(indx & (dexp<0))
    if (length(ind)) dfrac[ind] <- paste0("0.", substring(zero, 1, -dexp[ind]), dfrac[ind])
    # 
    ndfrac     <- nchar(dfrac)
    ind        <- which(indx & (dexp>0) & (ndfrac>dexp))  
    if (length(ind)) dfrac[ind] <- paste0(substring(dfrac[ind], 1, dexp[ind]), '.',  substring(dfrac[ind], dexp[ind]+1))    
    ind        <- which(indx & (dexp>0) & (ndfrac<dexp))
    if (length(ind)) dfrac[ind] <- paste0(dfrac[ind], substring(zero, 1, dexp[ind]-ndfrac[ind]))
    # delete trailing zeroes
    ind        <- which(grepl(".", dfrac, fixed=TRUE))
    if (length(ind)) {
      dfrac[ind] <- sub("0+$", "", dfrac[ind])
      dfrac[ind] <- sub("\\.$", "", dfrac[ind])  
    }
  }
  # Step 2: fractional numbers
  if (denom>=0) {
    dexp   <- ifelse(x==0, 0, 1+floor(log10(dabsx)))
    dfrac2 <- attr(10^dexp*fractions(dabsx*10^(-dexp)), "fracs")
    if (denom>1) {
      dfracs <- strsplit(dfrac2, '/', fixed=TRUE)
      for (i in seq_along(dfracs)) {
        if (length(dfracs[[i]])==1) {
          d1 <- as.character(denom*as.double(dfracs[[i]][1]))
          d2 <- as.character(denom)
        } else {
          d1 <- dfracs[[i]][1]
          d2 <- dfracs[[i]][2]
          if (abs(denom%%as.double(d2))<1e-6) {
            f  <- denom/as.double(d2)
            d1 <- f*as.double(d1)
            d2 <- as.character(denom)
          }
        }
        dfrac2[i] <- paste0(d1, '/', d2)
      }
    }
  }
  if (denom==0) dfrac <- ifelse(nchar(dfrac)>nchar(dfrac2), dfrac2, dfrac)
  if (denom>0) dfrac <- dfrac2
  # non-finite values and sign
  ind        <- !is.finite(x)  
  if (length(ind)) dfrac[ind] <- as.character(abs(x[ind]))
  dsign[is.na(dsign)] <- 0
  dsign    <- ifelse(dsign<0, "-", if(plus) "+" else "")
  paste0(dsign,dfrac)
}


