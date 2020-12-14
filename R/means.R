#' means
#'
#' Computes mean's of \code{x}. The list returned has an attribute \code{"mindiff"} which contains 
#' the smallest distance between two mean values before rounding.
#' If \code{winsor} and/or \code{trim} set to \code{NA} then the trimmed and/or winsorized mean are not computed. 
#' 
#' Currently are implemented
#' \describe{
#' \item{\code{mean}}{arithmetic mean}
#' \item{\code{median}}{median}
#' \item{\code{harmonic}}{harmonic mean}
#' \item{\code{geometric}}{geometric mean}
#' \item{\code{mode}}{(first) mode}
#' \item{\code{trim}}{trimmed mean}
#' \item{\code{winsor}}{winsorized mean}
#' }
#'
#' @param x numeric: data values
#' @param digits numeric:	integer indicating the number of decimal places for rounding (negative values are allowed)
#' @param na.rm logical: should \code{NA}s removed before
#' @param trim numeric: the fraction (0 to 0.5) of observations to be trimmed from each end of \code{x}
#' @param winsor numeric: the fraction (0 to 0.5) of observations to be moved from each end of \code{x}
#'
#' @importFrom stats median na.omit
#' @importFrom psych winsor.mean harmonic.mean
#' @return a list with mean values
#' @export
#'
#' @examples
#' x <- c(runif(9), 3)
#' means(x, 2)
means <- function (x, digits, na.rm=TRUE, trim=0.2, winsor=0.2) {
  if (missing(digits)) stop("parameter 'digits' is required")
  if (na.rm) x <- na.omit(x)
  tx  <- table(x)
  ret <- list(mean=mean(x),
              median=median(x),
              harmonic=harmonic.mean(x), 
              geometric=exp(mean(log(x))),
              mode=as.numeric(names(tx)[which.max(tx)]))
  if (!is.na(trim))   ret$trim <- mean(x, trim=trim)
  if (!is.na(winsor)) ret$winsor <- winsor.mean(x, trim=winsor)
  attr(ret, "mindiff") <- min(diff(sort(unlist(ret))))
  for (i in 1:length(ret)) ret[[i]] <- round(ret[[i]], digits)
  ret
}