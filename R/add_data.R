#' @title add_data
#' @description Adds data values to a given data vector \code{x}. 
#' 
#' @details Based on the data \code{x} or the \code{range(box)} a box is computed. 
#' The length aof the box gives the multiplier for the \code{range}. 
#' Then a left and right interval from which the additional values are drawn uniformly is computed:
#' \eqn{[left box value-range[2]*box length; left box value-range[1]*box length]} (left interval) and
#' \eqn{[right box value+range[1]*box length; right box value+range[2]*box length]} (right interval).
#' 
#' For \code{box} can be used also \code{"boxplot"} and \code{quantile(x, c(0.25, 0.75), na.rm=TRUE)} is used or 
#' \code{"range"} and \code{range(x, na.rm=TRUE)} is used.
#' 
#' \code{n} can be a single number which will add \code{n} data values at the right side of \code{x}. 
#' If \code{n} is a vector of length two then \code{n[1]} data values will be added at the left side of \code{x} and
#' \code{n[2]} data values will be added at the right side of \code{x}.
#' 
#' @param x numeric: data vector
#' @param box character or numeric: basic box used
#' @param n numeric: number of  data values on the left, the right or both sides of \code{x} (default: \code{c(0,1)})
#' @param range numeric: determines the range where the additional data values will be drawn from (default: \code{c(0,1)})
#'
#' @return a data vectors with new values
#' @importFrom stats quantile runif
#' @export
#'
#' @examples
#' x <- rnorm(8)
#' # add one value to the right
#' add_data(x, "box", range=1.5)
#' add_data(x, "range", range=0.1)
#' add_data(x, "box", range=c(1.5, 3))
#' # add two values to the right
#' add_data(x, "range", n=2, range=0.1)
#' # add two values to the left and three to the right
#' add_data(x, "range", n=c(2,3), range=0.1)
add_data <- function(x, box, n=c(0,1), range=c(0,1)) {
  if (missing(box)) stop("parameter 'box' is required")  
  if (length(n)==1) n <- c(0,n)
  if (is.character(box)) {
    boxi <- pmatch(box, c("boxplot", "range")) 
    stopifnot(is.finite(boxi))
    if (boxi==1) box <- quantile(x, c(0.25, 0.75), na.rm=TRUE)
    if (boxi==2) box <- range(x, na.rm=TRUE)
  }
  box   <- range(box)
  blen  <- diff(box)
  rangeval <- range(range)
  if (length(n)>1) {
    xp <- c(runif(n[1], min=box[1]-blen*rangeval[2], max=box[1]-blen*rangeval[1]),
           runif(n[2], min=box[2]+blen*rangeval[1], max=box[2]+blen*rangeval[2]))
  } else {
    xp <- runif(n[1], min=box[1]-blen*rangeval[2], max=box[1]-blen*rangeval[1])
  }
  c(x, xp)
}