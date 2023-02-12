#' histbreaks
#'
#' Select randomly `size` breakpoints from `breaks`. If `outer` is `TRUE` then 
#' the first and last element of `breaks` is always included into the returned break points.
#' If `size` is a vector then first the number of breakpoints is sampled from `size`.
#'
#' @param breaks numeric: a vector of possible break points
#' @param size integer: number of break points
#' @param outer logical: should be the first and last element of breaks included (default: \code{TRUE}) 
#' @param ... further parameters given if sampling of `size` is necessary, see [base::sample]
#'
#' @return a vector of breakpoints
#' @export
#'
#' @examples
#' # includes always 100 and 200
#' histbreaks(seq(100, 200, by=10), 4)
#' # includes always 100 and 200 and chooses randomly between 3 to 5 bareak points  
#' histbreaks(seq(100, 200, by=10), 3:5)           
#' # may not include include 100 and 200
#' histbreaks(seq(100, 200, by=10), 4, outer=FALSE) 
histbreaks <- function(breaks, size, outer=TRUE, ...) {
  if (length(size)>1) {
    args      <- list(...)
    args$x    <- size
    args$size <- 1
    size      <- do.call(sample, args)
  }
  stopifnot(size>2)
  stopifnot(length(breaks)>=size)
  if (outer) b <- c(breaks[1], breaks[length(breaks)]) else b <-vector(mode(breaks), 0)
  bs   <- setdiff(breaks, b)
  size <- size-length(b)
  if (size==0) return(sort(bs))
  b <- c(b, if (length(bs)==1) bs else sample(bs, size))
  sort(b)  
}