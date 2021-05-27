#' lcmval
#'
#' Computes the least common multiple for a numeric vector `x`.
#'
#' @param x integer: numbers to find the least common multiple 
#'
#' @md
#' @return the least common multiple 
#' @export
#'
#' @examples
#' lcmval(c(144, 160))      # = 1440
#' lcmval(c(144, 160, 175)) # = 50.400
lcmval <- function(x) {
  lcmab <- function(a, b) {
    m <- a*b
    while(b>0) {
      h <- a%%b
      a <- b
      b <- h
    }
    m/a
  }
  #
  stopifnot(length(x)>1)
  x   <- as.integer(x)
  ret <- lcmab(x[1], x[2]) 
  if (length(x)>2) {
    for (xi in x[-(1:2)]) ret <- lcmab(ret, xi)
  }
  ret
}