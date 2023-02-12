#' meanint_data
#'
#' @param x numeric: number of observations or x values
#' @param r numeric(2): range where the x values allowed (default: `range(x)`)
#' @param ... further parameters given to `mean`
#'
#' @return a set of integer observations with an integer mean
#' @export
#'
#' @examples
#' x <- meanint_data(10, c(1, 10))
#' mean(x)
meanint_data <- function(x, r=range(x), ...) {
  frac <- function(x) { abs(x-round(x)) }
  #
  if (length(x)==1) x <- runif(x, r[1], r[2])
  x  <- round(x)
  fx <- mean(x, ...)
  while(!has_digits(fx,0)) {
    xi <- x
    i  <- sample(length(x), 1)
    xi[i] <- x[i] + if (runif(1)<0.5) -1 else +1
    if ((xi[i]<r[1]) || (xi[i]>r[2])) xi[i] <- x[i] 
    fxi <- mean(xi, ...)
    if (frac(fxi)<frac(fx)) {
      x <- xi
      fx <- fxi
    }
  }
  x
}