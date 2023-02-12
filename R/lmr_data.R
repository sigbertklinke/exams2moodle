#' lmr_data
#'
#' Computes an `lm` object for a simple linear regression from a range of x and y values 
#' including intermediate values. If `r` is not given then zero correlation is used with `cor_data`
#' `digits` determines the rounding for the x and y values. If only one value is given then
#' it will be used for x and y. If no value is given then it will be determined from 
#' the x and y values by `3+ceiling(-log10(diff(range(.))))`.
#'
#' @param xr numeric: range of x values 
#' @param yr numeric: range of y values 
#' @param n numeric: number of observations to generate
#' @param r	numeric: desired correlation, uses `cor_data`
#' @param digits numeric(2): digits for rounding, for x `digits[1]` is used, for y `digits[2]` is used (default: `NULL`)
#' @param ... further parameters used in `cor_data` 
#'
#' @return An object of class `lm` with the additional components
#' * `x` the generated x values
#' * `y` the generated y values
#' * `sumx` \eqn{\sum_{i=1}^n x_i}
#' * `sumy` \eqn{\sum_{i=1}^n y_i}
#' * `sumx2` \eqn{\sum_{i=1}^n x_i^2}
#' * `sumy2` \eqn{\sum_{i=1}^n y_i^2}
#' * `sumxy` \eqn{\sum_{i=1}^n x_i y_i}
#' * `meanx` the mean of x: \eqn{1/n \sum_{i=1}^n x_i}
#' * `meany` the mean of y: \eqn{1/n \sum_{i=1}^n y_i}
#' * `varx` the variation of x: \eqn{\sum_{i=1}^n (x_i-\bar{x})^2}
#' * `vary` the variation of y: \eqn{\sum_{i=1}^n (y_i-\bar{y})^2}
#' * `varxy` the common variation of x and y:\eqn{\sum_{i=1}^n (x_i-\bar{x})(y_i-\bar{y})}
#' * `sxy` the covariance of x and y 
#' * `rxy` the correlation of x and y 
#' * `b0` the intercept of the linear regression
#' * `b1` the slope of the linear regression
#' * `r2` the coefficient of determination of the linear regression
#' @export
#'
#' @examples
#' # Engine displacement typically ranges from 500 to 2000 cm^3
#' # Fuel economy typically ranges from 2 to 8 liter/100 km
#' lmr <- lmr_data(c(500, 2000), c(2, 8), n=8)
#' str(lmr)
lmr_data <- function(xr, yr, n, r=0, digits=NULL, ...) {
  #browser()
  if (length(digits)==1) digits <- c(digits, digits)
  xr  <- xr[is.finite(xr)]
  yr  <- yr[is.finite(yr)]
  #
  dig <- if (is.null(digits)) 2+ceiling(-log10(diff(range(xr)))) else digits[1]
  repeat{
    x   <- signif(runif(n, min=min(xr), max=max(xr)), dig)  
    if (diff(range(x))>0) break
  }
  dig <- if (is.null(digits)) 2+ceiling(-log10(diff(range(yr)))) else digits[2]
  repeat{
    y   <- signif(runif(n, min=min(yr), max=max(yr)), dig)  
    if (diff(range(y))>0) break
  }
  xy  <- cor_data(x, y, r=r, ...)
  #
  ret       <- lm(xy[,2]~xy[,1])
  ret$x     <- xy[,1] 
  ret$y     <- xy[,2]
  ret$sumx  <- sum(ret$x)
  ret$sumy  <- sum(ret$y)
  ret$sumx2 <- sum(ret$x^2)
  ret$sumy2 <- sum(ret$y^2) 
  ret$sumxy <- sum(ret$x*ret$y)
  ret$meanx <- mean(ret$x)
  ret$meany <- mean(ret$y)
  ret$varx  <- sum((ret$x-ret$meanx)^2)
  ret$vary  <- sum((ret$y-ret$meany)^2)
  ret$varxy <- sum((ret$x-ret$meanx)*(ret$y-ret$meany))  
  ret$sxy   <- ret$varxy/n
  ret$rxy   <- ret$varxy/sqrt(ret$varx*ret$vary)
  ret$r2    <- ret$rxy^2
  ret$b1    <- ret$varxy/ret$varx
  ret$b0    <- ret$meany-ret$b1*ret$meanx
  ret
}