#' ts_data
#'
#' Creates an univariate time series based on linear or exponential trend, 
#' an additive or multiplicative seasonal adjustment and with white noise.
#'
#' @param end integer: length of time series
#' @param trend logical: if \code{TRUE} a linear trend otherwise a exponential trend (default: \code{TRUE})
#' @param trend.coeff numeric: coefficients for a linear model (default: \code{c(1,1)})
#' @param season logical: if \code{TRUE} an additive seasonal adjustment is done otherwise a multiplicative seasonal adjustment  (default: \code{TRUE})
#' @param season.coeff numeric: coefficients for the adjustment (default: \code{NULL}). If \code{NULL} then no seasonal adjustment is made.
#' @param error logical: if \code{TRUE} an additive error term is used otherwise a multiplicative error term (default: \code{TRUE}). 
#' @param error.coeff numeric: standard deviation(s) for white noise error (default: \code{NULL}). If \code{NULL} then no error is added.
#' @param digits integer: number of digits to round time series (default: \code{NA}). If \code{NA} then no rounding is done.
#'
#' @md
#' @return ts_data object with list elements
#' * `t` the time points
#' * `s` the season for the time poins
#' * `xt` the time series values
#' @importFrom stats rnorm
#' @export
#'
#' @examples
#' # time series from linear trend
#' ts <- ts_data(12, trend.coeff= c(sample(0:10, 1), sample(1+(1:10)/20, 1)))
#' ts
#' # time series from exponential trend
#' ts <- ts_data(12, trend.coeff= c(sample(0:10, 1), sample(1+(1:10)/20, 1)), trend=FALSE)
#' ts   
#' # time series from linear trend and additive seasonal adjustment (quartely data)
#' ts <- ts_data(12, trend.coeff=c(sample(0:10, 1), sample(1+(1:10)/20, 1)),
#'                   season.coeff=sample((-20:20)/20, 4))
#' ts   
#' # time series from linear trend and additive seasonal adjustment (half-yearly data)
#' ts <- ts_data(12, trend.coeff=c(sample(0:10, 1), sample(1+(1:10)/20, 1)),
#'                   season.coeff=sample((-20:20)/20, 2))
#' ts   
#' # time series from linear trend and mutliplicative seasonal adjustment (quartely data)
#' ts <- ts_data(12, trend.coeff=c(sample(0:10, 1), sample(1+(1:10)/20, 1)),
#'                   season.coeff=sample((-20:20)/20, 4), season=FALSE)
#' ts   
ts_data <- function(end, 
                    trend=TRUE,  trend.coeff=c(1,1),
                    season=TRUE, season.coeff=NULL, 
                    error=TRUE,  error.coeff=NULL,
                    digits=NA) {
  t <- seq(from=1, to=end, by=1)
  stopifnot(length(t)>2)
  if (trend) {
    xt <- outer(t, 0:(length(trend.coeff)-1), "^")%*%trend.coeff
  } else {
    xt <- exp(outer(t, 0:(length(trend.coeff)-1), "^")%*%log(trend.coeff))  
  }
  if (is.null(season.coeff)) {
    s <- rep(1, length(t))
  } else {
    s <- rep(1:length(season.coeff), length.out=length(t))
    if (season) {
      xt <- xt+rep(season.coeff, length.out=length(t))
    } else {
      xt <- xt*rep(season.coeff, length.out=length(t))
    }
  } 
  if (!is.null(error.coeff)) {
    if (error) {
      xt <- xt+rnorm(length(t), 0, error.coeff)
    } else {
      xt <- xt*rnorm(length(t), 1, error.coeff)
    }
  }
  #
  if (!is.na(digits)) xt <- round(xt, digits)
  structure(list(t=t, s=s, xt=as.numeric(xt)), class="ts_data")
}