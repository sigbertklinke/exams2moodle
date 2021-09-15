#' as_ts
#'
#' Converts a \code{ts_data} object to a time series object (\code{ts}) in R.
#'
#' @param ts \code{ts_data} object
#'
#' @importFrom stats ts
#' @return \code{ts} object
#' @export
#'
#' @examples
#' # time series from linear trend
#' ts <- ts_data(12, trend.coeff= c(sample(0:10, 1), sample(1+(1:10)/20, 1)))
#' as_ts(ts)
as_ts <- function (ts) {
  stopifnot("ts_data" %in% class(ts))
  n <- length(ts$t)
  ts(data=ts$xt, start=c(1, ts$s[1]), end=c(n%/%max(ts$s), ts$s[n]), deltat=(ts$t[n]-ts$t[1])/n, frequency=max(ts$s))
}