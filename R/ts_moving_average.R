#' ts_moving_average
#'
#' Computes the moving average for a \code{ts_data} object.
#'
#' @param ts \code{ts_data} object
#' @param order integer: order of the moving average 
#'
#' @md
#' @return returns an extended \code{ts_data} object with list elements:
#' * `filter` the filter used
#' * `moving.average` the computed moving average
#' @importFrom stats filter
#' @export
#'
#' @examples
#' # trend from a quadratic model
#' ts <- ts_data(12, trend.coeff=c(sample(0:10, 1), sample(1+(1:10)/20, 1), 0.5))
#' ts_moving_average(ts, 3)
ts_moving_average <- function(ts, order) {
  stopifnot(as.integer(order)>0)
  ones   <- rep(1, 2*(order-1)%/%2+1)
  ts$filter <- (if (order%%2) ones else c(0.5, ones, 0.5))/order
  ts$moving.average <- as.numeric(filter(ts$xt, ts$filter))
  ts
}
